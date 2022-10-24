package nutcore.frontend.instr_fetch.branch_predict

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import nutcore._

import utils._
import top.Settings

class BranchPredictSequential extends NutCoreModule {
  val io = IO(new Bundle {
    val in = new Bundle { val pc = Flipped(Valid((UInt(VAddrBits.W)))) }
    val out = new RedirectIO
    val flush = Input(Bool())
    val brIdx = Output(UInt(3.W))
    val crosslineJump = Output(Bool())
  })

  val flush = BoolStopWatch(io.flush, io.in.pc.valid, startHighPriority = true)

  // BTB
  val NRbtb = 512
  val btbAddr = new TableAddr(log2Up(NRbtb))
  def btbEntry() = new Bundle {
    val tag = UInt(btbAddr.tagBits.W)
    val _type = UInt(2.W)
    val target = UInt(VAddrBits.W)
    val brIdx = UInt(3.W)
    val valid = Bool()
  }

  val btb = Module(new SRAMTemplate(btbEntry(), set = NRbtb, shouldReset = true, holdRead = true, singlePort = true))
  // flush BTB when executing fence.i
  val flushBTB = WireInit(false.B)
  val flushTLB = WireInit(false.B)
  BoringUtils.addSink(flushBTB, "MOUFlushICache")
  BoringUtils.addSink(flushTLB, "MOUFlushTLB")
  btb.reset := reset.asBool || (flushBTB || flushTLB)
  Debug(reset.asBool || (flushBTB || flushTLB), "[BPU-RESET] bpu-reset flushBTB:%d flushTLB:%d\n", flushBTB, flushTLB)

  btb.io.r.req.valid := io.in.pc.valid
  btb.io.r.req.bits.setIdx := btbAddr.getIdx(io.in.pc.bits)


  val btbRead = Wire(btbEntry())
  btbRead := btb.io.r.resp.data(0)
  // since there is one cycle latency to read SyncReadMem,
  // we should latch the input pc for one cycle
  val pcLatch = RegEnable(io.in.pc.bits, io.in.pc.valid)
  val btbHit = btbRead.valid && btbRead.tag === btbAddr.getTag(pcLatch) && !flush && RegNext(btb.io.r.req.fire(), init = false.B) && !(pcLatch(1) && btbRead.brIdx(0))
  // btbHit will ignore pc(1,0). pc(1,0) is used to build brIdx
  // !(pcLatch(1) && btbRead.brIdx(0)) is used to deal with the following case:
  // -------------------------------------------------
  // 0 jump rvc         // marked as "take branch" in BTB
  // 2 xxx  rvc <-- pc  // misrecognize this instr as "btb hit" with target of previous jump instr
  // -------------------------------------------------
  val crosslineJump = btbRead.brIdx(2) && btbHit
  io.crosslineJump := crosslineJump
  // val crosslineJumpLatch = RegNext(crosslineJump)
  // val crosslineJumpTarget = RegEnable(btbRead.target, crosslineJump)
  Debug(btbHit, "[BTBHT1] %d pc=%x tag=%x,%x index=%x bridx=%x tgt=%x,%x flush %x type:%x\n", GTimer(), pcLatch, btbRead.tag, btbAddr.getTag(pcLatch), btbAddr.getIdx(pcLatch), btbRead.brIdx, btbRead.target, io.out.target, flush,btbRead._type)
  Debug(btbHit, "[BTBHT2] btbRead.brIdx %x mask %x\n", btbRead.brIdx, Cat(crosslineJump, Fill(2, io.out.valid)))
  // Debug(btbHit, "[BTBHT5] btbReqValid:%d btbReqSetIdx:%x\n",btb.io.r.req.valid, btb.io.r.req.bits.setId)
  
  // PHT
  val pht = Mem(NRbtb, UInt(2.W))
  val phtTaken = RegEnable(pht.read(btbAddr.getIdx(io.in.pc.bits))(1), io.in.pc.valid)

  // RAS

  val NRras = 16
  val ras = Mem(NRras, UInt(VAddrBits.W))
  // val raBrIdxs = Mem(NRras, UInt(2.W))
  val sp = Counter(NRras)
  val rasTarget = RegEnable(ras.read(sp.value), io.in.pc.valid)
  // val rasBrIdx = RegEnable(raBrIdxs.read(sp.value), io.in.pc.valid)

  // update
  val req = WireInit(0.U.asTypeOf(new BPUUpdateReq))
  val btbWrite = WireInit(0.U.asTypeOf(btbEntry()))
  BoringUtils.addSink(req, "bpuUpdateReq")

  Debug(req.valid, "[BTBUP] pc=%x tag=%x index=%x bridx=%x tgt=%x type=%x\n", req.pc, btbAddr.getTag(req.pc), btbAddr.getIdx(req.pc), Cat(req.pc(1), ~req.pc(1)), req.actualTarget, req.btbType)

    //val fflag = req.btbType===3.U && btb.io.w.req.valid && btb.io.w.req.bits.setIdx==="hc9".U
    //when(fflag && GTimer()>2888000.U) {
    //  Debug("%d\n", GTimer())
    //  Debug("[BTBHT6] btbWrite.type is BTBtype.R/RET!!! Inpc:%x btbWrite.brIdx:%x setIdx:%x\n", io.in.pc.bits, btbWrite.brIdx, btb.io.w.req.bits.setIdx)
    //  Debug("[BTBHT6] tag:%x target:%x _type:%x bridx:%x\n", btbWrite.tag,btbWrite.target,btbWrite._type,btbWrite.brIdx)
    //  Debug(p"[BTBHT6] req:${req} \n")
    //} 
    //Debug("[BTBHT5] tag: target:%x type:%d brIdx:%d\n", req.actualTarget, req.btbType, Cat(req.pc(2,0)==="h6".U && !req.isRVC, req.pc(1), ~req.pc(1)))

  btbWrite.tag := btbAddr.getTag(req.pc)
  btbWrite.target := req.actualTarget
  btbWrite._type := req.btbType
  btbWrite.brIdx := Cat(req.pc(2,0)==="h6".U && !req.isRVC, req.pc(1), ~req.pc(1))
  btbWrite.valid := true.B 
  // NOTE: We only update BTB at a miss prediction.
  // If a miss prediction is found, the pipeline will be flushed
  // in the next cycle. Therefore it is safe to use single-port
  // SRAM to implement BTB, since write requests have higher priority
  // than read request. Again, since the pipeline will be flushed
  // in the next cycle, the read request will be useless.
  btb.io.w.req.valid := req.isMissPredict && req.valid
  btb.io.w.req.bits.setIdx := btbAddr.getIdx(req.pc)
  btb.io.w.req.bits.data := btbWrite

  //Debug(true) {
    //when (btb.io.w.req.valid && btbWrite.tag === btbAddr.getTag("hffffffff803541a4".U)) {
    //  Debug("[BTBWrite] %d setIdx:%x req.valid:%d pc:%x target:%x bridx:%x\n", GTimer(), btbAddr.getIdx(req.pc), req.valid, req.pc, req.actualTarget, btbWrite.brIdx)
    //}
  //}

  //when (GTimer() > 77437484.U && btb.io.w.req.valid) {
  //  Debug("[BTBWrite-ALL] %d setIdx:%x req.valid:%d pc:%x target:%x bridx:%x\n", GTimer(), btbAddr.getIdx(req.pc), req.valid, req.pc, req.actualTarget, btbWrite.brIdx)
  //}

  val cnt = RegNext(pht.read(btbAddr.getIdx(req.pc)))
  val reqLatch = RegNext(req)
  when (reqLatch.valid && ALUOpType.isBranch(reqLatch.fuOpType)) {
    val taken = reqLatch.actualTaken
    val newCnt = Mux(taken, cnt + 1.U, cnt - 1.U)
    val wen = (taken && (cnt =/= "b11".U)) || (!taken && (cnt =/= "b00".U))
    when (wen) {
      pht.write(btbAddr.getIdx(reqLatch.pc), newCnt)
      //Debug(){
        //Debug("BPUPDATE: pc %x cnt %x\n", reqLatch.pc, newCnt)
      //}
    }
  }
  when (req.valid) {
    when (req.fuOpType === ALUOpType.call)  {
      ras.write(sp.value + 1.U, Mux(req.isRVC, req.pc + 2.U, req.pc + 4.U))
      // raBrIdxs.write(sp.value + 1.U, Mux(req.pc(1), 2.U, 1.U))
      sp.value := sp.value + 1.U
    }
    .elsewhen (req.fuOpType === ALUOpType.ret) {
      when(sp.value === 0.U) {
        //Debug("ATTTTT: sp.value is 0.U\n") //TODO: sp.value may equal to 0.U
      }
      sp.value := Mux(sp.value===0.U, 0.U, sp.value - 1.U) //TODO: sp.value may less than 0.U
    }
  }

  io.out.target := Mux(btbRead._type === BTBtype.R, rasTarget, btbRead.target)
  // io.out.target := Mux(crosslineJumpLatch && !flush, crosslineJumpTarget, Mux(btbRead._type === BTBtype.R, rasTarget, btbRead.target))
  // io.out.brIdx  := btbRead.brIdx & Fill(3, io.out.valid)
  io.brIdx  := btbRead.brIdx & Cat(true.B, crosslineJump, Fill(2, io.out.valid))
  io.out.valid := btbHit && Mux(btbRead._type === BTBtype.B, phtTaken, true.B && rasTarget=/=0.U) //TODO: add rasTarget=/=0.U, need fix
  io.out.rtype := 0.U
  // io.out.valid := btbHit && Mux(btbRead._type === BTBtype.B, phtTaken, true.B) && !crosslineJump || crosslineJumpLatch && !flush && !crosslineJump
  // Note: 
  // btbHit && Mux(btbRead._type === BTBtype.B, phtTaken, true.B) && !crosslineJump : normal branch predict
  // crosslineJumpLatch && !flush && !crosslineJump : cross line branch predict, bpu will require imem to fetch the next 16bit of current inst in next instline
  // `&& !crosslineJump` is used to make sure this logic will run correctly when imem stalls (pcUpdate === false)
  // by using `instline`, we mean a 64 bit instfetch result from imem
  // ROCKET uses a 32 bit instline, and its IDU logic is more simple than this implentation.
}

