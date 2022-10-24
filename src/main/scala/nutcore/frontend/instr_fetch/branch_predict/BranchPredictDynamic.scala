package nutcore.frontend.instr_fetch.branch_predict

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import nutcore._

import utils._
import top.Settings

// nextline predictor generates NPC from current NPC in 1 cycle
class BranchPredictDynamic extends NutCoreModule {
  val io = IO(new Bundle {
    val in = new Bundle { val pc = Flipped(Valid((UInt(VAddrBits.W)))) }
    val out = new RedirectIO 
    val flush = Input(Bool())
    val brIdx = Output(Vec(4, Bool()))
    // val target = Output(Vec(4, UInt(VAddrBits.W)))
    // val instValid = Output(UInt(4.W)) // now instValid is generated in IFU
    val crosslineJump = Output(Bool())
  })

  val flush = BoolStopWatch(io.flush, io.in.pc.valid, startHighPriority = true)

  // BTB
  val NRbtb = 512
  val btbAddr = new TableAddr(log2Up(NRbtb >> 2))
  def btbEntry() = new Bundle {
    val tag = UInt(btbAddr.tagBits.W)
    val _type = UInt(2.W)
    val target = UInt(VAddrBits.W)
    val crosslineJump = Bool()
    val valid = Bool()
  }

  val btb = List.fill(4)(Module(new SRAMTemplate(btbEntry(), set = NRbtb >> 2, shouldReset = true, holdRead = true, singlePort = true)))
  // flush BTB when executing fence.i
  val flushBTB = WireInit(false.B)
  val flushTLB = WireInit(false.B)
  BoringUtils.addSink(flushBTB, "MOUFlushICache")
  BoringUtils.addSink(flushTLB, "MOUFlushTLB")
  (0 to 3).map(i => (btb(i).reset := reset.asBool || (flushBTB || flushTLB)))

  Debug(reset.asBool || (flushBTB || flushTLB), "[BPU-RESET] bpu-reset flushBTB:%d flushTLB:%d\n", flushBTB, flushTLB)

  (0 to 3).map(i => (btb(i).io.r.req.valid := io.in.pc.valid))
  (0 to 3).map(i => (btb(i).io.r.req.bits.setIdx := btbAddr.getIdx(io.in.pc.bits)))


  val btbRead = Wire(Vec(4, btbEntry()))
  (0 to 3).map(i => (btbRead(i) := btb(i).io.r.resp.data(0)))
  // since there is one cycle latency to read SyncReadMem,
  // we should latch the input pc for one cycle
  val pcLatch = RegEnable(io.in.pc.bits, io.in.pc.valid)
  val btbHit = Wire(Vec(4, Bool()))
  (0 to 3).map(i => btbHit(i) := btbRead(i).valid && btbRead(i).tag === btbAddr.getTag(pcLatch) && !flush && RegNext(btb(i).io.r.req.fire(), init = false.B))
  // btbHit will ignore pc(2,0). pc(2,0) is used to build brIdx
  val crosslineJump = btbRead(3).crosslineJump && btbHit(3) && !io.brIdx(0) && !io.brIdx(1) && !io.brIdx(2)
  io.crosslineJump := crosslineJump
  // val crosslineJumpLatch = RegNext(crosslineJump)
  // val crosslineJumpTarget = RegEnable(btbRead.target, crosslineJump)
  
  // PHT
  val pht = List.fill(4)(Mem(NRbtb >> 2, UInt(2.W)))
  val phtTaken = Wire(Vec(4, Bool()))
  (0 to 3).map(i => (phtTaken(i) := RegEnable(pht(i).read(btbAddr.getIdx(io.in.pc.bits))(1), io.in.pc.valid)))

  // RAS
  val NRras = 16
  val ras = Mem(NRras, UInt(VAddrBits.W))
  val sp = Counter(NRras)
  val rasTarget = RegEnable(ras.read(sp.value), io.in.pc.valid)

  // update
  val req = WireInit(0.U.asTypeOf(new BPUUpdateReq))
  val btbWrite = WireInit(0.U.asTypeOf(btbEntry()))
  BoringUtils.addSink(req, "bpuUpdateReq")

  btbWrite.tag := btbAddr.getTag(req.pc)
  btbWrite.target := req.actualTarget
  btbWrite._type := req.btbType
  btbWrite.crosslineJump := req.pc(2,1)==="h3".U && !req.isRVC // ((pc_offset % 8) == 6) && inst is 32bit in length
  btbWrite.valid := true.B 
  // NOTE: We only update BTB at a miss prediction.
  // If a miss prediction is found, the pipeline will be flushed
  // in the next cycle. Therefore it is safe to use single-port
  // SRAM to implement BTB, since write requests have higher priority
  // than read request. Again, since the pipeline will be flushed
  // in the next cycle, the read request will be useless.
  (0 to 3).map(i => btb(i).io.w.req.valid := req.isMissPredict && req.valid && i.U === req.pc(2,1))
  (0 to 3).map(i => btb(i).io.w.req.bits.setIdx := btbAddr.getIdx(req.pc))
  (0 to 3).map(i => btb(i).io.w.req.bits.data := btbWrite)

  val getpht = LookupTree(req.pc(2,1), List.tabulate(4)(i => (i.U -> pht(i).read(btbAddr.getIdx(req.pc)))))
  val cnt = RegNext(getpht)
  val reqLatch = RegNext(req)
  when (reqLatch.valid && ALUOpType.isBranch(reqLatch.fuOpType)) {
    val taken = reqLatch.actualTaken
    val newCnt = Mux(taken, cnt + 1.U, cnt - 1.U)
    val wen = (taken && (cnt =/= "b11".U)) || (!taken && (cnt =/= "b00".U))
    when (wen) {
      (0 to 3).map(i => when(i.U === reqLatch.pc(2,1)){pht(i).write(btbAddr.getIdx(reqLatch.pc), newCnt)})
    }
  }
  when (req.valid) {
    when (req.fuOpType === ALUOpType.call)  {
      ras.write(sp.value + 1.U, Mux(req.isRVC, req.pc + 2.U, req.pc + 4.U))
      sp.value := sp.value + 1.U
    }
    .elsewhen (req.fuOpType === ALUOpType.ret) {
      when(sp.value === 0.U) {
        // RAS empty, do nothing
      }
      sp.value := Mux(sp.value===0.U, 0.U, sp.value - 1.U)
    }
  }

  def genInstValid(pc: UInt) = LookupTree(pc(2,1), List(
    "b00".U -> "b1111".U,
    "b01".U -> "b1110".U,
    "b10".U -> "b1100".U,
    "b11".U -> "b1000".U
  ))

  val pcLatchValid = genInstValid(pcLatch)

  val target = Wire(Vec(4, UInt(VAddrBits.W)))
  (0 to 3).map(i => target(i) := Mux(btbRead(i)._type === BTBtype.R, rasTarget, btbRead(i).target))
  (0 to 3).map(i => io.brIdx(i) := btbHit(i) && pcLatchValid(i).asBool && Mux(btbRead(i)._type === BTBtype.B, phtTaken(i), true.B) && btbRead(i).valid)
  io.out.target := PriorityMux(io.brIdx, target)
  io.out.valid := io.brIdx.asUInt.orR
  io.out.rtype := 0.U
  Debug(io.out.valid, "[BPU] pc %x io.brIdx.asUInt %b phtTaken %x %x %x %x valid %x %x %x %x\n", pcLatch, io.brIdx.asUInt, phtTaken(0), phtTaken(1), phtTaken(2), phtTaken(3), btbRead(0).valid, btbRead(1).valid, btbRead(2).valid, btbRead(3).valid)

  // io.out.valid := btbHit && Mux(btbRead._type === BTBtype.B, phtTaken, true.B) && !crosslineJump || crosslineJumpLatch && !flush && !crosslineJump
  // Note: 
  // btbHit && Mux(btbRead._type === BTBtype.B, phtTaken, true.B) && !crosslineJump : normal branch predict
  // crosslineJumpLatch && !flush && !crosslineJump : cross line branch predict, bpu will require imem to fetch the next 16bit of current inst in next instline
  // `&& !crosslineJump` is used to make sure this logic will run correctly when imem stalls (pcUpdate === false)
  // by using `instline`, we mean a 64 bit instfetch result from imem
  // ROCKET uses a 32 bit instline, and its IDU logic is more simple than this implentation.
}
