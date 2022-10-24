package nutcore.frontend.instr_fetch.branch_predict

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import nutcore._

import utils._
import top.Settings

class BranchPredictEmbedded extends NutCoreModule {
  val io = IO(new Bundle {
    val in = new Bundle { val pc = Flipped(Valid((UInt(32.W)))) }
    val out = new RedirectIO
    val flush = Input(Bool())
  })

  val flush = BoolStopWatch(io.flush, io.in.pc.valid, startHighPriority = true)

  // BTB
  val NRbtb = 512
  val btbAddr = new TableAddr(log2Up(NRbtb))
  def btbEntry() = new Bundle {
    val tag = UInt(btbAddr.tagBits.W)
    val _type = UInt(2.W)
    val target = UInt(32.W)
  }

  val btb = Module(new SRAMTemplate(btbEntry(), set = NRbtb, shouldReset = true, holdRead = true, singlePort = true))
  btb.io.r.req.valid := io.in.pc.valid
  btb.io.r.req.bits.setIdx := btbAddr.getIdx(io.in.pc.bits)

  val btbRead = Wire(btbEntry())
  btbRead := btb.io.r.resp.data(0)
  // since there is one cycle latency to read SyncReadMem,
  // we should latch the input pc for one cycle
  val pcLatch = RegEnable(io.in.pc.bits, io.in.pc.valid)
  val btbHit = btbRead.tag === btbAddr.getTag(pcLatch) && !flush && RegNext(btb.io.r.req.ready, init = false.B)

  // PHT
  val pht = Mem(NRbtb, UInt(2.W))
  val phtTaken = RegEnable(pht.read(btbAddr.getIdx(io.in.pc.bits))(1), io.in.pc.valid)

  // RAS
  val NRras = 16
  val ras = Mem(NRras, UInt(32.W))
  val sp = Counter(NRras)
  val rasTarget = RegEnable(ras.read(sp.value), io.in.pc.valid)

  // update
  val req = WireInit(0.U.asTypeOf(new BPUUpdateReq))
  val btbWrite = WireInit(0.U.asTypeOf(btbEntry()))
  BoringUtils.addSink(req, "bpuUpdateReq")

  btbWrite.tag := btbAddr.getTag(req.pc)
  btbWrite.target := req.actualTarget
  btbWrite._type := req.btbType
  // NOTE: We only update BTB at a miss prediction.
  // If a miss prediction is found, the pipeline will be flushed
  // in the next cycle. Therefore it is safe to use single-port
  // SRAM to implement BTB, since write requests have higher priority
  // than read request. Again, since the pipeline will be flushed
  // in the next cycle, the read request will be useless.
  btb.io.w.req.valid := req.isMissPredict && req.valid
  btb.io.w.req.bits.setIdx := btbAddr.getIdx(req.pc)
  btb.io.w.req.bits.data := btbWrite

  val cnt = RegNext(pht.read(btbAddr.getIdx(req.pc)))
  val reqLatch = RegNext(req)
  when (reqLatch.valid && ALUOpType.isBranch(reqLatch.fuOpType)) {
    val taken = reqLatch.actualTaken
    val newCnt = Mux(taken, cnt + 1.U, cnt - 1.U)
    val wen = (taken && (cnt =/= "b11".U)) || (!taken && (cnt =/= "b00".U))
    when (wen) {
      pht.write(btbAddr.getIdx(reqLatch.pc), newCnt)
    }
  }
  when (req.valid) {
    when (req.fuOpType === ALUOpType.call) {
      ras.write(sp.value + 1.U, req.pc + 4.U)
      sp.value := sp.value + 1.U
    }
    .elsewhen (req.fuOpType === ALUOpType.ret) {
      sp.value := sp.value - 1.U
    }
  }

  val flushBTB = WireInit(false.B)
  val flushTLB = WireInit(false.B)
  BoringUtils.addSink(flushBTB, "MOUFlushICache")
  BoringUtils.addSink(flushTLB, "MOUFlushTLB")

  io.out.target := Mux(btbRead._type === BTBtype.R, rasTarget, btbRead.target)
  io.out.valid := btbHit && Mux(btbRead._type === BTBtype.B, phtTaken, true.B)
  io.out.rtype := 0.U
}
