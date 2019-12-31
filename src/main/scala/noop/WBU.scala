package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import utils._

class WBU(implicit val p: NOOPConfig) extends NOOPModule{
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(Vec(2, new CommitIO)))
    val wb = Vec(2, new WriteBackIO)
    val redirect = new RedirectIO
  })

  val commitPipeline2 = io.in.valid && io.in.bits(1).decode.pipeline2 && !io.redirect.valid
  io.wb(0).rfWen := io.in.bits(0).decode.ctrl.rfWen && io.in.valid
  io.wb(0).rfDest := io.in.bits(0).decode.ctrl.rfDest
  io.wb(0).rfData := io.in.bits(0).commits(io.in.bits(0).decode.ctrl.fuType)
  io.wb(1).rfWen := io.in.bits(1).decode.ctrl.rfWen && commitPipeline2
  io.wb(1).rfDest := io.in.bits(1).decode.ctrl.rfDest
  io.wb(1).rfData := io.in.bits(1).commits(io.in.bits(1).decode.ctrl.fuType)
  io.in.ready := true.B

  io.redirect := io.in.bits(0).decode.cf.redirect
  io.redirect.valid := io.in.bits(0).decode.cf.redirect.valid && io.in.valid

  Debug(true){
    when (io.in.valid) { printf("[COMMIT1] TIMER: %d WBU: pc = 0x%x inst %x wen %x wdst %x wdata %x mmio %x intrNO %x\n", GTimer(), io.in.bits(0).decode.cf.pc, io.in.bits(0).decode.cf.instr, io.wb(0).rfWen, io.wb(0).rfDest, io.wb(0).rfData, io.in.bits(0).isMMIO, io.in.bits(0).intrNO) }
    when (io.in.valid && io.in.bits(1).decode.pipeline2) { printf("[COMMIT2] TIMER: %d WBU: pc = 0x%x inst %x wen %x wdst %x wdata %x mmio %x intrNO %x\n", GTimer(), io.in.bits(1).decode.cf.pc, io.in.bits(1).decode.cf.instr, io.wb(1).rfWen, io.wb(1).rfDest, io.wb(1).rfData, io.in.bits(1).isMMIO, io.in.bits(1).intrNO) }
  }

  BoringUtils.addSource(io.in.valid, "perfCntCondMinstret")
  BoringUtils.addSource(commitPipeline2, "perfCntCondMultiCommit")
  
  if (!p.FPGAPlatform) {
    BoringUtils.addSource(RegNext(io.in.valid), "difftestCommit")
    BoringUtils.addSource(RegNext(commitPipeline2), "difftestMultiCommit")
    BoringUtils.addSource(RegNext(SignExt(io.in.bits(0).decode.cf.pc, AddrBits)), "difftestThisPC")
    BoringUtils.addSource(RegNext(io.in.bits(0).decode.cf.instr), "difftestThisINST")
    BoringUtils.addSource(RegNext(io.in.bits(0).isMMIO), "difftestIsMMIO")
    BoringUtils.addSource(RegNext(io.in.bits(0).decode.cf.instr(1,0)=/="b11".U), "difftestIsRVC")
    BoringUtils.addSource(RegNext(io.in.bits(0).intrNO), "difftestIntrNO")
  } else {
    BoringUtils.addSource(io.in.valid, "ilaWBUvalid")
    BoringUtils.addSource(io.in.bits(0).decode.cf.pc, "ilaWBUpc")
    BoringUtils.addSource(io.wb(0).rfWen, "ilaWBUrfWen")
    BoringUtils.addSource(io.wb(0).rfDest, "ilaWBUrfDest")
    BoringUtils.addSource(io.wb(0).rfData, "ilaWBUrfData")
  }
}
