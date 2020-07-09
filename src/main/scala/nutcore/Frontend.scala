package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import bus.simplebus._

class Frontend(implicit val p: NutCoreConfig) extends NutCoreModule {
  val io = IO(new Bundle {
    val out = Vec(2, Decoupled(new DecodeIO))
    val imem = new SimpleBusUC(userBits = ICacheUserBundleWidth, addrBits = VAddrBits)
    val flushVec = Output(UInt(4.W))
    val bpFlush = Output(Bool())
    val ipf = Input(Bool())
    val redirect = Flipped(new RedirectIO)
  })

  def pipelineConnect2[T <: Data](left: DecoupledIO[T], right: DecoupledIO[T],
    isFlush: Bool, entries: Int = 4, pipe: Boolean = false) = {
    right <> FlushableQueue(left, isFlush,  entries = entries, pipe = pipe)
  }

  val ifu  = Module(new IFU)
  val ibf = Module(new IBF)
  val idu  = Module(new IDU)

  pipelineConnect2(ifu.io.out, ibf.io.in, ifu.io.flushVec(0))
  PipelineVector2Connect(new CtrlFlowIO, ibf.io.out(0), ibf.io.out(1), idu.io.in(0), idu.io.in(1), ifu.io.flushVec(1), if (EnableOutOfOrderExec) 8 else 4)
  ibf.io.flush := ifu.io.flushVec(1)

  io.out <> idu.io.out
  io.redirect <> ifu.io.redirect
  io.flushVec <> ifu.io.flushVec
  io.bpFlush <> ifu.io.bpFlush
  io.ipf <> ifu.io.ipf
  io.imem <> ifu.io.imem

  Debug() {
    printf("------------------------ FRONTEND: %d ------------------------\n", GTimer())
    printf("flush = %b, ifu:(%d,%d), ibf:(%d,%d), idu:(%d,%d)\n",
      ifu.io.flushVec.asUInt, ifu.io.out.valid, ifu.io.out.ready,
      ibf.io.in.valid, ibf.io.in.ready, idu.io.in(0).valid, idu.io.in(0).ready)
    when (ifu.io.out.valid) { printf("IFU: pc = 0x%x, instr = 0x%x\n", ifu.io.out.bits.pc, ifu.io.out.bits.instr)} ; 
    when (ibf.io.in.valid) { printf("IBF: pc = 0x%x, instr = 0x%x\n", ibf.io.in.bits.pc, ibf.io.in.bits.instr)}
    when (idu.io.in(0).valid) { printf("IDU1: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", idu.io.in(0).bits.pc, idu.io.in(0).bits.instr, idu.io.in(0).bits.pnpc) }
    when (idu.io.in(1).valid) { printf("IDU2: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", idu.io.in(1).bits.pc, idu.io.in(1).bits.instr, idu.io.in(1).bits.pnpc) }
  }

}

class Frontend_dummy(implicit val p: NutCoreConfig) extends NutCoreModule {
  val io = IO(new Bundle {
    val out = Vec(2, Decoupled(new DecodeIO))
    val imem = new SimpleBusUC(userBits = ICacheUserBundleWidth, addrBits = VAddrBits)
    val flushVec = Output(UInt(4.W))
    val bpFlush = Output(Bool())
    val ipf = Input(Bool())
    val redirect = Flipped(new RedirectIO)
  })

  val ifu  = Module(new IFU_dummy)
  val idu  = Module(new IDU)

  PipelineConnect(ifu.io.out, idu.io.in(0), idu.io.out(0).fire(), ifu.io.flushVec(0))
  idu.io.in(1) := DontCare

  io.out <> idu.io.out
  io.redirect <> ifu.io.redirect
  io.flushVec <> ifu.io.flushVec
  io.bpFlush <> ifu.io.bpFlush
  io.ipf <> ifu.io.ipf
  io.imem <> ifu.io.imem

  Debug() {
    printf("------------------------ FRONTEND: %d ------------------------\n", GTimer())
    printf("flush = %b, ifu:(%d,%d), idu:(%d,%d)\n",
      ifu.io.flushVec.asUInt, ifu.io.out.valid, ifu.io.out.ready, idu.io.in(0).valid, idu.io.in(0).ready)
    when (ifu.io.out.valid) { printf("IFU: pc = 0x%x, instr = 0x%x\n", ifu.io.out.bits.pc, ifu.io.out.bits.instr)} ; 
    when (idu.io.in(0).valid) { printf("IDU1: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", idu.io.in(0).bits.pc, idu.io.in(0).bits.instr, idu.io.in(0).bits.pnpc) }
  }
}