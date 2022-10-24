
package nutcore.frontend

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import nutcore._
import nutcore.frontend.decode._
import nutcore.frontend.instr_fetch._

import utils._
import bus.simplebus._
import chisel3.experimental.IO

class FrontendDynamic(implicit val p: NutCoreConfig) extends NutCoreModule with HasFrontendIO {
  def pipelineConnect2[T <: Data](left: DecoupledIO[T], right: DecoupledIO[T],
    isFlush: Bool, entries: Int = 4, pipe: Boolean = false) = {
    // NOTE: depend on https://github.com/chipsalliance/chisel3/pull/2245
    // right <> Queue(left,  entries = entries, pipe = pipe, flush = Some(isFlush))
    right <> FlushableQueue(left, isFlush, entries = entries, pipe = pipe)
  }

  val ifu  = Module(new InstrFetchDynamic)
  val ibf = Module(new IBF)
  val idu  = Module(new Decode)

  pipelineConnect2(ifu.io.out, ibf.io.in, ifu.io.flushVec(0))
  PipelineVector2Connect(new CtrlFlowIO, ibf.io.out(0), ibf.io.out(1), idu.io.in(0), idu.io.in(1), ifu.io.flushVec(1), if (EnableOutOfOrderExec) 8 else 4)
  ibf.io.flush := ifu.io.flushVec(1)

  io.out <> idu.io.out
  io.redirect <> ifu.io.redirect
  io.flushVec <> ifu.io.flushVec
  io.bpFlush <> ifu.io.bpFlush
  io.ipf <> ifu.io.ipf
  io.imem <> ifu.io.imem

  Debug("------------------------ FRONTEND:------------------------\n")
  Debug("flush = %b, ifu:(%d,%d), ibf:(%d,%d), idu:(%d,%d)\n",
    ifu.io.flushVec.asUInt, ifu.io.out.valid, ifu.io.out.ready,
    ibf.io.in.valid, ibf.io.in.ready, idu.io.in(0).valid, idu.io.in(0).ready)
  Debug(ifu.io.out.valid, "IFU: pc = 0x%x, instr = 0x%x\n", ifu.io.out.bits.pc, ifu.io.out.bits.instr)
  Debug(ibf.io.in.valid, "IBF: pc = 0x%x, instr = 0x%x\n", ibf.io.in.bits.pc, ibf.io.in.bits.instr)
  Debug(idu.io.in(0).valid, "IDU1: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", idu.io.in(0).bits.pc, idu.io.in(0).bits.instr, idu.io.in(0).bits.pnpc)
  Debug(idu.io.in(1).valid, "IDU2: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", idu.io.in(1).bits.pc, idu.io.in(1).bits.instr, idu.io.in(1).bits.pnpc)
}

