
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

class FrontendSequential(implicit val p: NutCoreConfig) extends NutCoreModule with HasFrontendIO {
  val ifu  = Module(new InstrFetchSequential)
  val ibf = Module(new NaiveRVCAlignBuffer)
  val idu  = Module(new Decode)

  def PipelineConnect2[T <: Data](left: DecoupledIO[T], right: DecoupledIO[T],
    isFlush: Bool, entries: Int = 4, pipe: Boolean = false) = {
    // NOTE: depend on https://github.com/chipsalliance/chisel3/pull/2245
    // right <> Queue(left,  entries = entries, pipe = pipe, flush = Some(isFlush))
    right <> FlushableQueue(left, isFlush, entries = entries, pipe = pipe)
  }

  PipelineConnect2(ifu.io.out, ibf.io.in, ifu.io.flushVec(0))
  PipelineConnect(ibf.io.out, idu.io.in(0), idu.io.out(0).fire(), ifu.io.flushVec(1))
  idu.io.in(1) := DontCare

  ibf.io.flush := ifu.io.flushVec(1)
  io.out <> idu.io.out
  io.redirect <> ifu.io.redirect
  io.flushVec <> ifu.io.flushVec
  io.bpFlush <> ifu.io.bpFlush
  io.ipf <> ifu.io.ipf
  io.imem <> ifu.io.imem

  Debug("------------------------ FRONTEND:------------------------\n")
  Debug("flush = %b, ifu:(%d,%d), idu:(%d,%d)\n",
    ifu.io.flushVec.asUInt, ifu.io.out.valid, ifu.io.out.ready, idu.io.in(0).valid, idu.io.in(0).ready)
  Debug(ifu.io.out.valid, "IFU: pc = 0x%x, instr = 0x%x\n", ifu.io.out.bits.pc, ifu.io.out.bits.instr)
  Debug(idu.io.in(0).valid, "IDU1: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", idu.io.in(0).bits.pc, idu.io.in(0).bits.instr, idu.io.in(0).bits.pnpc)
}