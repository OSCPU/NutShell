
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


class FrontendEmbedded(implicit val p: NutCoreConfig) extends NutCoreModule with HasFrontendIO {
  val ifu  = Module(new InstrFetchEmbedded)
  val idu  = Module(new Decode)

  PipelineConnect(ifu.io.out, idu.io.in(0), idu.io.out(0).fire(), ifu.io.flushVec(0))
  idu.io.in(1) := DontCare

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
