
package nutcore.backend

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import nutcore._

import utils._
import bus.simplebus._
import difftest._

class BackendSequential(implicit val p: NutCoreConfig) extends NutCoreModule {
  val io = IO(new Bundle {
    val in = Vec(2, Flipped(Decoupled(new DecodeIO)))
    val flush = Input(UInt(2.W))
    val dmem = new SimpleBusUC(addrBits = VAddrBits)
    val memMMU = Flipped(new MemMMUIO)

    val redirect = new RedirectIO
  })

  val isu  = Module(new ISU)
  val exu  = Module(new EXU)
  val wbu  = Module(new WBU)

  PipelineConnect(isu.io.out, exu.io.in, exu.io.out.fire(), io.flush(0))
  PipelineConnect(exu.io.out, wbu.io.in, true.B, io.flush(1))

  isu.io.in <> io.in
  
  isu.io.flush := io.flush(0)
  exu.io.flush := io.flush(1)

  isu.io.wb <> wbu.io.wb
  io.redirect <> wbu.io.redirect
  // forward
  isu.io.forward <> exu.io.forward  

  io.memMMU.imem <> exu.io.memMMU.imem
  io.memMMU.dmem <> exu.io.memMMU.dmem
  io.dmem <> exu.io.dmem
}
