package nutcore.frontend.instr_fetch.branch_predict

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import nutcore._

import utils._
import top.Settings

class BranchPredictDummy extends NutCoreModule {
  val io = IO(new Bundle {
    val in = new Bundle { val pc = Flipped(Valid((UInt(VAddrBits.W)))) }
    val out = new RedirectIO
    val valid = Output(Bool())
    val flush = Input(Bool())
    val ignore = Input(Bool())
    val brIdx = Output(Vec(4, Bool()))
  })
  // Note: when io.ignore, io.out.valid must be false.B for this pc
  // This limitation is for cross instline inst fetch logic
  io.valid := io.in.pc.valid // Predictor is returning a result
  io.out.valid := false.B // Need redirect
  io.out.target := DontCare // Redirect target
  io.out.rtype := DontCare // Predictor does not need to care about it 
  io.brIdx := VecInit(Seq.fill(4)(false.B)) // Which inst triggers jump
}
