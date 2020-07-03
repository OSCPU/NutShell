package noop

import chisel3._
import chisel3.util._

import utils._

class WritebackDelayer(bru: Boolean = false, name: String = "unnamedDelayer") extends NOOPModule with HasRSConst with HasBackendConst {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new OOCommitIO))
    val out = Decoupled(new OOCommitIO)
    val mispredictRec = Flipped(new MisPredictionRecIO)
    val brMaskIn = Input(UInt(checkpointSize.W))
    val brMaskOut = Output(UInt(checkpointSize.W))
    val flush = Input(Bool())
    val checkpointIn = if (bru) Some(Input(UInt(brTagWidth.W))) else None
    val freeCheckpoint = if (bru) Some(Output(Valid(UInt(brTagWidth.W)))) else None
  })

  val valid = RegInit(false.B)
  val brMask = Reg(UInt(io.brMaskIn.getWidth.W))
  
  def needMispredictionRecovery(brMask: UInt) = {
    io.mispredictRec.valid && io.mispredictRec.redirect.valid && brMask(io.mispredictRec.checkpoint)
  }

  def updateBrMask(brMask: UInt) = {
    brMask & ~ (UIntToOH(io.mispredictRec.checkpoint) & Fill(checkpointSize, io.mispredictRec.valid))
  }

  brMask := updateBrMask(brMask)
  when(io.in.fire()){brMask := updateBrMask(io.brMaskIn)}
  when(needMispredictionRecovery(brMask) || io.out.fire()){valid := false.B}
  when(io.in.fire()){valid := true.B}
  when(io.flush) {valid := false.B}

  io.brMaskOut := brMask

  io.in.ready := (!valid || io.out.fire()) && !needMispredictionRecovery(io.brMaskIn)
  io.out.bits <> RegEnable(io.in.bits, io.in.fire())
  io.out.valid := valid

  if(bru){
    io.freeCheckpoint.get.bits <> RegEnable(io.checkpointIn.get, io.in.fire())
    io.freeCheckpoint.get.valid := io.out.fire()
  }

  Debug(){
    when(valid){printf("[WBDelay-"+name+"] delayer valid: pc %x brMask %x\n", io.out.bits.decode.cf.pc, brMask)}
  }
}


object WritebackDelayer {
  def apply(in: Data, brmask: UInt, mispredictRec: Data, flush: Bool) = {
    val delayer = Module(new WritebackDelayer())
    delayer.io.in := in
    delayer.io.mispredictRec := mispredictRec
    delayer.io.brMaskIn := brmask
    delayer.io.flush := flush
    delayer
  }
}
