package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

// Out of Order Execution Pipeline for NutShell/Argo
// 
// It also serves as a wrapper to adept in-order fu for OoO core

class ExecutionPipelineIO extends NutCoreBundle {
  val in = Flipped(Decoupled(new RenamedDecodeIO))
  val out = Decoupled(new OOCommitIO)
  val mispredictRec = Input(new MisPredictionRecIO)
  val flush = Input(Bool())
}

class ExecutionPipeline extends NutCoreMultiIOModule {
  val io = IO(new ExecutionPipelineIO)
  def access(uop: Data, mispredictRec: Data, flush: Data): Data = {
    this.io.in := uop
    this.io.mispredictRec := mispredictRec
    this.io.flush := flush
    io.out
  }
  def updateBrMask(brMask: UInt) = {
    brMask & ~ (UIntToOH(io.mispredictRec.checkpoint) & Fill(checkpointSize, io.mispredictRec.valid))
  }
  io.out.bits.isMMIO := false.B
  io.out.bits.intrNO := 0.U
  io.out.bits.exception := false.B
  io.out.bits.store := false.B
}

class ALUEP extends ExecutionPipeline {
  val alu = Module(new ALU())
  alu.io.in.valid := io.in.valid
  alu.io.in.bits.src1 := io.in.bits.decode.data.src1
  alu.io.in.bits.src2 := io.in.bits.decode.data.src2
  alu.io.in.bits.func := io.in.bits.decode.ctrl.fuOpType
  alu.io.cfIn := io.in.bits.decode.cf
  alu.io.offset := io.in.bits.decode.data.imm
  alu.io.out.ready := io.out.ready

  io.out.bits.decode := io.in.bits.decode
  io.out.bits.decode.cf.redirect.valid := false.B
  io.out.bits.decode.cf.redirect.rtype := DontCare
  io.out.bits.commits := alu.io.out.bits
  io.out.bits.prfidx := io.in.bits.prfDest
  io.out.bits.brMask := io.in.bits.brMask

  io.in.ready := alu.io.in.ready
  io.out.valid := alu.io.out.valid
}

class BRUEP extends ExecutionPipeline {
  val bruio = IO(new Bundle{
    val mispredictRec = Output(new MisPredictionRecIO)
    val recoverCheckpoint = Input(Valid(UInt(log2Up(checkpointSize).W))) // brurs.io.recoverCheckpoint.get.bits
    val bruRedirect = Output(new RedirectIO)
    val freeCheckpoint = Output(Valid(UInt(brTagWidth.W))) // bruDelayer.io.freeCheckpoint.get
  })

  val bru = Module(new ALU(hasBru = true))
  val bruDelayer = Module(new WritebackDelayer(bru = true))

  bru.io.in.valid := io.in.valid
  bru.io.in.bits.src1 := io.in.bits.decode.data.src1
  bru.io.in.bits.src2 := io.in.bits.decode.data.src2
  bru.io.in.bits.func := io.in.bits.decode.ctrl.fuOpType
  bru.io.cfIn := io.in.bits.decode.cf
  bru.io.offset := io.in.bits.decode.data.imm
  bru.io.out.ready := bruDelayer.io.in.ready

  val brucommit = Wire(new OOCommitIO)
  brucommit.decode := io.in.bits.decode
  brucommit.isMMIO := false.B
  brucommit.intrNO := 0.U
  brucommit.commits := bru.io.out.bits
  brucommit.prfidx := io.in.bits.prfDest
  brucommit.brMask := io.in.bits.brMask
  brucommit.decode.cf.redirect := bru.io.redirect
  brucommit.exception := false.B
  brucommit.store := false.B

  bruDelayer.io.in.bits := brucommit
  bruDelayer.io.in.valid := bru.io.out.valid
  bruDelayer.io.out.ready := io.out.ready
  bruDelayer.io.mispredictRec := bruio.mispredictRec
  bruDelayer.io.flush := io.flush
  bruDelayer.io.checkpointIn.get := bruio.recoverCheckpoint.bits
  io.out.bits := bruDelayer.io.out.bits

  // commit redirect
  bruio.bruRedirect := bruDelayer.io.out.bits.decode.cf.redirect
  bruio.bruRedirect.valid := bruDelayer.io.out.bits.decode.cf.redirect.valid && bruDelayer.io.out.fire()
  bruio.mispredictRec.valid := bruDelayer.io.out.fire()
  bruio.mispredictRec.checkpoint := bruDelayer.io.freeCheckpoint.get.bits
  bruio.mispredictRec.prfidx := bruDelayer.io.out.bits.prfidx
  bruio.mispredictRec.redirect := bruio.bruRedirect

  bruio.freeCheckpoint := bruDelayer.io.freeCheckpoint.get

  io.in.ready := bru.io.in.ready
  io.out.valid := bruDelayer.io.out.valid
}

class MDUEP extends ExecutionPipeline {
  val mduio = IO(new Bundle{
    val mdufinish = Output(Bool())
  })

  val mdu = Module(new MDU)
  val mducommit = Wire(new OOCommitIO)

  val mduDelayer = Module(new WritebackDelayer())

  mdu.io.in.valid := io.in.valid
  mdu.io.in.bits.src1 := io.in.bits.decode.data.src1
  mdu.io.in.bits.src2 := io.in.bits.decode.data.src2
  mdu.io.in.bits.func := io.in.bits.decode.ctrl.fuOpType

  mdu.io.out.ready := mduDelayer.io.in.ready
  mducommit.decode := io.in.bits.decode
  mducommit.isMMIO := false.B
  mducommit.intrNO := 0.U
  mducommit.commits := mdu.io.out.bits
  mducommit.prfidx := io.in.bits.prfDest
  mducommit.decode.cf.redirect.valid := false.B
  mducommit.decode.cf.redirect.rtype := DontCare
  mducommit.exception := false.B
  mducommit.store := false.B
  mducommit.brMask := io.in.bits.brMask

  mduio.mdufinish := mdu.io.out.valid

  mduDelayer.io.in.bits := mducommit
  mduDelayer.io.in.valid := mdu.io.out.valid && io.in.valid
  mduDelayer.io.out.ready := io.out.ready
  mduDelayer.io.mispredictRec := io.mispredictRec
  mduDelayer.io.flush := io.flush
  io.out.bits := mduDelayer.io.out.bits

  io.in.ready := mdu.io.in.ready
  io.out.valid := mduDelayer.io.out.valid
}
