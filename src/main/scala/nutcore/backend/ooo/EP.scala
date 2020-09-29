package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import bus.simplebus._

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

class CSREP(implicit val p: NutCoreConfig) extends ExecutionPipeline {
  val csrio = IO(new Bundle{
    val isBackendException = Input(Bool())
    val memMMU = Flipped(new MemMMUIO)
  })

  // CSR

  val csr = Module(new CSR)
  val csrcommit = Wire(new OOCommitIO)

  csr.io.in.valid := io.in.valid && io.in.bits.decode.ctrl.fuType === FuType.csr || csrio.isBackendException
  csr.io.in.bits.src1 := io.in.bits.decode.data.src1
  csr.io.in.bits.src2 := io.in.bits.decode.data.src2
  csr.io.in.bits.func := io.in.bits.decode.ctrl.fuOpType
  csr.io.cfIn := io.in.bits.decode.cf
  csr.io.out.ready := true.B

  csr.io.instrValid := io.in.valid && !io.flush
  csr.io.isBackendException := csrio.isBackendException

  csrcommit.decode := io.in.bits.decode
  csrcommit.isMMIO := false.B
  csrcommit.intrNO := csr.io.intrNO
  csrcommit.commits := csr.io.out.bits
  csrcommit.prfidx := io.in.bits.prfDest
  csrcommit.decode.cf.redirect := csr.io.redirect
  csrcommit.exception := false.B
  csrcommit.store := false.B
  csrcommit.brMask := DontCare //FIXIT
  // fix wen
  when(csr.io.wenFix){csrcommit.decode.ctrl.rfWen := false.B}

  csr.io.imemMMU <> csrio.memMMU.imem
  csr.io.dmemMMU <> csrio.memMMU.dmem

  // MOU

  val mou = Module(new MOU)
  val moucommit = Wire(new OOCommitIO)
  // mou does not write register

  mou.io.in.valid := io.in.valid && io.in.bits.decode.ctrl.fuType === FuType.mou
  mou.io.in.bits.src1 := io.in.bits.decode.data.src1
  mou.io.in.bits.src2 := io.in.bits.decode.data.src2
  mou.io.in.bits.func := io.in.bits.decode.ctrl.fuOpType

  mou.io.cfIn := io.in.bits.decode.cf
  mou.io.out.ready := true.B // mou will stall the pipeline

  moucommit.decode := io.in.bits.decode
  moucommit.isMMIO := false.B
  moucommit.intrNO := 0.U
  moucommit.commits := DontCare
  moucommit.prfidx := io.in.bits.prfDest
  moucommit.decode.cf.redirect := mou.io.redirect
  moucommit.exception := false.B
  moucommit.store := false.B
  moucommit.brMask := DontCare //FIXIT

  // Output mux

  io.out.bits := Mux(csr.io.in.valid, csrcommit, moucommit)
  assert(!(csr.io.in.valid && mou.io.in.valid))

  io.in.ready := csr.io.in.ready
  io.out.valid := csr.io.out.valid || mou.io.out.valid
}

class LSUEP extends ExecutionPipeline {
  val lsuio = IO(new Bundle{
    val stMaskIn = Input(UInt(robSize.W)) // lsurs.io.stMaskOut.get 
    val robAllocate = Input(Valid(UInt(log2Up(robSize).W)))
    val scommit = Input(Bool()) // rob.io.scommit
    val haveUnfinishedStore = Output(Bool())
    val dmem = new SimpleBusUC(addrBits = VAddrBits, userBits = DCacheUserBundleWidth)
    val dtlb = new SimpleBusUC(addrBits = VAddrBits, userBits = DCacheUserBundleWidth)
  })

  val lsu = Module(new LSU)
  val lsuTlbPF = WireInit(false.B)
  
  io.out.bits.commits := lsu.access(
    valid = io.in.valid,
    src1 = io.in.bits.decode.data.src1, 
    src2 = io.in.bits.decode.data.imm, 
    func = io.in.bits.decode.ctrl.fuOpType, 
    dtlbPF = lsuTlbPF
  )

  lsu.io.uopIn := io.in.bits
  lsu.io.flush := io.flush
  lsu.io.wdata := io.in.bits.decode.data.src2
  lsu.io.mispredictRec := io.mispredictRec

  lsuio.stMaskIn <> lsu.io.stMaskIn
  lsuio.robAllocate <> lsu.io.robAllocate
  lsuio.scommit <> lsu.io.scommit
  lsuio.haveUnfinishedStore <> lsu.io.haveUnfinishedStore
  lsuio.dmem <> lsu.io.dmem
  lsuio.dtlb <> lsu.io.dtlb

  lsu.io.out.ready := true.B // Always permit lsu writeback to reduce load-to-use delay
  io.out.bits.decode := lsu.io.uopOut.decode
  io.out.bits.isMMIO := lsu.io.isMMIO
  io.out.bits.prfidx := lsu.io.uopOut.prfDest
  io.out.bits.exception := lsu.io.exceptionVec.asUInt.orR
  io.out.bits.store := lsu.io.commitStoreToCDB
  io.out.bits.brMask := DontCare

  // fix exceptionVec
  io.out.bits.decode.cf.exceptionVec := lsu.io.exceptionVec

  // Backend exceptions only come from LSU
  // for backend exceptions, we reuse 'intrNO' field in ROB
  // when ROB.exception(x) === 1, intrNO(x) represents backend exception vec for this inst
  io.out.bits.intrNO := io.out.bits.decode.cf.exceptionVec.asUInt

  io.in.ready := lsu.io.in.ready
  io.out.valid := lsu.io.out.valid
}

