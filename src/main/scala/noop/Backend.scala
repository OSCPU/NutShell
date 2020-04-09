package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import bus.simplebus._

trait HasBackendConst{
  // val multiIssue = true
  val robSize = 16
  val robWidth = 2
  val robInstCapacity = robSize * robWidth
  val rmqSize = 4 // register map queue size
  val prfAddrWidth = log2Up(robSize) + log2Up(robWidth) // physical rf addr width

  val DispatchWidth = 2
  val CommitWidth = 2
  val RetireWidth = 2

  val enableBranchEarlyRedirect = true
}

// Out Of Order Execution Backend 
class Backend(implicit val p: NOOPConfig) extends NOOPModule with HasRegFileParameter with HasBackendConst{

  val io = IO(new Bundle {
    // EXU
    val in = Vec(2, Flipped(Decoupled(new DecodeIO)))
    val flush = Input(Bool())
    val dmem = new SimpleBusUC(addrBits = VAddrBits, userBits = DCacheUserBundleWidth)
    val dtlb = new SimpleBusUC(addrBits = VAddrBits, userBits = DCacheUserBundleWidth)

    val memMMU = Flipped(new MemMMUIO)

    // WBU
    val redirect = new RedirectIO
  })

  // For current version:
  // There is only 1 BRU (combined with ALU1)
  // There is only 1 LSU

  val cdb = Wire(Vec(CommitWidth, Valid(new OOCommitIO)))
  val rf = new RegFile
  val rob = Module(new ROB)

  val alu1rs = Module(new RS(priority = true, name = "ALU1RS"))
  val alu2rs = Module(new RS(priority = true, name = "ALU2RS"))
  val csrrs  = Module(new RS(priority = true, name = "CSRRS", size = 1)) // CSR & MOU
  val lsurs  = Module(new RS(fifo = true, name = "LSURS")) // FIXIT: out of order l/s disabled
  val mdurs  = Module(new RS(priority = true, pipelined = false, name = "MDURS"))

  val instCango = Wire(Vec(DispatchWidth + 1, Bool()))
  val bruRedirect = Wire(new RedirectIO)
  val flushBackend = io.flush || rob.io.redirect.valid && rob.io.redirect.rtype === 1.U
  io.redirect := Mux(rob.io.redirect.valid && rob.io.redirect.rtype === 0.U, rob.io.redirect, bruRedirect)

  rob.io.cdb <> cdb
  rob.io.flush := flushBackend
  when (rob.io.wb(0).rfWen) { rf.write(rob.io.wb(0).rfDest, rob.io.wb(0).rfData) }
  when (rob.io.wb(1).rfWen) { rf.write(rob.io.wb(1).rfDest, rob.io.wb(1).rfData) }
  List.tabulate(DispatchWidth)(i => {
    rob.io.in(i).valid := io.in(i).valid && instCango(i)
    io.in(i).ready := rob.io.in(i).ready && instCango(i)
    rob.io.in(i).bits := io.in(i).bits
  })

  Debug(){
    when(bruRedirect.valid){printf("[MPR] %d: bruRedirect pc %x idx %x to %x\n", GTimer(), cdb(0).bits.decode.cf.pc, cdb(0).bits.prfidx, bruRedirect.target)}
  }

  // ------------------------------------------------
  // Backend stage 1
  // Dispatch
  // ------------------------------------------------

  // Common Data Bus
  // Function Units are divided into 2 groups.
  // For each group, only one inst can be commited to ROB in a single cycle.
  // Group 1: ALU1(BRU) CSR MOU LSU
  // Group 2: ALU2 MDU

  // Choose inst to be dispatched
  // Check structural hazard
  //TODO: Use bit instead of counter
  val mduCnt = Wire(UInt(2.W)) 
  mduCnt := List.tabulate(robWidth)(i => (io.in(i).valid && io.in(i).bits.ctrl.fuType === FuType.mdu)).foldRight(0.U)((sum, i) => sum +& i)
  val lsuCnt = Wire(UInt(2.W)) 
  lsuCnt := List.tabulate(robWidth)(i => (io.in(i).valid && io.in(i).bits.ctrl.fuType === FuType.lsu)).foldRight(0.U)((sum, i) => sum +& i)
  val bruCnt = Wire(UInt(2.W)) 
  bruCnt := List.tabulate(robWidth)(i => (io.in(i).valid && io.in(i).bits.ctrl.fuType === FuType.alu && ALUOpType.isBru(io.in(i).bits.ctrl.fuOpType))).foldRight(0.U)((sum, i) => sum +& i)
  val csrCnt = Wire(UInt(2.W)) 
  csrCnt := List.tabulate(robWidth)(i => (io.in(i).valid && (io.in(i).bits.ctrl.fuType === FuType.csr || io.in(i).bits.ctrl.fuType === FuType.mou))).foldRight(0.U)((sum, i) => sum +& i)

  val rfSrc = List(
    io.in(0).bits.ctrl.rfSrc1,
    io.in(0).bits.ctrl.rfSrc2,
    io.in(1).bits.ctrl.rfSrc1,
    io.in(1).bits.ctrl.rfSrc2
  )
  val rfDest = List(
    io.in(0).bits.ctrl.rfDest,
    io.in(1).bits.ctrl.rfDest
  )

  val inst = Wire(Vec(DispatchWidth + 1, new RenamedDecodeIO))
  
  List.tabulate(DispatchWidth)(i => {
    inst(i).decode := io.in(i).bits
    inst(i).prfDest := Cat(rob.io.index, i.U(1.W))
    inst(i).prfSrc1 := rob.io.aprf(2*i)
    inst(i).prfSrc2 := rob.io.aprf(2*i+1)
    inst(i).src1Rdy := !rob.io.rvalid(2*i) || rob.io.rcommited(2*i)
    inst(i).src2Rdy := !rob.io.rvalid(2*i+1) || rob.io.rcommited(2*i+1)
    // read rf, update src
    inst(i).decode.data.src1 := rf.read(rfSrc(2*i)) 
    when(rob.io.rvalid(2*i) && rob.io.rcommited(2*i)){inst(i).decode.data.src1 := rob.io.rprf(2*i)}
    inst(i).decode.data.src2 := rf.read(rfSrc(2*i + 1)) 
    when(rob.io.rvalid(2*i+1) && rob.io.rcommited(2*i+1)){inst(i).decode.data.src2 := rob.io.rprf(2*i+1)}
    // TODO: ROB WB
  })
  inst(DispatchWidth) := DontCare

  def isDepend(rfSrc: UInt, rfDest: UInt, wen: Bool): Bool = (rfSrc =/= 0.U) && (rfSrc === rfDest) && wen
  def isDepend2(rfSrc: UInt, rfDest1: UInt, wen1: Bool, rfDest2: UInt, wen2: Bool): Bool = (rfSrc =/= 0.U) && ((rfSrc === rfDest1) && wen1 || (rfSrc === rfDest2) && wen2)

  // check dependency for insts at commit stage
  List.tabulate(DispatchWidth)(i => {
    List.tabulate(CommitWidth)(j => {
      when(inst(i).prfSrc1 === cdb(j).bits.prfidx && cdb(j).valid && cdb(j).bits.decode.ctrl.rfWen && rob.io.rvalid(2*i)){
        inst(i).src1Rdy := true.B
        inst(i).decode.data.src1 := cdb(j).bits.commits
      }
      when(inst(i).prfSrc2 === cdb(j).bits.prfidx && cdb(j).valid && cdb(j).bits.decode.ctrl.rfWen && rob.io.rvalid(2*i+1)){
        inst(i).src2Rdy := true.B
        inst(i).decode.data.src2 := cdb(j).bits.commits
      }
    })
  })

  // check dependency for insts at dispatch stage
  when(isDepend(inst(1).decode.ctrl.rfSrc1, inst(0).decode.ctrl.rfDest, inst(0).decode.ctrl.rfWen)){
    inst(1).src1Rdy := false.B
    inst(1).prfSrc1 := Cat(rob.io.index, 0.U)
  }
  when(isDepend(inst(1).decode.ctrl.rfSrc2, inst(0).decode.ctrl.rfDest, inst(0).decode.ctrl.rfWen)){
    inst(1).src2Rdy := false.B
    inst(1).prfSrc2 := Cat(rob.io.index, 0.U)
  }

  // fix src
  List.tabulate(DispatchWidth)(i => {
    when(io.in(i).bits.ctrl.src1Type === SrcType.pc){
      inst(i).src1Rdy := true.B
      inst(i).decode.data.src1 := SignExt(io.in(i).bits.cf.pc, AddrBits)
    }
    when(io.in(i).bits.ctrl.src2Type =/= SrcType.reg){
      inst(i).src2Rdy := true.B
      inst(i).decode.data.src2 := io.in(i).bits.data.imm
    } 
  })

  //TODO: refactor src gen with Mux1H

  // We have to block store inst before we decouple load and store
  val hasBlockInst = List.tabulate(DispatchWidth)(i => io.in(i).bits.ctrl.noSpecExec || io.in(i).bits.ctrl.isBlocked)
  val pipeLineEmpty = rob.io.empty && alu1rs.io.empty && alu2rs.io.empty && csrrs.io.empty && lsurs.io.empty && mdurs.io.empty

  // Chicken Bit
  val Chicken = false
  if(Chicken){
    hasBlockInst(0) := true.B
    hasBlockInst(1) := true.B
  }

  val blockReg = RegInit(false.B)
  val haveUnfinishedStore = Wire(Bool())
  when((rob.io.empty || flushBackend) && !haveUnfinishedStore){ blockReg := false.B }
  when(io.in(0).bits.ctrl.isBlocked && io.in(0).fire()){ blockReg := true.B }
  val mispredictionRecoveryReg = RegInit(false.B)
  when(io.redirect.valid && io.redirect.rtype === 1.U){ mispredictionRecoveryReg := true.B }
  when(rob.io.empty || flushBackend){ mispredictionRecoveryReg := false.B }
  val mispredictionRecovery = mispredictionRecoveryReg && !rob.io.empty || io.redirect.valid && io.redirect.rtype === 1.U // waiting for misprediction recovery or misprediction detected

  instCango(0) := 
    io.in(0).valid &&
    rob.io.in(0).ready && // rob has empty slot
    !(hasBlockInst(0) && !pipeLineEmpty) &&
    !blockReg &&
    !mispredictionRecovery &&
    LookupTree(io.in(0).bits.ctrl.fuType, List(
      FuType.alu -> alu1rs.io.in.ready,
      FuType.lsu -> lsurs.io.in.ready,
      FuType.mdu -> mdurs.io.in.ready,
      FuType.csr -> csrrs.io.in.ready,
      FuType.mou -> csrrs.io.in.ready
    ))
  instCango(1) := 
    instCango(0) &&
    io.in(1).valid &&
    rob.io.in(1).ready && // rob has empty slot
    !hasBlockInst(0) && // there is no block inst
    !hasBlockInst(1) &&
    !blockReg &&
    !mispredictionRecovery &&
    LookupTree(io.in(1).bits.ctrl.fuType, List(
      FuType.alu -> (alu2rs.io.in.ready && !ALUOpType.isBru(inst(1).decode.ctrl.fuOpType)),
      FuType.lsu -> (lsurs.io.in.ready && (lsuCnt < 2.U)),
      FuType.mdu -> (mdurs.io.in.ready && (mduCnt < 2.U)),
      FuType.csr -> (csrrs.io.in.ready && (csrCnt < 2.U)),
      FuType.mou -> (csrrs.io.in.ready && (csrCnt < 2.U))
    ))
  instCango(2) := false.B
  assert(!(instCango(1) && !instCango(0))) // insts must be dispatched in seq

  val noInst = 2.U
  val alu1Inst = Mux(inst(0).decode.ctrl.fuType === FuType.alu, 0.U, noInst)
  val alu2Inst = Mux(inst(1).decode.ctrl.fuType === FuType.alu, 1.U, noInst)
  val csrInst  = Mux(inst(0).decode.ctrl.fuType === FuType.csr || inst(0).decode.ctrl.fuType === FuType.mou, 0.U, noInst)
  val lsuInst  = Mux(inst(0).decode.ctrl.fuType === FuType.lsu, 0.U, Mux(inst(1).decode.ctrl.fuType === FuType.lsu, 1.U, noInst))
  val mduInst  = Mux(inst(0).decode.ctrl.fuType === FuType.mdu, 0.U, Mux(inst(1).decode.ctrl.fuType === FuType.mdu, 1.U, noInst))

  def updateBrMask(brMask: UInt) = {
    brMask & ~ List.tabulate(CommitWidth)(i => (UIntToOH(cdb(i).bits.prfidx) & Fill(robInstCapacity, cdb(i).valid))).foldRight(0.U)((sum, i) => sum | i)
  }

  val brMaskReg = RegInit(0.U(robInstCapacity.W))
  val brMaskGen = updateBrMask(brMaskReg & ~rob.io.brMaskClearVec)
  val brMask = Wire(Vec(robWidth+1, UInt(robInstCapacity.W)))
  val isBranch = List.tabulate(robWidth)(i => io.in(i).valid && io.in(i).bits.ctrl.fuType === FuType.alu && ALUOpType.isBru(io.in(i).bits.ctrl.fuOpType))
  brMask(0) := brMaskGen
  brMask(1) := brMaskGen | (UIntToOH(inst(0).prfDest) & Fill(robInstCapacity, io.in(0).fire() && isBranch(0)))
  brMask(2) := brMask(1) | (UIntToOH(inst(1).prfDest) & Fill(robInstCapacity, io.in(1).fire() && isBranch(1)))
  brMaskReg := Mux(flushBackend, 0.U, brMask(2))

  Debug(){
    printf("[brMAsk] %d: old %x -> new %x\n", GTimer(), brMaskReg, Mux(flushBackend, 0.U, brMask(2)))
  }

  alu1rs.io.in.valid := instCango(alu1Inst) 
  alu2rs.io.in.valid := instCango(alu2Inst) 
  csrrs.io.in.valid  := instCango(csrInst)
  lsurs.io.in.valid  := instCango(lsuInst)
  mdurs.io.in.valid  := instCango(mduInst)

  alu1rs.io.in.bits := inst(alu1Inst) 
  alu2rs.io.in.bits := inst(alu2Inst) 
  csrrs.io.in.bits  := inst(csrInst)
  lsurs.io.in.bits  := inst(lsuInst)
  mdurs.io.in.bits  := inst(mduInst)

  // val rs = List(alu1rs, alu2rs, csrrs, lsurs, mdurs)
  // List.tabulate(5)(i => {
  //   rs(i).io.commit.valid := cdb.valid
  //   rs(i).io.commit.bits := cdb.bits
  // )}

  alu1rs.io.flush := flushBackend
  alu2rs.io.flush := flushBackend
  csrrs.io.flush  := flushBackend
  lsurs.io.flush  := flushBackend
  mdurs.io.flush  := flushBackend

  alu1rs.io.brMaskIn := brMask(alu1Inst)
  alu2rs.io.brMaskIn := brMask(alu2Inst)
  csrrs.io.brMaskIn  := brMask(csrInst)
  lsurs.io.brMaskIn  := brMask(lsuInst)
  mdurs.io.brMaskIn  := brMask(mduInst)

  alu1rs.io.cdb <> cdb
  alu2rs.io.cdb <> cdb
  csrrs.io.cdb  <> cdb
  lsurs.io.cdb  <> cdb
  mdurs.io.cdb  <> cdb

  List.tabulate(DispatchWidth)(i => {
    rob.io.in(i).valid := instCango(i)
    rob.io.in(i).bits := inst(i).decode
    rob.io.brMaskIn(i) := brMask(i)
  })

  // ------------------------------------------------
  // Backend stage 2
  // Issue
  // ------------------------------------------------

  // Backend exception regs

  val raiseBackendException = WireInit(false.B)
  val commitBackendException = WireInit(false.B)

  commitBackendException := rob.io.exception

  // Function Units
  // TODO: FU template

  val alu1 = Module(new ALU(hasBru = true))
  val alu1commit = Wire(new OOCommitIO)
  val aluOut = alu1.access(
    valid = alu1rs.io.out.valid, 
    src1 = alu1rs.io.out.bits.decode.data.src1, 
    src2 = alu1rs.io.out.bits.decode.data.src2, 
    func = alu1rs.io.out.bits.decode.ctrl.fuOpType
  )
  alu1.io.cfIn := alu1rs.io.out.bits.decode.cf
  alu1.io.offset := alu1rs.io.out.bits.decode.data.imm
  alu1.io.out.ready := true.B //TODO
  alu1commit.decode := alu1rs.io.out.bits.decode
  alu1commit.isMMIO := false.B
  alu1commit.intrNO := 0.U
  alu1commit.commits := aluOut
  alu1commit.prfidx := alu1rs.io.out.bits.prfDest
  // commit redirect
  bruRedirect :=  alu1.io.redirect
  if(enableBranchEarlyRedirect){
    alu1commit.decode.cf.redirect := alu1.io.redirect
    alu1commit.exception := false.B
    bruRedirect.valid := alu1.io.redirect.valid && alu1rs.io.out.fire()
  } else {
    alu1commit.decode.cf.redirect := alu1.io.redirect
    alu1commit.decode.cf.redirect.rtype := 0.U // force set rtype to 0
    alu1commit.exception := false.B
    bruRedirect.valid := false.B
  }

  // def isBru(func: UInt) = func(4)
  val alu2 = Module(new ALU)
  val alu2commit = Wire(new OOCommitIO)
  val alu2Out = alu2.access(
    valid = alu2rs.io.out.valid, 
    src1 = alu2rs.io.out.bits.decode.data.src1, 
    src2 = alu2rs.io.out.bits.decode.data.src2, 
    func = alu2rs.io.out.bits.decode.ctrl.fuOpType
  )
  alu2.io.cfIn :=  alu2rs.io.out.bits.decode.cf
  alu2.io.offset := alu2rs.io.out.bits.decode.data.imm
  alu2.io.out.ready := true.B //TODO
  alu2commit.decode := alu2rs.io.out.bits.decode
  alu2commit.isMMIO := false.B
  alu2commit.intrNO := 0.U
  alu2commit.commits := alu2Out
  alu2commit.prfidx := alu2rs.io.out.bits.prfDest
  alu2commit.decode.cf.redirect.valid := false.B
  alu2commit.decode.cf.redirect.rtype := DontCare
  alu2commit.exception := false.B

  val lsu = Module(new LSU)
  val lsucommit = Wire(new OOCommitIO)
  val lsuTlbPF = WireInit(false.B)
  
  val lsuUop = lsurs.io.out.bits

  val lsuOut = lsu.access(
    valid = lsurs.io.out.valid,
    src1 = lsuUop.decode.data.src1, 
    src2 = lsuUop.decode.data.imm, 
    func = lsuUop.decode.ctrl.fuOpType, 
    dtlbPF = lsuTlbPF
  )
  lsu.io.uopIn := lsuUop
  lsu.io.brMaskIn := lsurs.io.brMaskOut
  lsu.io.cdb := cdb
  lsu.io.scommit := rob.io.scommit
  haveUnfinishedStore := lsu.io.haveUnfinishedStore
  lsu.io.flush := flushBackend
  lsu.io.wdata := lsuUop.decode.data.src2
  // lsu.io.instr := lsuUop.decode.cf.instr
  io.dmem <> lsu.io.dmem
  io.dtlb <> lsu.io.dtlb
  BoringUtils.addSource(io.memMMU.dmem.loadPF, "loadPF") // FIXIT: this is nasty
  BoringUtils.addSource(io.memMMU.dmem.storePF, "storePF") // FIXIT: this is nasty
  lsu.io.out.ready := true.B //TODO
  lsucommit.decode := lsu.io.uopOut.decode
  lsucommit.isMMIO := lsu.io.isMMIO
  lsucommit.commits := lsuOut
  lsucommit.prfidx := lsu.io.uopOut.prfDest
  lsucommit.decode.cf.redirect.valid := false.B
  lsucommit.decode.cf.redirect.rtype := DontCare
  lsucommit.exception := lsu.io.exceptionVec.asUInt.orR
  // fix exceptionVec
  lsucommit.decode.cf.exceptionVec := lsu.io.exceptionVec

  // backend exceptions only come from LSU
  raiseBackendException := lsucommit.exception && lsu.io.out.fire()
  // for backend exceptions, we reuse 'intrNO' field in ROB
  // when ROB.exception(x) === 1, intrNO(x) represents backend exception vec for this inst
  lsucommit.intrNO := lsucommit.decode.cf.exceptionVec.asUInt

  val mdu = Module(new MDU)
  val mducommit = Wire(new OOCommitIO)
  val mduOut = mdu.access(
    valid = mdurs.io.out.valid, 
    src1 = mdurs.io.out.bits.decode.data.src1, 
    src2 = mdurs.io.out.bits.decode.data.src2, 
    func = mdurs.io.out.bits.decode.ctrl.fuOpType
  )
  mdu.io.out.ready := true.B //TODO
  mducommit.decode := mdurs.io.out.bits.decode
  mducommit.isMMIO := false.B
  mducommit.intrNO := 0.U
  mducommit.commits := mduOut
  mducommit.prfidx := mdurs.io.out.bits.prfDest
  mducommit.decode.cf.redirect.valid := false.B
  mducommit.decode.cf.redirect.rtype := DontCare
  mducommit.exception := false.B
  mdurs.io.commit.get := mdu.io.out.fire()

  val csrVaild = csrrs.io.out.valid && csrrs.io.out.bits.decode.ctrl.fuType === FuType.csr || commitBackendException
  val csrUop = WireInit(csrrs.io.out.bits)
  when(commitBackendException){
    csrUop := rob.io.beUop
  }
  val csr = Module(new CSR)
  val csrcommit = Wire(new OOCommitIO)
  val csrOut = csr.access(
    valid = csrVaild, 
    src1 = csrUop.decode.data.src1, 
    src2 = csrUop.decode.data.src2, 
    func = csrUop.decode.ctrl.fuOpType
  )
  csr.io.cfIn := csrUop.decode.cf
  csr.io.instrValid := csrVaild && !flushBackend
  csr.io.isBackendException := commitBackendException
  csr.io.out.ready := true.B
  csrcommit.decode := csrUop.decode
  csrcommit.isMMIO := false.B
  csrcommit.intrNO := csr.io.intrNO
  csrcommit.commits := csrOut
  csrcommit.prfidx := csrUop.prfDest
  csrcommit.decode.cf.redirect := csr.io.redirect
  csrcommit.exception := false.B
  // fix wen
  when(csr.io.wenFix){csrcommit.decode.ctrl.rfWen := false.B}

  csr.io.imemMMU <> io.memMMU.imem
  csr.io.dmemMMU <> io.memMMU.dmem

  Debug(){
    when(csrVaild && commitBackendException){
      printf("[BACKEND EXC] time %d pc %x inst %x evec %b\n", GTimer(), csrUop.decode.cf.pc, csrUop.decode.cf.instr, csrUop.decode.cf.exceptionVec.asUInt)
    }
  }

  val mou = Module(new MOU)
  val moucommit = Wire(new OOCommitIO)
  // mou does not write register
  mou.access(
    valid = csrrs.io.out.valid && csrrs.io.out.bits.decode.ctrl.fuType === FuType.mou, 
    src1 = csrrs.io.out.bits.decode.data.src1, 
    src2 = csrrs.io.out.bits.decode.data.src2, 
    func = csrrs.io.out.bits.decode.ctrl.fuOpType
  )
  mou.io.cfIn := csrrs.io.out.bits.decode.cf
  mou.io.out.ready := true.B // mou will stall the pipeline
  moucommit.decode := csrrs.io.out.bits.decode
  moucommit.isMMIO := false.B
  moucommit.intrNO := 0.U
  moucommit.commits := DontCare
  moucommit.prfidx := csrrs.io.out.bits.prfDest
  moucommit.decode.cf.redirect := mou.io.redirect
  moucommit.exception := false.B

  // ------------------------------------------------
  // Backend stage 3+
  // Exec
  // ------------------------------------------------

  // ------------------------------------------------
  // Backend final stage
  // Commit to CDB
  // ------------------------------------------------

  // CDB arbit
  val (srcALU1, srcALU2, srcLSU, srcMDU, srcCSR, srcMOU) = (0, 1, 2, 3, 4, 5)
  val commit = VecInit(List(alu1commit, alu2commit, lsucommit, mducommit, csrcommit, moucommit))
  val commitValid = VecInit(List(alu1.io.out.valid, alu2.io.out.valid, lsu.io.out.valid, mdu.io.out.valid && mdurs.io.out.valid, csr.io.out.valid, mou.io.out.valid))

  val Src1Priority = Seq(
    srcCSR,
    srcMOU,
    srcMDU,
    srcALU1
  )
  val Src2Priority = Seq(
    srcLSU,
    srcALU2
  )

  val cmtStrHaz = List(
    alu1.io.out.valid.asUInt +& (mdu.io.out.valid && mdurs.io.out.valid).asUInt > 1.U,
    alu2.io.out.valid.asUInt +& lsu.io.out.valid.asUInt > 1.U
  )

  val cdbSrc1 = Src1Priority.foldRight(0.U)((i: Int, sum: UInt) => Mux(commitValid(i), i.U, sum))
  val cdbSrc2 = Src2Priority.foldRight(1.U)((i: Int, sum: UInt) => Mux(commitValid(i), i.U, sum))
  cdb(0).valid := commitValid(cdbSrc1)
  cdb(0).bits := commit(cdbSrc1)
  // cdb(0).ready := true.B
  cdb(1).valid := commitValid(cdbSrc2)
  cdb(1).bits := commit(cdbSrc2)
  // cdb(1).ready := true.B

  alu1rs.io.out.ready := cdbSrc1 === srcALU1.U
  alu2rs.io.out.ready := cdbSrc2 === srcALU2.U
  csrrs.io.out.ready := csr.io.in.ready
  lsurs.io.out.ready := lsu.io.in.ready
  mdurs.io.out.ready := mdu.io.in.ready

  Debug(){when(flushBackend){printf("[FLUSH] TIMER: %d\n", GTimer())}}
  Debug(){when(io.redirect.valid){printf("[RDIRECT] TIMER: %d target 0x%x\n", GTimer(), io.redirect.target)}}

  // Performance Counter

  val isBru = ALUOpType.isBru(alu1commit.decode.ctrl.fuOpType)
  BoringUtils.addSource(alu1.io.out.fire() && !isBru, "perfCntCondMaluInstr") //TODO: Fix it
  BoringUtils.addSource(alu1.io.out.fire() && isBru, "perfCntCondMbruInstr") //TODO: Fix it
  BoringUtils.addSource(lsu.io.out.fire(), "perfCntCondMlsuInstr")
  BoringUtils.addSource(mdu.io.out.fire(), "perfCntCondMmduInstr")

  BoringUtils.addSource(!rob.io.in(0).ready, "perfCntCondMrobFull")
  BoringUtils.addSource(!alu1rs.io.in.ready, "perfCntCondMalu1rsFull")
  BoringUtils.addSource(!alu2rs.io.in.ready, "perfCntCondMalu2rsFull")
  BoringUtils.addSource(!alu1rs.io.in.ready, "perfCntCondMbrursFull")
  BoringUtils.addSource(!lsurs.io.in.ready, "perfCntCondMlsursFull")
  BoringUtils.addSource(!mdurs.io.in.ready, "perfCntCondMmdursFull")
  BoringUtils.addSource(rob.io.empty, "perfCntCondMrobEmpty")
  BoringUtils.addSource(cmtStrHaz(0), "perfCntCondMcmtStrHaz1")
  BoringUtils.addSource(cmtStrHaz(1), "perfCntCondMcmtStrHaz2")
  BoringUtils.addSource(alu2.io.out.fire(), "perfCntCondMaluInstr2")
  BoringUtils.addSource(rob.io.in(0).fire() ^ rob.io.in(1).fire(), "perfCntCondMdispatch1")
  BoringUtils.addSource(rob.io.in(0).fire() & rob.io.in(1).fire(), "perfCntCondMdispatch2")

  if (!p.FPGAPlatform) {
    BoringUtils.addSource(VecInit((0 to NRReg-1).map(i => rf.read(i.U))), "difftestRegs")
  }

  if (!p.FPGAPlatform) {
    val mon = Module(new Monitor)
    val cycleCnt = WireInit(0.U(XLEN.W))
    val instrCnt = WireInit(0.U(XLEN.W))
    val nooptrap = csrrs.io.out.bits.decode.ctrl.isNoopTrap && csrrs.io.out.valid
    mon.io.clk := clock
    mon.io.reset := reset.asBool
    mon.io.isNoopTrap := nooptrap
    mon.io.trapCode := csrrs.io.out.bits.decode.data.src1
    mon.io.trapPC := csrrs.io.out.bits.decode.cf.pc
    mon.io.cycleCnt := cycleCnt
    mon.io.instrCnt := instrCnt

    BoringUtils.addSink(cycleCnt, "simCycleCnt")
    BoringUtils.addSink(instrCnt, "simInstrCnt")
    BoringUtils.addSource(nooptrap, "nooptrap")
  }
  
}
