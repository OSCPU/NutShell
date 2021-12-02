/**************************************************************************************
* Copyright (c) 2020 Institute of Computing Technology, CAS
* Copyright (c) 2020 University of Chinese Academy of Sciences
* 
* NutShell is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2. 
* You may obtain a copy of Mulan PSL v2 at:
*             http://license.coscl.org.cn/MulanPSL2 
* 
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER 
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR 
* FIT FOR A PARTICULAR PURPOSE.  
*
* See the Mulan PSL v2 for more details.  
***************************************************************************************/

package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import bus.simplebus._
import difftest._

trait HasBackendConst{
  // val multiIssue = true
  val robSize = 16
  val robWidth = 2
  val robInstCapacity = robSize * robWidth
  val checkpointSize = 4 // register map checkpoint size
  val brTagWidth = log2Up(checkpointSize)
  val prfAddrWidth = log2Up(robSize) + log2Up(robWidth) // physical rf addr width

  val DispatchWidth = 2
  val CommitWidth = 2
  val RetireWidth = 2

  val enableCheckpoint = true
}

// NutShell/Argo Out Of Order Execution Backend
class Backend_ooo(implicit val p: NutCoreConfig) extends NutCoreModule with HasRegFileParameter with HasBackendConst{

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
  // There is only 1 BRU
  // There is only 1 LSU

  val cdb = Wire(Vec(CommitWidth, Valid(new OOCommitIO)))
  val rf = new RegFile
  val rob = Module(new ROB)

  val brurs  = Module(new RS(priority = true, size = checkpointSize, checkpoint = true, name = "BRURS"))
  val alu1rs = Module(new RS(priority = true, size = 4, name = "ALU1RS"))
  val alu2rs = Module(new RS(priority = true, size = 4, name = "ALU2RS"))
  val csrrs  = Module(new RS(priority = true, size = 1, name = "CSRRS")) // CSR & MOU
  val lsurs  = Module(new RS(storeSeq = true, size = 4, name = "LSURS")) // FIXIT: out of order l/s disabled
  val mdurs  = Module(new RS(priority = true, size = 4, pipelined = false, name = "MDURS"))

  val bruDelayer = Module(new WritebackDelayer(bru = true))
  val mduDelayer = Module(new WritebackDelayer())

  val instCango = Wire(Vec(DispatchWidth + 1, Bool()))
  val bruRedirect = Wire(new RedirectIO)
  val mispredictRec = Wire(new MisPredictionRecIO)
  val flushBackend = if(enableCheckpoint){
    io.flush 
  } else {
    io.flush || rob.io.redirect.valid && rob.io.redirect.rtype === 1.U
  }

  io.redirect := Mux(rob.io.redirect.valid && rob.io.redirect.rtype === 0.U, rob.io.redirect, bruRedirect)

  rob.io.cdb <> cdb
  rob.io.mispredictRec := mispredictRec
  rob.io.flush := flushBackend
  when (rob.io.wb(0).rfWen) { rf.write(rob.io.wb(0).rfDest, rob.io.wb(0).rfData) }
  when (rob.io.wb(1).rfWen) { rf.write(rob.io.wb(1).rfDest, rob.io.wb(1).rfData) }
  List.tabulate(DispatchWidth)(i => {
    rob.io.in(i).valid := io.in(i).valid && instCango(i)
    io.in(i).ready := rob.io.in(i).ready && instCango(i)
    rob.io.in(i).bits := io.in(i).bits
  })

  brurs.io.updateCheckpoint.get <> rob.io.updateCheckpoint
  rob.io.recoverCheckpoint.bits := bruDelayer.io.freeCheckpoint.get.bits
  brurs.io.freeCheckpoint.get <> bruDelayer.io.freeCheckpoint.get
  if(enableCheckpoint){
    rob.io.recoverCheckpoint.valid := io.redirect.valid && io.redirect.rtype === 1.U
  } else {
    rob.io.recoverCheckpoint.valid := false.B
    rob.io.updateCheckpoint.valid := false.B
  }

  // ------------------------------------------------
  // Backend stage 1
  // Dispatch
  // ------------------------------------------------

  // Choose inst to be dispatched
  // Check structural hazard
  //TODO: Use bit instead of counter
  val mduCnt = Wire(UInt(2.W)) 
  mduCnt := List.tabulate(robWidth)(i => (io.in(i).valid && io.in(i).bits.ctrl.fuType === FuType.mdu)).foldRight(0.U)((sum, i) => sum +& i)
  val lsuCnt = Wire(UInt(2.W)) 
  lsuCnt := List.tabulate(robWidth)(i => (io.in(i).valid && io.in(i).bits.ctrl.fuType === FuType.lsu)).foldRight(0.U)((sum, i) => sum +& i)
  val bruCnt = Wire(UInt(2.W)) 
  bruCnt := List.tabulate(robWidth)(i => (io.in(i).valid && io.in(i).bits.ctrl.fuType === FuType.bru && ALUOpType.isBru(io.in(i).bits.ctrl.fuOpType))).foldRight(0.U)((sum, i) => sum +& i)
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
    inst(i).brMask := DontCare
    // read rf, update src
    inst(i).decode.data.src1 := rf.read(rfSrc(2*i)) 
    when(rob.io.rvalid(2*i) && rob.io.rcommited(2*i)){inst(i).decode.data.src1 := rob.io.rprf(2*i)}
    inst(i).decode.data.src2 := rf.read(rfSrc(2*i + 1)) 
    when(rob.io.rvalid(2*i+1) && rob.io.rcommited(2*i+1)){inst(i).decode.data.src2 := rob.io.rprf(2*i+1)}
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
  when(io.in(0).bits.ctrl.isBlocked && io.in(0).fire){ blockReg := true.B }
  val mispredictionRecoveryReg = RegInit(false.B)
  when(io.redirect.valid && io.redirect.rtype === 1.U){ mispredictionRecoveryReg := true.B }
  when(rob.io.empty || flushBackend){ mispredictionRecoveryReg := false.B }
  val mispredictionRecovery = if(enableCheckpoint){
    io.redirect.valid && io.redirect.rtype === 1.U
  } else {
    mispredictionRecoveryReg && !rob.io.empty || io.redirect.valid && io.redirect.rtype === 1.U // waiting for misprediction recovery or misprediction detected
  }

  Debug(flushBackend, "flushbackend\n")
  Debug(io.redirect.valid && io.redirect.rtype === 1.U, "[REDIRECT] bpr start, redirect to %x\n", io.redirect.target)
  Debug(io.redirect.valid && io.redirect.rtype === 0.U, "[REDIRECT]special redirect to %x\n", io.redirect.target)

  instCango(0) := 
    io.in(0).valid &&
    rob.io.in(0).ready && // rob has empty slot
    !(hasBlockInst(0) && !pipeLineEmpty) &&
    !blockReg &&
    !mispredictionRecovery &&
    LookupTree(io.in(0).bits.ctrl.fuType, List(
      FuType.bru -> brurs.io.in.ready,
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
      FuType.bru -> (brurs.io.in.ready && (bruCnt < 2.U)),
      FuType.alu -> (alu2rs.io.in.ready),
      FuType.lsu -> (lsurs.io.in.ready && (lsuCnt < 2.U)),
      FuType.mdu -> (mdurs.io.in.ready && (mduCnt < 2.U)),
      FuType.csr -> (csrrs.io.in.ready && (csrCnt < 2.U)),
      FuType.mou -> (csrrs.io.in.ready && (csrCnt < 2.U))
    ))
  instCango(2) := false.B
  assert(!(instCango(1) && !instCango(0))) // insts must be dispatched in seq

  val noInst = 2.U
  val bruInst  = Mux(inst(0).decode.ctrl.fuType === FuType.bru, 0.U, Mux(inst(1).decode.ctrl.fuType === FuType.bru, 1.U, noInst))
  val alu1Inst = Mux(inst(0).decode.ctrl.fuType === FuType.alu, 0.U, noInst)
  val alu2Inst = Mux(inst(1).decode.ctrl.fuType === FuType.alu, 1.U, noInst)
  val csrInst  = Mux(inst(0).decode.ctrl.fuType === FuType.csr || inst(0).decode.ctrl.fuType === FuType.mou, 0.U, noInst)
  val lsuInst  = Mux(inst(0).decode.ctrl.fuType === FuType.lsu, 0.U, Mux(inst(1).decode.ctrl.fuType === FuType.lsu, 1.U, noInst))
  val mduInst  = Mux(inst(0).decode.ctrl.fuType === FuType.mdu, 0.U, Mux(inst(1).decode.ctrl.fuType === FuType.mdu, 1.U, noInst))

  def updateBrMask(brMask: UInt) = {
    brMask & ~ (UIntToOH(mispredictRec.checkpoint) & Fill(checkpointSize, mispredictRec.valid))
  }

  val brMaskReg = RegInit(0.U(checkpointSize.W))
  val brMaskGen = updateBrMask(brMaskReg)
  val brMask = Wire(Vec(robWidth+2, UInt(checkpointSize.W)))
  val isBranch = List.tabulate(robWidth)(i => io.in(i).valid && io.in(i).bits.ctrl.fuType === FuType.bru)
  brMask(0) := brMaskGen
  brMask(1) := brMaskGen | (UIntToOH(brurs.io.updateCheckpoint.get.bits) & Fill(checkpointSize, io.in(0).fire && isBranch(0)))
  brMask(2) := DontCare
  brMask(3) := brMask(1) | (UIntToOH(brurs.io.updateCheckpoint.get.bits) & Fill(checkpointSize, io.in(1).fire && isBranch(1)))
  brMaskReg := Mux(flushBackend, 0.U, Mux(io.redirect.valid && io.redirect.rtype === 1.U, updateBrMask(bruDelayer.io.out.bits.brMask), brMask(3)))

  Debug("[brMask] %d: old %x -> new %x\n", GTimer(), brMaskReg, Mux(flushBackend, 0.U, brMask(2)))

  val rs = List(brurs, alu1rs, alu2rs, csrrs, lsurs, mdurs)
  val rsInstSel = List(bruInst, alu1Inst, alu2Inst, csrInst, lsuInst, mduInst)
  List.tabulate(rs.length)(i => {
    rs(i).io.in.valid := instCango(rsInstSel(i))
    rs(i).io.in.bits := inst(rsInstSel(i)) 
    rs(i).io.in.bits.brMask := brMask(rsInstSel(i))
    rs(i).io.cdb <> cdb
    rs(i).io.flush := flushBackend
    rs(i).io.mispredictRec := mispredictRec
  })

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

  val bru = Module(new ALU(hasBru = true))
  val brucommit = Wire(new OOCommitIO)
  val brucommitdelayed = Wire(new OOCommitIO)
  val bruOut = bru.access(
    valid = brurs.io.out.valid, 
    src1 = brurs.io.out.bits.decode.data.src1, 
    src2 = brurs.io.out.bits.decode.data.src2, 
    func = brurs.io.out.bits.decode.ctrl.fuOpType
  )
  val bruWritebackReady = Wire(Bool())
  bru.io.cfIn := brurs.io.out.bits.decode.cf
  bru.io.offset := brurs.io.out.bits.decode.data.imm
  bru.io.out.ready := bruDelayer.io.in.ready
  brucommit.decode := brurs.io.out.bits.decode
  brucommit.isMMIO := false.B
  brucommit.intrNO := 0.U
  brucommit.commits := bruOut
  brucommit.prfidx := brurs.io.out.bits.prfDest
  brucommit.brMask := brurs.io.out.bits.brMask
  brucommit.decode.cf.redirect := bru.io.redirect
  brucommit.exception := false.B
  brucommit.store := false.B

  bruDelayer.io.in.bits := brucommit
  bruDelayer.io.in.valid := bru.io.out.valid
  bruDelayer.io.out.ready := bruWritebackReady
  bruDelayer.io.mispredictRec := mispredictRec
  bruDelayer.io.flush := io.flush
  bruDelayer.io.checkpointIn.get := brurs.io.recoverCheckpoint.get.bits
  brucommitdelayed := bruDelayer.io.out.bits

  // commit redirect
  bruRedirect := bruDelayer.io.out.bits.decode.cf.redirect
  bruRedirect.valid := bruDelayer.io.out.bits.decode.cf.redirect.valid && bruDelayer.io.out.fire
  mispredictRec.valid := bruDelayer.io.out.fire
  mispredictRec.checkpoint := bruDelayer.io.freeCheckpoint.get.bits
  mispredictRec.prfidx := bruDelayer.io.out.bits.prfidx
  mispredictRec.redirect := bruRedirect

  val alu1 = Module(new ALUEP())
  alu1rs.io.out <> alu1.io.in
  alu1.io.flush := io.flush
  alu1.io.mispredictRec := mispredictRec

  val alu2 = Module(new ALUEP())
  alu2rs.io.out <> alu2.io.in
  alu2.io.flush := io.flush
  alu2.io.mispredictRec := mispredictRec

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
  lsu.io.stMaskIn := lsurs.io.stMaskOut.get
  lsu.io.robAllocate.valid := io.in(0).fire
  lsu.io.robAllocate.bits := rob.io.index
  lsu.io.mispredictRec := mispredictRec
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
  lsucommit.exception := lsu.io.exceptionVec.asUInt.orR
  lsucommit.store := lsu.io.commitStoreToCDB
  lsucommit.brMask := DontCare // FIXIT: gen lsucommit in LSU
  // fix exceptionVec
  lsucommit.decode.cf.exceptionVec := lsu.io.exceptionVec

  // backend exceptions only come from LSU
  raiseBackendException := lsucommit.exception && lsu.io.out.fire
  // for backend exceptions, we reuse 'intrNO' field in ROB
  // when ROB.exception(x) === 1, intrNO(x) represents backend exception vec for this inst
  lsucommit.intrNO := lsucommit.decode.cf.exceptionVec.asUInt

  // NutShell MDU is not pipelined, we can not wrap it into "Execution Pipeline"
  // TODO: update MDU
  val mdu = Module(new MDU)
  val mducommit = Wire(new OOCommitIO)
  val mducommitdelayed = Wire(new OOCommitIO)
  val mduOut = mdu.access(
    valid = mdurs.io.out.valid, 
    src1 = mdurs.io.out.bits.decode.data.src1, 
    src2 = mdurs.io.out.bits.decode.data.src2, 
    func = mdurs.io.out.bits.decode.ctrl.fuOpType
  )
  val mduWritebackReady = Wire(Bool())
  mdu.io.out.ready := mduDelayer.io.in.ready
  mducommit.decode := mdurs.io.out.bits.decode
  mducommit.isMMIO := false.B
  mducommit.intrNO := 0.U
  mducommit.commits := mduOut
  mducommit.prfidx := mdurs.io.out.bits.prfDest
  mducommit.decode.cf.redirect.valid := false.B
  mducommit.decode.cf.redirect.rtype := DontCare
  mducommit.exception := false.B
  mducommit.store := false.B
  mducommit.brMask := mdurs.io.out.bits.brMask
  mdurs.io.commit.get := mdu.io.out.valid

  // assert(!(mdu.io.out.valid && !mduDelayer.io.in.ready))
  mduDelayer.io.in.bits := mducommit
  mduDelayer.io.in.valid := mdu.io.out.valid && mdurs.io.out.valid
  mduDelayer.io.out.ready := mduWritebackReady
  mduDelayer.io.mispredictRec := mispredictRec
  mduDelayer.io.flush := io.flush
  mducommitdelayed := mduDelayer.io.out.bits

  val csr = Module(new CSR)
  assert(!(csrrs.io.out.valid && csrrs.io.out.bits.decode.ctrl.fuType === FuType.csr && commitBackendException))
  val csrVaild = csrrs.io.out.valid && csrrs.io.out.bits.decode.ctrl.fuType === FuType.csr || commitBackendException
  val csrUop = WireInit(csrrs.io.out.bits)
  when(commitBackendException){
    csrUop := rob.io.beUop
  }
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
  csrcommit.store := false.B
  csrcommit.brMask := DontCare //FIXIT
  // fix wen
  when(csr.io.wenFix){csrcommit.decode.ctrl.rfWen := false.B}

  csr.io.imemMMU <> io.memMMU.imem
  csr.io.dmemMMU <> io.memMMU.dmem

  Debug(csrVaild && commitBackendException, "[BACKEND EXC] pc %x inst %x evec %b\n", csrUop.decode.cf.pc, csrUop.decode.cf.instr, csrUop.decode.cf.exceptionVec.asUInt)

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
  moucommit.store := false.B
  moucommit.brMask := DontCare //FIXIT

  // ------------------------------------------------
  // Backend stage 3+
  // Exec
  // ------------------------------------------------

  // ------------------------------------------------
  // Backend final stage
  // Commit to CDB (i.e. Writeback)
  // ------------------------------------------------

  // Common Data Bus
  // 
  // Currently, FUs can commit to any CDB socket.
  //  
  // Alternatively, FUs can be divided into different groups.
  // For each group, only one inst can be commited to ROB in a single cycle.

  val nullCommit = Wire(new OOCommitIO)
  nullCommit := DontCare

  // CDB arbit
  val (srcBRU, srcALU1, srcALU2, srcLSU, srcMDU, srcCSR, srcMOU, srcNone) = (0, 1, 2, 3, 4, 5, 6, 7)
  val commit = List(brucommitdelayed, alu1.io.out.bits, alu2.io.out.bits, lsucommit, mducommitdelayed, csrcommit, moucommit, nullCommit)
  val commitValid = List(bruDelayer.io.out.valid, alu1.io.out.valid, alu2.io.out.valid, lsu.io.out.valid, mduDelayer.io.out.valid, csr.io.out.valid, mou.io.out.valid, false.B)

  val WritebackPriority = Seq(
    srcCSR,
    srcMOU,
    srcLSU,
    srcMDU,
    srcBRU,
    srcALU1,
    srcALU2,
    srcNone
  )

  // select 2 CDB commit request with highest priority
  val commitPriority = VecInit(WritebackPriority.map(i => commit(i)))
  val commitValidPriority = VecInit(WritebackPriority.map(i => commitValid(i)))
  // val secondValidMask = VecInit((0 until WritebackPriority.size).map(i => WritebackPriority(0 until i).map(j => commitValid(j)).reduceLeft(_ ^ _)))
  val notFirstMask = Wire(Vec(WritebackPriority.size, Bool()))
  notFirstMask(0) := false.B
  for(i <- 0 until WritebackPriority.size){
    if(i != 0){notFirstMask(i) := notFirstMask(i-1) | commitValidPriority(i-1)}
  }
  val secondCommitValid = commitValidPriority.asUInt & notFirstMask.asUInt
  val notSecondMask = Wire(Vec(WritebackPriority.size, Bool()))
  notSecondMask(0) := false.B
  for(i <- 0 until WritebackPriority.size){
    if(i != 0){notSecondMask(i) := notSecondMask(i-1) | secondCommitValid(i-1)}
  }
  val commitValidVec = commitValidPriority.asUInt & ~notSecondMask.asUInt

  Debug("[CDB Arb] %b %b %b %b %b\n", commitValidPriority.asUInt, notFirstMask.asUInt, secondCommitValid, notSecondMask.asUInt, commitValidVec)

  val cdbSrc1 = PriorityMux(commitValidPriority, commitPriority)
  val cdbSrc1Valid = PriorityMux(commitValidPriority, commitValidPriority)
  val cdbSrc2 = PriorityMux(secondCommitValid, commitPriority)
  val cdbSrc2Valid = PriorityMux(secondCommitValid, commitValidPriority)

  val cmtStrHaz = List(
    PopCount(commitValidPriority.asUInt) === 0.U,
    PopCount(commitValidPriority.asUInt) === 1.U,
    PopCount(commitValidPriority.asUInt) === 2.U,
    PopCount(commitValidPriority.asUInt) === 3.U,
    PopCount(commitValidPriority.asUInt) > 3.U
  )
  val commitValidPriorityUInt = commitValidPriority.asUInt
  assert(!(PopCount(commitValidPriorityUInt(3,0)) > 2.U))

  cdb(0).valid := cdbSrc1Valid
  cdb(0).bits := cdbSrc1
  // cdb(0).ready := true.B
  cdb(1).valid := cdbSrc2Valid
  cdb(1).bits := cdbSrc2
  // cdb(1).ready := true.B

  mduWritebackReady  := commitValidVec(WritebackPriority.indexOf(srcMDU))
  bruWritebackReady  := commitValidVec(WritebackPriority.indexOf(srcBRU))
  alu1.io.out.ready := commitValidVec(WritebackPriority.indexOf(srcALU1))
  alu2.io.out.ready := commitValidVec(WritebackPriority.indexOf(srcALU2))

  brurs.io.out.ready  := bru.io.in.ready
  alu1rs.io.out.ready := alu1.io.in.ready
  alu2rs.io.out.ready := alu2.io.in.ready
  csrrs.io.out.ready := csr.io.in.ready
  lsurs.io.out.ready := lsu.io.in.ready
  mdurs.io.out.ready := mdu.io.in.ready

  Debug(flushBackend, "[FLUSH]\n")
  Debug(io.redirect.valid, "[RDIRECT] target 0x%x\n", io.redirect.target)

  // Performance Counter

  BoringUtils.addSource(alu1.io.out.fire, "perfCntCondMaluInstr")
  BoringUtils.addSource(bru.io.out.fire, "perfCntCondMbruInstr")
  BoringUtils.addSource(lsu.io.out.fire, "perfCntCondMlsuInstr")
  BoringUtils.addSource(mdu.io.out.fire, "perfCntCondMmduInstr")
  BoringUtils.addSource(!rob.io.in(0).ready, "perfCntCondMrobFull")
  BoringUtils.addSource(!alu1rs.io.in.ready, "perfCntCondMalu1rsFull")
  BoringUtils.addSource(!alu2rs.io.in.ready, "perfCntCondMalu2rsFull")
  BoringUtils.addSource(!brurs.io.in.ready, "perfCntCondMbrursFull")
  BoringUtils.addSource(!lsurs.io.in.ready, "perfCntCondMlsursFull")
  BoringUtils.addSource(!mdurs.io.in.ready, "perfCntCondMmdursFull")
  BoringUtils.addSource(lsurs.io.out.fire, "perfCntCondMlsuIssue")
  BoringUtils.addSource(mdurs.io.out.fire, "perfCntCondMmduIssue")
  BoringUtils.addSource(rob.io.empty, "perfCntCondMrobEmpty")
  BoringUtils.addSource(cmtStrHaz(0), "perfCntCondMcmtCnt0")
  BoringUtils.addSource(cmtStrHaz(1), "perfCntCondMcmtCnt1")
  BoringUtils.addSource(cmtStrHaz(2), "perfCntCondMcmtCnt2")
  BoringUtils.addSource(cmtStrHaz(3), "perfCntCondMcmtStrHaz1")
  BoringUtils.addSource(cmtStrHaz(4), "perfCntCondMcmtStrHaz2")
  BoringUtils.addSource(alu2.io.out.fire, "perfCntCondMaluInstr2")
  BoringUtils.addSource(!(rob.io.in(0).fire | rob.io.in(1).fire), "perfCntCondMdispatch0")
  BoringUtils.addSource(rob.io.in(0).fire ^ rob.io.in(1).fire, "perfCntCondMdispatch1")
  BoringUtils.addSource(rob.io.in(0).fire & rob.io.in(1).fire, "perfCntCondMdispatch2")

  val inst1RSfull = !LookupTree(io.in(0).bits.ctrl.fuType, List(
    FuType.bru -> brurs.io.in.ready,
    FuType.alu -> alu1rs.io.in.ready,
    FuType.lsu -> lsurs.io.in.ready,
    FuType.mdu -> mdurs.io.in.ready,
    FuType.csr -> csrrs.io.in.ready,
    FuType.mou -> csrrs.io.in.ready
  ))
  val inst2RSfull = !LookupTree(io.in(1).bits.ctrl.fuType, List(
    FuType.bru -> brurs.io.in.ready,
    FuType.alu -> alu2rs.io.in.ready,
    FuType.lsu -> lsurs.io.in.ready,
    FuType.mdu -> mdurs.io.in.ready,
    FuType.csr -> csrrs.io.in.ready,
    FuType.mou -> csrrs.io.in.ready
  ))
  val dispatchConflict = bruCnt > 1.U || lsuCnt > 1.U || mduCnt > 1.U || csrCnt > 1.U
  BoringUtils.addSource(io.in(0).valid && (hasBlockInst(0) && !pipeLineEmpty || blockReg), "perfCntCondMdp1StBlk")
  BoringUtils.addSource(io.in(0).valid && inst1RSfull, "perfCntCondMdp1StRSf")
  BoringUtils.addSource(io.in(0).valid && !rob.io.in(0).ready, "perfCntCondMdp1StROBf")
  BoringUtils.addSource(dispatchConflict, "perfCntCondMdp1StConf")
  BoringUtils.addSource(io.in(0).valid && !instCango(0), "perfCntCondMdp1StCnt")

  BoringUtils.addSource(io.in(1).valid && (hasBlockInst(0) || hasBlockInst(1) || blockReg), "perfCntCondMdp2StBlk")
  BoringUtils.addSource(io.in(1).valid && inst2RSfull, "perfCntCondMdp2StRSf")
  BoringUtils.addSource(io.in(1).valid && !rob.io.in(1).ready, "perfCntCondMdp2StROBf")
  BoringUtils.addSource(dispatchConflict, "perfCntCondMdp2StConf")
  BoringUtils.addSource(io.in(1).valid && !instCango(0), "perfCntCondMdp2StSeq")
  BoringUtils.addSource(io.in(1).valid && !instCango(1), "perfCntCondMdp2StCnt")
  BoringUtils.addSource(!io.in(0).valid, "perfCntCondMdpNoInst")

  if (!p.FPGAPlatform) {
    val difftestGpr = Module(new DifftestArchIntRegState)
    difftestGpr.io.clock  := clock
    difftestGpr.io.coreid := 0.U // TODO
    difftestGpr.io.gpr    := VecInit((0 to NRReg-1).map(i => rf.read(i.U)))
  }

  if (!p.FPGAPlatform) {
    val cycleCnt = WireInit(0.U(XLEN.W))
    val instrCnt = WireInit(0.U(XLEN.W))
    val nutcoretrap = csrrs.io.out.bits.decode.ctrl.isNutCoreTrap && csrrs.io.out.valid

    BoringUtils.addSink(cycleCnt, "simCycleCnt")
    BoringUtils.addSink(instrCnt, "simInstrCnt")
    BoringUtils.addSource(nutcoretrap, "nutcoretrap")

    val difftest = Module(new DifftestTrapEvent)
    difftest.io.clock    := clock
    difftest.io.coreid   := 0.U // TODO: nutshell does not support coreid auto config
    difftest.io.valid    := nutcoretrap
    difftest.io.code     := csrrs.io.out.bits.decode.data.src1
    difftest.io.pc       := csrrs.io.out.bits.decode.cf.pc
    difftest.io.cycleCnt := cycleCnt
    difftest.io.instrCnt := instrCnt
  }
  
}

class Backend_inorder(implicit val p: NutCoreConfig) extends NutCoreModule {
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

  PipelineConnect(isu.io.out, exu.io.in, exu.io.out.fire, io.flush(0))
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