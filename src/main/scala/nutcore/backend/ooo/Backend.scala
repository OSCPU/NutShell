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

trait HasBackendConst{
  // val multiIssue = true
  val robSize = 16
  val robWidth = 2
  val robInstCapacity = robSize * robWidth
  val checkpointSize = 4 // register map checkpoint size
  val brTagWidth = log2Up(checkpointSize)
  val prfAddrWidth = log2Up(robSize) + log2Up(robWidth) // physical rf addr width

  val DispatchWidth = 2
  val WritebackWidth = 2
  val CommitWidth = 2
}

// NutShell/Argo Out Of Order Execution Backend
class Backend(implicit val p: NutCoreConfig) extends NutCoreModule with HasRegFileParameter with HasBackendConst{

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

  val cdb = Wire(Vec(WritebackWidth, Valid(new OOCommitIO)))
  val rf = new RegFile
  val rob = Module(new ROB)

  val brurs  = Module(new RS(priority = true, size = checkpointSize, checkpoint = true, name = "BRURS"))
  val alu1rs = Module(new RS(priority = true, size = 4, name = "ALU1RS"))
  val alu2rs = Module(new RS(priority = true, size = 4, name = "ALU2RS"))
  val csrrs  = Module(new RS(priority = true, size = 1, name = "CSRRS")) // CSR & MOU
  val lsurs  = Module(new RS(storeSeq = true, size = 4, name = "LSURS")) // FIXIT: out of order l/s disabled
  val mdurs  = Module(new RS(priority = true, size = 4, pipelined = false, name = "MDURS"))

  val rs = List(brurs, alu1rs, alu2rs, csrrs, lsurs, mdurs)

  val bru = Module(new BRUEP())
  val alu1 = Module(new ALUEP())
  val alu2 = Module(new ALUEP())
  val csr = Module(new CSREP)
  val lsu = Module(new LSUEP)
  val mdu = Module(new MDUEP)

  val fu = List(bru, alu1, alu2, csr, lsu, mdu)

  val instCango = Wire(Vec(DispatchWidth + 1, Bool()))
  val bruRedirect = Wire(new RedirectIO)
  val mispredictRec = Wire(new MisPredictionRecIO)

  io.redirect := Mux(rob.io.redirect.valid && rob.io.redirect.rtype === 0.U, rob.io.redirect, bruRedirect)

  rob.io.cdb <> cdb
  rob.io.mispredictRec := mispredictRec
  rob.io.flush := io.flush
  when (rob.io.wb(0).rfWen) { rf.write(rob.io.wb(0).rfDest, rob.io.wb(0).rfData) }
  when (rob.io.wb(1).rfWen) { rf.write(rob.io.wb(1).rfDest, rob.io.wb(1).rfData) }
  List.tabulate(DispatchWidth)(i => {
    rob.io.in(i).valid := io.in(i).valid && instCango(i)
    io.in(i).ready := rob.io.in(i).ready && instCango(i)
    rob.io.in(i).bits := io.in(i).bits
  })

  brurs.io.updateCheckpoint.get <> rob.io.updateCheckpoint
  rob.io.recoverCheckpoint.bits := bru.bruio.freeCheckpoint.bits
  brurs.io.freeCheckpoint.get <> bru.bruio.freeCheckpoint
  rob.io.recoverCheckpoint.valid := io.redirect.valid && io.redirect.rtype === 1.U

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
    List.tabulate(WritebackWidth)(j => {
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
  when((rob.io.empty || io.flush) && !haveUnfinishedStore){ blockReg := false.B }
  when(io.in(0).bits.ctrl.isBlocked && io.in(0).fire()){ blockReg := true.B }
  val mispredictionRecovery = io.redirect.valid && io.redirect.rtype === 1.U

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
  brMask(1) := brMaskGen | (UIntToOH(brurs.io.updateCheckpoint.get.bits) & Fill(checkpointSize, io.in(0).fire() && isBranch(0)))
  brMask(2) := DontCare
  brMask(3) := brMask(1) | (UIntToOH(brurs.io.updateCheckpoint.get.bits) & Fill(checkpointSize, io.in(1).fire() && isBranch(1)))
  brMaskReg := Mux(io.flush, 0.U, Mux(io.redirect.valid && io.redirect.rtype === 1.U, updateBrMask(bru.io.out.bits.brMask), brMask(3)))

  val rsInstSel = List(bruInst, alu1Inst, alu2Inst, csrInst, lsuInst, mduInst)
  List.tabulate(rs.length)(i => {
    rs(i).io.in.valid := instCango(rsInstSel(i))
    rs(i).io.in.bits := inst(rsInstSel(i)) 
    rs(i).io.in.bits.brMask := brMask(rsInstSel(i))
    rs(i).io.cdb <> cdb
    rs(i).io.flush := io.flush
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

  val commitBackendException = WireInit(false.B)

  commitBackendException := rob.io.exception

  // Function Units Wiring
  (fu zip rs).map{ case (fu, rs) => {
    rs.io.out <> fu.io.in
    fu.io.flush := io.flush
    fu.io.mispredictRec := mispredictRec
  }}

  // Special Wiring
  // BRU
  bru.bruio.recoverCheckpoint := brurs.io.recoverCheckpoint.get
  mispredictRec := bru.bruio.mispredictRec
  bruRedirect := bru.bruio.bruRedirect
  // LSU
  lsu.lsuio.stMaskIn := lsurs.io.stMaskOut.get
  lsu.lsuio.robAllocate.valid := io.in(0).fire()
  lsu.lsuio.robAllocate.bits := rob.io.index
  lsu.lsuio.scommit := rob.io.scommit
  io.dmem <> lsu.lsuio.dmem
  io.dtlb <> lsu.lsuio.dtlb
  haveUnfinishedStore := lsu.lsuio.haveUnfinishedStore
  BoringUtils.addSource(io.memMMU.dmem.loadPF, "loadPF") // FIXIT: this is nasty
  BoringUtils.addSource(io.memMMU.dmem.storePF, "storePF") // FIXIT: this is nasty
  // MDU
  // TODO: update MDU
  mdurs.io.commit.get := mdu.mduio.mdufinish
  // CSR
  csr.csrio.isBackendException := commitBackendException
  csr.csrio.memMMU <> io.memMMU
  // Override CSR in
  assert(!(csrrs.io.out.valid && csrrs.io.out.bits.decode.ctrl.fuType === FuType.csr && commitBackendException))
  csr.io.in.valid := csrrs.io.out.valid || commitBackendException
  csr.io.in.bits := Mux(commitBackendException, rob.io.beUop, csrrs.io.out.bits)

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

  require(WritebackWidth == 2 || WritebackWidth == 4)

  if(WritebackWidth == 2){
    val nullCommit = Wire(new OOCommitIO)
    nullCommit := DontCare

    // CDB arbit
    val (srcBRU, srcALU1, srcALU2, srcLSU, srcMDU, srcCSR, srcNone) = (0, 1, 2, 3, 4, 5, 6)
    val commit = List(bru.io.out.bits, alu1.io.out.bits, alu2.io.out.bits, lsu.io.out.bits, mdu.io.out.bits, csr.io.out.bits, nullCommit)
    val commitValid = List(bru.io.out.valid, alu1.io.out.valid, alu2.io.out.valid, lsu.io.out.valid, mdu.io.out.valid, csr.io.out.valid, false.B)

    val WritebackPriority = Seq(
      srcCSR,
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
    assert(!(PopCount(commitValidPriorityUInt(2,0)) > 2.U))

    cdb(0).valid := cdbSrc1Valid
    cdb(0).bits := cdbSrc1
    cdb(1).valid := cdbSrc2Valid
    cdb(1).bits := cdbSrc2

    csr.io.out.ready  := true.B
    lsu.io.out.ready  := true.B // Always permit lsu writeback to reduce load-to-use delay
    mdu.io.out.ready  := commitValidVec(WritebackPriority.indexOf(srcMDU))
    bru.io.out.ready  := commitValidVec(WritebackPriority.indexOf(srcBRU))
    alu1.io.out.ready := commitValidVec(WritebackPriority.indexOf(srcALU1))
    alu2.io.out.ready := commitValidVec(WritebackPriority.indexOf(srcALU2))
  }

  if(WritebackWidth == 4){
    cdb(0).valid := bru.io.out.valid
    cdb(0).bits := bru.io.out.bits
    cdb(1).valid := lsu.io.out.valid
    cdb(1).bits := lsu.io.out.bits
    cdb(2).valid := alu1.io.out.valid
    cdb(2).bits := alu1.io.out.bits
    cdb(3).valid := csr.io.out.valid || alu2.io.out.valid || mdu.io.out.valid
    cdb(3).bits := PriorityMux(
      List(
        csr.io.out.valid -> csr.io.out.bits,
        mdu.io.out.valid -> mdu.io.out.bits,
        alu2.io.out.valid -> alu2.io.out.bits
      )
    )

    mdu.io.out.ready  := true.B
    bru.io.out.ready  := true.B
    alu1.io.out.ready := true.B
    alu2.io.out.ready := !(csr.io.out.valid || mdu.io.out.valid)
  }

  brurs.io.out.ready  := bru.io.in.ready
  alu1rs.io.out.ready := alu1.io.in.ready
  alu2rs.io.out.ready := alu2.io.in.ready
  csrrs.io.out.ready := csr.io.in.ready
  lsurs.io.out.ready := lsu.io.in.ready
  mdurs.io.out.ready := mdu.io.in.ready

  Debug(io.redirect.valid, "[RDIRECT] target 0x%x\n", io.redirect.target)

  // Performance Counter

  BoringUtils.addSource(alu1.io.out.fire(), "perfCntCondMaluInstr")
  BoringUtils.addSource(bru.io.out.fire(), "perfCntCondMbruInstr")
  BoringUtils.addSource(lsu.io.out.fire(), "perfCntCondMlsuInstr")
  BoringUtils.addSource(mdu.io.out.fire(), "perfCntCondMmduInstr")
  BoringUtils.addSource(!rob.io.in(0).ready, "perfCntCondMrobFull")
  BoringUtils.addSource(!alu1rs.io.in.ready, "perfCntCondMalu1rsFull")
  BoringUtils.addSource(!alu2rs.io.in.ready, "perfCntCondMalu2rsFull")
  BoringUtils.addSource(!brurs.io.in.ready, "perfCntCondMbrursFull")
  BoringUtils.addSource(!lsurs.io.in.ready, "perfCntCondMlsursFull")
  BoringUtils.addSource(!mdurs.io.in.ready, "perfCntCondMmdursFull")
  BoringUtils.addSource(lsurs.io.out.fire(), "perfCntCondMlsuIssue")
  BoringUtils.addSource(mdurs.io.out.fire(), "perfCntCondMmduIssue")
  BoringUtils.addSource(rob.io.empty, "perfCntCondMrobEmpty")
  // BoringUtils.addSource(cmtStrHaz(0), "perfCntCondMcmtCnt0")
  // BoringUtils.addSource(cmtStrHaz(1), "perfCntCondMcmtCnt1")
  // BoringUtils.addSource(cmtStrHaz(2), "perfCntCondMcmtCnt2")
  // BoringUtils.addSource(cmtStrHaz(3), "perfCntCondMcmtStrHaz1")
  // BoringUtils.addSource(cmtStrHaz(4), "perfCntCondMcmtStrHaz2")
  BoringUtils.addSource(alu2.io.out.fire(), "perfCntCondMaluInstr2")
  BoringUtils.addSource(!(rob.io.in(0).fire() | rob.io.in(1).fire()), "perfCntCondMdispatch0")
  BoringUtils.addSource(rob.io.in(0).fire() ^ rob.io.in(1).fire(), "perfCntCondMdispatch1")
  BoringUtils.addSource(rob.io.in(0).fire() & rob.io.in(1).fire(), "perfCntCondMdispatch2")

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
    BoringUtils.addSource(VecInit((0 to NRReg-1).map(i => rf.read(i.U))), "DIFFTEST_r")
  }

  if (!p.FPGAPlatform) {
    val mon = Module(new Monitor)
    val cycleCnt = WireInit(0.U(XLEN.W))
    val instrCnt = WireInit(0.U(XLEN.W))
    val nutcoretrap = csrrs.io.out.bits.decode.ctrl.isNutCoreTrap && csrrs.io.out.valid
    mon.io.clk := clock
    mon.io.reset := reset.asBool
    mon.io.isNutCoreTrap := nutcoretrap
    mon.io.trapCode := csrrs.io.out.bits.decode.data.src1
    mon.io.trapPC := csrrs.io.out.bits.decode.cf.pc
    mon.io.cycleCnt := cycleCnt
    mon.io.instrCnt := instrCnt

    BoringUtils.addSink(cycleCnt, "simCycleCnt")
    BoringUtils.addSink(instrCnt, "simInstrCnt")
    BoringUtils.addSource(nutcoretrap, "nutcoretrap")
  }
  
}
