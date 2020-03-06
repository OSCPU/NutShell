package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import bus.simplebus._

// Out Of Order Execution Backend 
class Backend(implicit val p: NOOPConfig) extends NOOPModule with HasRegFileParameter with HasROBConst{

  val io = IO(new Bundle {
    // EXU
    val in = Vec(2, Flipped(Decoupled(new DecodeIO)))
    val flush = Input(Bool())
    val dmem = new SimpleBusUC(addrBits = VAddrBits)
    val memMMU = Flipped(new MemMMUIO)

    // WBU
    val redirect = new RedirectIO
  })

  // For current version:
  // There is only 1 BRU (combined with ALU1)
  // There is only 1 LSU

  val DispatchWidth = 2
  val CommitWidth = 2
  val RetireWidth = 2

  val cdb = Wire(Vec(CommitWidth, Valid(new OOCommitIO)))
  val rf = new RegFile
  val rob = Module(new ROB)
  rob.io.cdb <> cdb
  rob.io.flush := io.flush
  when (rob.io.wb(0).rfWen) { rf.write(rob.io.wb(0).rfDest, rob.io.wb(0).rfData) }
  when (rob.io.wb(1).rfWen) { rf.write(rob.io.wb(1).rfDest, rob.io.wb(1).rfData) }

  val instCango = Wire(Vec(DispatchWidth + 1, Bool()))
  io.redirect <> rob.io.redirect
  List.tabulate(DispatchWidth)(i => {
    rob.io.in(i).valid := io.in(i).valid && instCango(i)
    io.in(i).ready := rob.io.in(i).ready && instCango(i)
    rob.io.in(i).bits := io.in(i).bits
  })

  val alu1rs = Module(new RS(name = "ALU1RS"))
  val alu2rs = Module(new RS(name = "ALU2RS"))
  val csrrs = Module(new RS(name = "CSRRS", size = 1)) // CSR & MOU
  val lsurs = Module(new RS(pipelined = true, name = "LSURS"))
  val mdurs = Module(new RS(pipelined = false, name = "MDURS"))

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
  val hasBlockInst = List.tabulate(DispatchWidth)(i => io.in(i).bits.ctrl.isBlocked)
  val pipeLineEmpty = rob.io.empty && alu1rs.io.empty && alu2rs.io.empty && csrrs.io.empty && lsurs.io.empty && mdurs.io.empty

  // Chicken Bit
  val Chicken = false
  if(Chicken){
    hasBlockInst(0) := true.B
    hasBlockInst(1) := true.B
  }

  instCango(0) := 
    io.in(0).valid &&
    rob.io.in(0).ready && // rob has empty slot
    !(hasBlockInst(0) && !pipeLineEmpty) &&
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

  alu1rs.io.flush := io.flush
  alu2rs.io.flush := io.flush
  csrrs.io.flush  := io.flush
  lsurs.io.flush  := io.flush
  mdurs.io.flush  := io.flush

  alu1rs.io.cdb <> cdb
  alu2rs.io.cdb <> cdb
  csrrs.io.cdb  <> cdb
  lsurs.io.cdb  <> cdb
  mdurs.io.cdb  <> cdb

  //TODO: send ls exception to csr

  List.tabulate(DispatchWidth)(i => {
    rob.io.in(i).valid := instCango(i)
    rob.io.in(i).bits := inst(i).decode
  })

  // ------------------------------------------------
  // Backend stage 2
  // Issue
  // ------------------------------------------------

  //TODO: FU template

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
  alu1commit.decode.cf.redirect := alu1.io.redirect

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

  val lsu = Module(new LSU)
  val lsucommit = Wire(new OOCommitIO)
  val lsuTlbPF = WireInit(false.B)
  
  val lsuValid = RegInit(false.B)
  val lsuFlush = RegInit(false.B)
  val lsuUop = RegEnable(lsurs.io.out.bits, lsurs.io.out.fire())
  when(lsu.io.out.fire()){ lsuValid := false.B }
  when(lsurs.io.out.fire()){ lsuValid := true.B }
  // when(io.flush){ lsuValid := false.B }
  when(io.flush && (lsuValid || lsurs.io.out.fire())){ lsuFlush := true.B }
  when(lsu.io.out.fire()){ lsuFlush := false.B }
  Debug(){
    printf("[RS LSUFSM INFO] lsuValid %x lsuFlush %x in %x out %x pc %x\n", lsuValid, lsuFlush, lsurs.io.out.fire(), lsu.io.out.fire(), lsuUop.decode.cf.pc)
  }

  val lsuOut = lsu.access(
    valid = lsuValid,
    src1 = lsuUop.decode.data.src1, 
    src2 = lsuUop.decode.data.imm, 
    func = lsuUop.decode.ctrl.fuOpType, 
    dtlbPF = lsuTlbPF
  )
  lsu.io.wdata := lsuUop.decode.data.src2
  lsu.io.instr := lsuUop.decode.cf.instr
  io.dmem <> lsu.io.dmem
  lsu.io.out.ready := true.B //TODO
  lsucommit.decode := lsuUop.decode
  lsucommit.isMMIO := lsu.io.isMMIO || (AddressSpace.isMMIO(lsuUop.decode.cf.pc) && lsuValid)
  lsucommit.intrNO := 0.U
  lsucommit.commits := lsuOut
  lsucommit.prfidx := lsuUop.prfDest

  // val lsuOut = lsu.access(
  //   valid = lsurs.io.out.valid, 
  //   src1 = lsurs.io.out.bits.decode.data.src1, 
  //   src2 = lsurs.io.out.bits.decode.data.imm, 
  //   func = lsurs.io.out.bits.decode.ctrl.fuOpType, 
  //   dtlbPF = lsuTlbPF
  // )
  // lsu.io.wdata := lsurs.io.out.bits.decode.data.src2
  // lsu.io.instr := lsurs.io.out.bits.decode.cf.instr
  // io.dmem <> lsu.io.dmem
  // lsu.io.out.ready := true.B //TODO
  // lsucommit.decode := lsurs.io.out.bits.decode
  // lsucommit.isMMIO := lsu.io.isMMIO || (AddressSpace.isMMIO(lsurs.io.out.bits.decode.cf.pc) && lsurs.io.out.valid)
  // lsucommit.intrNO := 0.U
  // lsucommit.commits := lsuOut
  // lsucommit.prfidx := lsurs.io.out.bits.prfDest

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
  mdurs.io.commit.get := mdu.io.out.fire()

  val csr = Module(new CSR)
  val csrcommit = Wire(new OOCommitIO)
  val csrOut = csr.access(
    valid = csrrs.io.out.valid && lsurs.io.out.bits.decode.ctrl.fuType === FuType.csr, 
    src1 = csrrs.io.out.bits.decode.data.src1, 
    src2 = csrrs.io.out.bits.decode.data.src2, 
    func = csrrs.io.out.bits.decode.ctrl.fuOpType
  )
  csr.io.cfIn := csrrs.io.out.bits.decode.cf
  csr.io.cfIn.exceptionVec(loadAddrMisaligned) := lsu.io.loadAddrMisaligned
  csr.io.cfIn.exceptionVec(storeAddrMisaligned) := lsu.io.storeAddrMisaligned
  csr.io.instrValid := (lsurs.io.out.valid && csrrs.io.out.valid) && !io.flush //TODO: LSU valid
  csr.io.out.ready := true.B
  csrcommit.decode := csrrs.io.out.bits.decode
  csrcommit.isMMIO := false.B
  csrcommit.intrNO := csr.io.intrNO
  csrcommit.commits := csrOut
  csrcommit.prfidx := csrrs.io.out.bits.prfDest
  // fix wen
  when(csr.io.wenFix){csrcommit.decode.ctrl.rfWen := false.B}

  csr.io.imemMMU <> io.memMMU.imem
  csr.io.dmemMMU <> io.memMMU.dmem

  val mou = Module(new MOU)
  val moucommit = Wire(new OOCommitIO)
  // mou does not write register
  mou.access(
    valid = csrrs.io.out.valid && lsurs.io.out.bits.decode.ctrl.fuType === FuType.mou, 
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

  // ------------------------------------------------
  // Backend stage 3+
  // Exec
  // ------------------------------------------------

  // ------------------------------------------------
  // Backend final stage
  // Commit
  // ------------------------------------------------

  // CDB arbit
  val (srcALU1, srcALU2, srcLSU, srcMDU, srcCSR, srcMOU) = (0, 1, 2, 3, 4, 5)
  val commit = VecInit(List(alu1commit, alu2commit, lsucommit, mducommit, csrcommit, moucommit))
  val commitValid = VecInit(List(alu1.io.out.valid, alu2.io.out.valid, lsu.io.out.valid && !lsuFlush, mdu.io.out.valid && mdurs.io.out.valid, csr.io.out.valid, mou.io.out.valid))

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
  // lsurs.io.out.ready := lsu.io.in.ready //FIXME after update LSU
  lsurs.io.out.ready := !lsuValid || lsu.io.out.fire() //FIXME after update LSU
  mdurs.io.out.ready := mdu.io.in.ready

  Debug(){when(io.flush){printf("[FLUSH] TIMER: %d\n", GTimer())}}
  Debug(){when(io.redirect.valid){printf("[RDIRECT] TIMER: %d target 0x%x\n", GTimer(), io.redirect.target)}}

  // Performance Counter

  val isBru = ALUOpType.isBru(alu1commit.decode.ctrl.fuOpType)
  BoringUtils.addSource(alu1.io.out.fire() && !isBru, "perfCntCondMaluInstr") //TODO: Fix it
  BoringUtils.addSource(alu1.io.out.fire() && isBru, "perfCntCondMbruInstr") //TODO: Fix it
  BoringUtils.addSource(lsu.io.out.fire(), "perfCntCondMlsuInstr")
  BoringUtils.addSource(mdu.io.out.fire(), "perfCntCondMmduInstr")
  BoringUtils.addSource(csr.io.out.fire(), "perfCntCondMcsrInstr")

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
