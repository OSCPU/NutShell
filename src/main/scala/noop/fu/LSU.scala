package noop
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import bus.simplebus._

// Out Of Order Load/Store Unit

object LSUOpType { //TODO: refactor LSU fuop
  def lb   = "b0000000".U
  def lh   = "b0000001".U
  def lw   = "b0000010".U
  def ld   = "b0000011".U
  def lbu  = "b0000100".U
  def lhu  = "b0000101".U
  def lwu  = "b0000110".U
  def sb   = "b0001000".U
  def sh   = "b0001001".U
  def sw   = "b0001010".U
  def sd   = "b0001011".U

  def lr      = "b0100000".U
  def sc      = "b0100001".U
  def amoswap = "b0100010".U
  def amoadd  = "b1100011".U
  def amoxor  = "b0100100".U
  def amoand  = "b0100101".U
  def amoor   = "b0100110".U
  def amomin  = "b0110111".U
  def amomax  = "b0110000".U
  def amominu = "b0110001".U
  def amomaxu = "b0110010".U
  
  def isAdd(func: UInt) = func(6)
  def isAtom(func: UInt): Bool = func(5)
  def isStore(func: UInt): Bool = func(3)
  def isLoad(func: UInt): Bool = !isStore(func) & !isAtom(func)
  def isLR(func: UInt): Bool = func === lr
  def isSC(func: UInt): Bool = func === sc
  def isAMO(func: UInt): Bool = isAtom(func) && !isLR(func) && !isSC(func)

  def needMemWrite(func: UInt): Bool = isStore(func) || isAMO(func) || isSC(func)

  def atomW = "010".U
  def atomD = "011".U
}

object MEMOpID {
  def idle   = "b0000_000".U
  def load   = "b0001_001".U
  def store  = "b0001_010".U
  def storec = "b0010_010".U //store commit
  def amo    = "b0001_111".U
  def lr     = "b0001_101".U
  def sc     = "b0001_110".U
  def tlb    = "b0100_001".U
  def vload  = "b1000_001".U
  def vstore = "b1000_010".U

  def needLoad(memop: UInt) = memop(0)
  def needStore(memop: UInt) = memop(1)
  def needAlu(memop: UInt) = memop(2)
  def commitToCDB(memop: UInt) = memop(3)
  def commitToSTQ(memop: UInt) = memop(4)
  def commitToTLB(memop: UInt) = memop(5)
  def commitToVPU(memop: UInt) = memop(6)
}

trait HasLSUConst {
  val IndependentAddrCalcState = false
  val loadQueueSize = 8
  val storeQueueSize = 8
}

class LSUIO extends FunctionUnitIO {
  val wdata = Input(UInt(XLEN.W))
  // val instr = Input(UInt(32.W)) // Atom insts need aq rl funct3 bit from instr // TODO
  val dmem = new SimpleBusUC(addrBits = VAddrBits, userBits = DCacheUserBundleWidth)
  val dtlb = new SimpleBusUC(addrBits = VAddrBits, userBits = DCacheUserBundleWidth)
  val cdb = Vec(robWidth, Flipped(Valid(new OOCommitIO)))
  val brMaskIn = Input(UInt(robInstCapacity.W))
  val uopIn = Input(new RenamedDecodeIO)
  val uopOut = Output(new RenamedDecodeIO)
  val isMMIO = Output(Bool())
  val exceptionVec = Output(Vec(16, Bool()))
  val scommit = Input(Bool())
  val atomData = Output(UInt(XLEN.W))
  val haveUnfinishedStore = Output(Bool())
  val flush = Input(Bool())
}

class StoreQueueEntry extends NOOPBundle{
  val pc       = UInt(VAddrBits.W)
  val prfidx   = UInt(prfAddrWidth.W) // for debug
  val brMask   = UInt(robInstCapacity.W)
  val wmask    = UInt((XLEN/8).W) // for store queue forwarding
  val vaddr    = UInt(VAddrBits.W)
  val paddr    = UInt(PAddrBits.W)
  val func     = UInt(7.W)
  val size     = UInt(2.W)
  val op       = UInt(7.W)
  val data     = UInt(XLEN.W)
  val isMMIO   = Bool()
  val valid    = Bool()
}

class LoadQueueEntry extends NOOPBundle{
  val pc       = UInt(VAddrBits.W)
  val prfidx   = UInt(prfAddrWidth.W)
  val brMask   = UInt(robInstCapacity.W)
  val vaddr    = UInt(VAddrBits.W) // for debug
  val paddr    = UInt(PAddrBits.W)
  val func     = UInt(7.W)
  val size     = UInt(2.W)
  val op       = UInt(7.W)
  val data     = UInt(XLEN.W)
  val fdata    = UInt(XLEN.W) // forwarding data
  val fmask    = UInt((XLEN/8).W) // forwarding mask
  val asrc     = UInt(XLEN.W) // alusrc2 for atom inst
  val rfWen    = Bool()
  val isMMIO   = Bool()
  val valid    = Bool()
  val tlbfin   = Bool()
  val finished = Bool()
  val loadPageFault  = Bool()
  val storePageFault  = Bool()
  val loadAddrMisaligned  = Bool()
  val storeAddrMisaligned = Bool()
}

class DCacheUserBundle extends NOOPBundle {
  val ldqidx   = UInt(5.W) //TODO
  val op       = UInt(7.W)
}

class MemReq extends NOOPBundle {
  val addr = UInt(VAddrBits.W)
  val size = UInt(2.W)
  val wdata = UInt(XLEN.W)
  val wmask = UInt(8.W)
  val cmd = UInt(4.W)
  val user = new DCacheUserBundle
  val valid = Bool()
}

class AtomALU extends NOOPModule {
  val io = IO(new NOOPBundle{
    val src1 = Input(UInt(XLEN.W))
    val src2 = Input(UInt(XLEN.W))
    val func = Input(UInt(7.W))
    val isWordOp = Input(Bool())
    val result = Output(UInt(XLEN.W))
  })

  // src1: load result
  // src2: reg  result
  val src1 = io.src1
  val src2 = io.src2
  val func = io.func
  val isAdderSub = !LSUOpType.isAdd(func) 
  val adderRes = (src1 +& (src2 ^ Fill(XLEN, isAdderSub))) + isAdderSub
  val xorRes = src1 ^ src2
  val sltu = !adderRes(XLEN)
  val slt = xorRes(XLEN-1) ^ sltu

  val res = LookupTreeDefault(func(5, 0), adderRes, List(
    LSUOpType.amoswap -> src2,
    // LSUOpType.amoadd  -> adderRes,
    LSUOpType.amoxor  -> xorRes,
    LSUOpType.amoand  -> (src1 & src2),
    LSUOpType.amoor   -> (src1 | src2),
    LSUOpType.amomin  -> Mux(slt(0), src1, src2),
    LSUOpType.amomax  -> Mux(slt(0), src2, src1),
    LSUOpType.amominu -> Mux(sltu(0), src1, src2),
    LSUOpType.amomaxu -> Mux(sltu(0), src2, src1)
  ))

  io.result :=  Mux(io.isWordOp, SignExt(res(31,0), 64), res)
}

// Out Of Order Load/Store Unit
class LSU extends NOOPModule with HasLSUConst {
  val io = IO(new LSUIO)
  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt, dtlbPF: Bool): UInt = {
    this.valid := valid && !needMispredictionRecovery(io.brMaskIn)
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  def needMispredictionRecovery(brMask: UInt) = {
    List.tabulate(CommitWidth)(i => (io.cdb(i).bits.decode.cf.redirect.valid && (io.cdb(i).bits.decode.cf.redirect.rtype === 1.U) && brMask(io.cdb(i).bits.prfidx))).foldRight(false.B)((sum, i) => sum | i)
  }

  def updateBrMask(brMask: UInt) = {
    brMask & ~ List.tabulate(CommitWidth)(i => (UIntToOH(io.cdb(i).bits.prfidx) & Fill(robInstCapacity, io.cdb(i).valid))).foldRight(0.U)((sum, i) => sum | i)
  }

  val dmem = io.dmem
  // Gen result
  val tlbRespLdqidx = io.dtlb.resp.bits.user.get.asTypeOf(new DCacheUserBundle).ldqidx
  val dmemUserOut = dmem.resp.bits.user.get.asTypeOf(new DCacheUserBundle)
  val opResp = dmem.resp.bits.user.get.asTypeOf(new DCacheUserBundle).op
  val opReq = dmem.req.bits.user.get.asTypeOf(new DCacheUserBundle).op
  val ldqidxResp = dmemUserOut.ldqidx
  val branchIndex = Mux(
    io.cdb(0).valid && io.cdb(0).bits.decode.cf.redirect.valid && io.cdb(0).bits.decode.cf.redirect.rtype === 1.U,
    io.cdb(0).bits.prfidx,
    io.cdb(1).bits.prfidx
  )
  val bruFlush = List.tabulate(CommitWidth)(i => 
    io.cdb(i).bits.decode.cf.redirect.valid && io.cdb(i).bits.decode.cf.redirect.rtype === 1.U
  ).reduce(_ | _)

  // Decode
  val instr    = io.uopIn.decode.cf.instr
  val storeReq = valid & LSUOpType.isStore(func)
  val loadReq  = valid & LSUOpType.isLoad(func)
  val atomReq  = valid & LSUOpType.isAtom(func)
  val amoReq   = valid & LSUOpType.isAMO(func)
  val lrReq   = valid & LSUOpType.isLR(func)
  val scReq   = valid & LSUOpType.isSC(func)
  val aq = instr(26)
  val rl = instr(25)
  val funct3 = instr(14, 12)
  val atomWidthW = !funct3(0)
  val atomWidthD = funct3(0)
  val findLoadAddrMisaligned = Wire(Bool())
  val findStoreAddrMisaligned = Wire(Bool())

  // Atom LR/SC Control Bits
  val setLr = Wire(Bool())
  val setLrVal = Wire(Bool())
  val setLrAddr = Wire(UInt(AddrBits.W))
  val lr = WireInit(Bool(), false.B)
  val lrAddr = WireInit(UInt(AddrBits.W), DontCare)
  BoringUtils.addSource(setLr, "set_lr")
  BoringUtils.addSource(setLrVal, "set_lr_val")
  BoringUtils.addSource(setLrAddr, "set_lr_addr")
  BoringUtils.addSink(lr, "lr")
  BoringUtils.addSink(lrAddr, "lr_addr")
  val scInvalid = !(src1 === lrAddr) && scReq
  setLr := (lrReq || scReq) && io.in.fire()
  setLrVal := lrReq
  setLrAddr := src1

  // PF signal from TLB
  // TODO: add a TLB bus instead of using BoringUtils.addSink/Src
  val dtlbFinish = WireInit(false.B)
  val dtlbPF = WireInit(false.B)
  val dtlbEnable = WireInit(false.B)
  BoringUtils.addSink(dtlbFinish, "DTLBFINISH")
  BoringUtils.addSink(dtlbPF, "DTLBPF")
  BoringUtils.addSink(dtlbEnable, "DTLBENABLE")

  val addr = Mux(atomReq || lrReq || scReq, src1, src1 + src2)
  val data = io.uopIn.decode.data.src2
  val size = Mux(LSUOpType.isAtom(func), Mux(atomWidthW, "b10".U, "b11".U), func(1,0))
  val memop = Wire(UInt(7.W))
  memop := Cat(
    false.B, // commitToVPU
    false.B, // commitToTLB
    false.B, // commitToSTQ
    io.in.valid, // commitToCDB
    amoReq, // needAlu
    (storeReq || amoReq || scReq && !scInvalid), // needStore
    (loadReq || amoReq || lrReq) // needLoad
  )
  // If a LSU inst triggers an exception, it has no difference compared with other normal insts 
  // except it will be canceled by redirect bit

  // L/S Queue

  val storeQueueEnqueue = Wire(Bool())

  //                           Load Queue
  // ---------------------------------------------------------------------------
  // |   not used   |   waittlb    |   waitmem    |   dmemreq   |   not used   |
  // ---------------------------------------------------------------------------
  //                |              |              |             |
  //              head            tlb            mem           tail

  val loadQueue = RegInit(VecInit(Seq.fill(loadQueueSize)(0.U.asTypeOf(new LoadQueueEntry)))) // TODO: replace it with utils.queue
  // Load Queue contains Load, Store and TLB request
  // Store insts will access TLB before its result being commited to CDB
  // loadQueue should be called 'loadStoreQueue' or 'memReOrderQueue', in some ways
  val loadHeadPtr  = RegInit(0.U((log2Up(loadQueueSize)).W))
  val loadDtlbPtr  = RegInit(0.U((log2Up(loadQueueSize)).W))
  val loadDmemPtr  = RegInit(0.U((log2Up(loadQueueSize)).W))
  val loadTailPtr  = RegInit(0.U((log2Up(loadQueueSize)).W))
  val loadQueueFull = loadHeadPtr === (loadTailPtr - 1.U) //TODO: fix it with maybe_full logic
  val loadQueueEmpty = loadHeadPtr === loadTailPtr //TODO: fix it with maybe_full logic
  val havePendingDtlbReq = loadDtlbPtr =/= loadHeadPtr
  val havePendingDmemReq = MEMOpID.needLoad(loadQueue(loadDmemPtr).op) && !loadQueue(loadDmemPtr).loadPageFault && !loadQueue(loadDmemPtr).storePageFault && !loadQueue(loadDmemPtr).loadAddrMisaligned && !loadQueue(loadDmemPtr).storeAddrMisaligned && !loadQueue(loadDmemPtr).finished && loadQueue(loadDmemPtr).valid && loadQueue(loadDmemPtr).tlbfin
  val havePendingStoreEnq = MEMOpID.needStore(loadQueue(loadDmemPtr).op) && !MEMOpID.needAlu(loadQueue(loadDmemPtr).op) && loadQueue(loadDmemPtr).valid && loadQueue(loadDmemPtr).tlbfin
  val havePendingAMOStoreEnq = MEMOpID.needAlu(loadQueue(loadDmemPtr).op) && loadQueue(loadDmemPtr).valid && loadQueue(loadDmemPtr).tlbfin
  val dmemReqFromLoadQueue = havePendingDmemReq || havePendingStoreEnq || havePendingAMOStoreEnq
  val skipAInst = !loadQueue(loadDmemPtr).valid && loadDmemPtr =/= loadDtlbPtr || loadQueue(loadDmemPtr).valid && loadQueue(loadDmemPtr).tlbfin && (loadQueue(loadDmemPtr).loadPageFault || loadQueue(loadDmemPtr).storePageFault || loadQueue(loadDmemPtr).loadAddrMisaligned || loadQueue(loadDmemPtr).storeAddrMisaligned || !loadQueue(loadDmemPtr).op(2,0).orR)
  val haveLoadResp = io.dmem.resp.fire() && MEMOpID.commitToCDB(opResp) && loadQueue(ldqidxResp).valid //FIXIT: to use non blocking dcache, set it to false
  val havePendingCDBCmt = (0 until loadQueueSize).map(i => loadQueue(i).finished && loadQueue(i).valid).reduce(_ | _)
  val pendingCDBCmtSelect = PriorityEncoder(VecInit((0 until loadQueueSize).map(i => loadQueue(i).finished && loadQueue(i).valid)))
  val writebackSelect = Wire(UInt(log2Up(loadQueueSize).W))
  writebackSelect := Mux(haveLoadResp, ldqidxResp, pendingCDBCmtSelect)
  // assert(!(loadQueue(pendingCDBCmtSelect).valid && !MEMOpID.needStore(loadQueue(pendingCDBCmtSelect).op) && MEMOpID.needLoad(loadQueue(pendingCDBCmtSelect).op)))

  // load queue enqueue
  val loadQueueEnqueue = io.in.fire()
  when(loadQueueEnqueue){ loadHeadPtr := loadHeadPtr + 1.U }
  // move loadDtlbPtr
  val dtlbReqsend = io.dtlb.req.fire() || !dtlbEnable && loadQueueEnqueue
  when(dtlbReqsend){ loadDtlbPtr := loadDtlbPtr + 1.U }
  // move loadDmemPtr
  val loadQueueReqsend = dmem.req.fire() && MEMOpID.commitToCDB(opReq)
  val nextLoadDmemPtr = WireInit(loadDmemPtr)
  when(loadQueueReqsend || storeQueueEnqueue || skipAInst){
    nextLoadDmemPtr := loadDmemPtr + 1.U
  }
  loadDmemPtr := nextLoadDmemPtr

  // load queue dequeue
  when(io.out.fire()){
    loadQueue(writebackSelect).valid := false.B
    loadQueue(writebackSelect).finished := true.B
  }

  when((loadTailPtr =/= loadDmemPtr) && !loadQueue(loadTailPtr).valid && loadQueue(loadTailPtr).finished){
    loadTailPtr := loadTailPtr + 1.U
    loadQueue(loadTailPtr).valid := false.B
    loadQueue(loadTailPtr).finished := false.B
  }

  // when branch, invalidate insts in wrong branch direction
  List.tabulate(loadQueueSize)(i => {
    when(loadQueue(i).brMask(branchIndex) && bruFlush){
      loadQueue(i).valid := false.B
    }
    loadQueue(i).brMask := updateBrMask(loadQueue(i).brMask) // fixit, AS WELL AS STORE BRMASK !!!!!
  })
  
  // write data to loadqueue
  val vaddrIsMMIO = AddressSpace.isMMIO(addr)
  val paddrIsMMIO = AddressSpace.isMMIO(io.dtlb.resp.bits.rdata)

  when(loadQueueEnqueue){
    loadQueue(loadHeadPtr).pc := io.uopIn.decode.cf.pc
    loadQueue(loadHeadPtr).prfidx := io.uopIn.prfDest
    loadQueue(loadHeadPtr).brMask := updateBrMask(io.brMaskIn)
    loadQueue(loadHeadPtr).vaddr := addr
    loadQueue(loadHeadPtr).paddr := addr
    loadQueue(loadHeadPtr).func := func
    loadQueue(loadHeadPtr).size := size
    loadQueue(loadHeadPtr).op := memop
    loadQueue(loadHeadPtr).data := genWdata(io.wdata, size)
    loadQueue(loadHeadPtr).fdata := 0.U
    loadQueue(loadHeadPtr).fmask := 0.U
    loadQueue(loadHeadPtr).asrc := io.wdata // FIXIT
    loadQueue(loadHeadPtr).rfWen := io.uopIn.decode.ctrl.rfWen
    loadQueue(loadHeadPtr).isMMIO := vaddrIsMMIO // FIXIT
    loadQueue(loadHeadPtr).valid := true.B
    loadQueue(loadHeadPtr).tlbfin := !dtlbEnable
    loadQueue(loadHeadPtr).finished := false.B
    loadQueue(loadHeadPtr).loadPageFault := false.B
    loadQueue(loadHeadPtr).storePageFault := false.B
    loadQueue(loadHeadPtr).loadAddrMisaligned := findLoadAddrMisaligned
    loadQueue(loadHeadPtr).storeAddrMisaligned := findStoreAddrMisaligned
  }

  when(storeQueueEnqueue || skipAInst){
    loadQueue(loadDmemPtr).finished := true.B
    // store inst does not need to access mem until it is commited
  }

  Debug(){
    printf("[LSU LDQ] time %d\n", GTimer())
    printf("[LSU LDQ] pc           id vaddr        paddr        func    op      data             mmio   valid   finished    exc     mask\n")
    for(i <- 0 until loadQueueSize){
      printf(
        "[LSU LDQ] 0x%x %x 0x%x 0x%x %b %b %x mmio:%b valid:%b finished:%b%b exc:%b%b%b%b %x %d", 
        loadQueue(i).pc, loadQueue(i).prfidx, loadQueue(i).vaddr, loadQueue(i).paddr, loadQueue(i).func, loadQueue(i).op, loadQueue(i).data, loadQueue(i).isMMIO, loadQueue(i).valid, loadQueue(i).tlbfin, loadQueue(i).finished, loadQueue(i).loadPageFault, loadQueue(i).storePageFault, loadQueue(i).loadAddrMisaligned, loadQueue(i).storeAddrMisaligned, loadQueue(i).brMask, i.U
      )
      when(loadQueue(i).valid){printf(" valid")}
      when(loadHeadPtr === i.U){printf(" head")}
      when(loadDtlbPtr === i.U){printf(" tlb")}
      when(loadDmemPtr === i.U){printf(" mem")}
      when(loadTailPtr === i.U){printf(" tail")}
      printf("\n")
    }
  }

  Debug(){
    when(loadQueueEnqueue){
      printf("[ENLDQ] pc %x ldqidx %x time %x\n", io.uopIn.decode.cf.pc, loadHeadPtr, GTimer())
    }
  }

  //                               Store Queue
  //              ------------------------------------------------------------
  // ---> Enqueue |   not used   |   commited   |   retiring   |   retired   |  --> Dequeue
  //              ------------------------------------------------------------
  //                             |              |              |
  //                            head           cmt            req
  val storeQueue = Reg(Vec(storeQueueSize, new StoreQueueEntry))
  // Store Queue contains store insts that have finished TLB lookup stage
  // There are 2 types of store insts in this queue: ROB-commited (retired) / CDB-commited (commited)
  // CDB-commited insts have already gotten their paddr from TLB, 
  // but whether these insts will be canceled is still pending for judgement.
  // ROB-commited insts are those insts already retired from ROB
  // val storeAlloc   = RegInit(0.U((log2Up(storeQueueSize)+1).W))
  val storeHeadPtr    = RegInit(0.U((log2Up(storeQueueSize)+1).W))
  val storeCmtPtr     = RegInit(0.U((log2Up(storeQueueSize)+1).W))
  val nextStoreCmtPtr = Wire(UInt((log2Up(storeQueueSize)+1).W))
  val haveUnconfirmedStore = storeHeadPtr =/= storeCmtPtr
  val haveUnrequiredStore = storeCmtPtr =/= 0.U && storeQueue(0).valid
  val haveUnfinishedStore = 0.U =/= storeHeadPtr
  val storeQueueFull = storeHeadPtr === storeQueueSize.U 
  io.haveUnfinishedStore := haveUnfinishedStore

  // alloc a slot when a store tlb request is sent
  // val storeQueueAlloc = dmem.req.fire() && MEMOpID.commitToCDB(opReq) && MEMOpID.needStore(opReq)
  // after a store inst get its paddr from TLB, add it to store queue
  val dtlbRespUser = io.dtlb.resp.bits.user.get.asTypeOf(new DCacheUserBundle)
  val tlbRespStoreEnq = io.dtlb.resp.fire() && MEMOpID.needStore(dtlbRespUser.op) && !MEMOpID.needAlu(dtlbRespUser.op) && !bruFlush && loadQueue(dtlbRespUser.ldqidx).valid && tlbRespLdqidx === loadDmemPtr
  storeQueueEnqueue := havePendingStoreEnq && !storeQueueFull || !havePendingDmemReq && tlbRespStoreEnq && !storeQueueFull
  val tlbRespAMOStoreEnq = io.dtlb.resp.fire() && MEMOpID.needAlu(dtlbRespUser.op) && !bruFlush && loadQueue(dtlbRespUser.ldqidx).valid && tlbRespLdqidx === loadDmemPtr
  val storeQueueAMOEnqueue = havePendingAMOStoreEnq && loadQueueReqsend || tlbRespAMOStoreEnq && loadQueueReqsend
  assert(!(storeQueueAMOEnqueue && storeQueueFull))
  // when a store inst is retired, commit 1 term in Store Queue
  val storeQueueConfirm = io.scommit // TODO: Argo only support 1 scommit / cycle
  // when a store inst actually writes data to dmem, mark it as `waiting for dmem resp`
  val storeQueueReqsend = dmem.req.fire() && MEMOpID.commitToSTQ(opReq)
  val storeQueueSkipInst = !storeQueue(0).valid && storeHeadPtr =/= 0.U
  // when dmem try to commit to store queue, i.e. dmem report a write op is finished, dequeue
  // FIXIT: in current case, we can always assume a store is succeed after req.fire()
  // therefore storeQueueDequeue is not necessary 
  val storeQueueDequeue = storeQueueReqsend || storeQueueSkipInst // dmem.resp.fire() && MEMOpID.commitToSTQ(opResp) && !MEMOpID.needLoad(opResp) //&& MEMOpID.needStore(opResp)
  when(storeQueueDequeue){
    // storeQueue := Cat(storeQueue(0), storeQueue(storeQueueSize-1, 1))
    List.tabulate(storeQueueSize - 1)(i => {
      storeQueue(i) := storeQueue(i+1)
    })
  }

  // move storeCmtPtr ptr
  nextStoreCmtPtr := storeCmtPtr
  when((storeQueueDequeue && !storeQueueSkipInst) && !storeQueueConfirm){nextStoreCmtPtr := storeCmtPtr - 1.U}
  when(!(storeQueueDequeue && !storeQueueSkipInst) && storeQueueConfirm){nextStoreCmtPtr := storeCmtPtr + 1.U}
  storeCmtPtr := nextStoreCmtPtr
  
  // move storeHeadPtr ptr
  when(storeQueueDequeue && !(storeQueueEnqueue || storeQueueAMOEnqueue)){storeHeadPtr := storeHeadPtr - 1.U}
  when(!storeQueueDequeue && (storeQueueEnqueue || storeQueueAMOEnqueue)){storeHeadPtr := storeHeadPtr + 1.U}
  when(io.flush){storeHeadPtr := nextStoreCmtPtr}
  assert(!(storeQueueEnqueue && storeQueueAMOEnqueue))

  // when branch, invalidate insts in wrong branch direction
  List.tabulate(storeQueueSize)(i => {
    when(storeQueueDequeue){
      when(storeQueue(i).brMask(branchIndex) && bruFlush){
        if(i != 0){ storeQueue(i-1).valid := false.B }
      }
      if(i != 0){ storeQueue(i-1).brMask := updateBrMask(storeQueue(i).brMask) }
    }.otherwise{
      when(storeQueue(i).brMask(branchIndex) && bruFlush){
        storeQueue(i).valid := false.B
      }
      storeQueue(i).brMask := updateBrMask(storeQueue(i).brMask)
    }
  })

  // write data to store queue
  val storeQueueEnqPtr = Mux(storeQueueDequeue, storeHeadPtr - 1.U, storeHeadPtr)
  val havePendingStqEnq = havePendingStoreEnq || havePendingAMOStoreEnq
  val storeQueueEnqSrcPick = Mux(havePendingStqEnq, loadDmemPtr, dtlbRespUser.ldqidx)
  when(storeQueueEnqueue || storeQueueAMOEnqueue){
    storeQueue(storeQueueEnqPtr).pc := loadQueue(storeQueueEnqSrcPick).pc
    storeQueue(storeQueueEnqPtr).prfidx := loadQueue(storeQueueEnqSrcPick).prfidx
    storeQueue(storeQueueEnqPtr).brMask := updateBrMask(loadQueue(storeQueueEnqSrcPick).brMask)
    storeQueue(storeQueueEnqPtr).wmask := genWmask(loadQueue(storeQueueEnqSrcPick).vaddr, loadQueue(loadDmemPtr).size)
    storeQueue(storeQueueEnqPtr).vaddr := loadQueue(storeQueueEnqSrcPick).vaddr
    storeQueue(storeQueueEnqPtr).paddr := Mux(havePendingStqEnq, loadQueue(loadDmemPtr).paddr, io.dtlb.resp.bits.rdata)
    storeQueue(storeQueueEnqPtr).func := loadQueue(storeQueueEnqSrcPick).func
    storeQueue(storeQueueEnqPtr).size := loadQueue(storeQueueEnqSrcPick).size
    storeQueue(storeQueueEnqPtr).op := loadQueue(storeQueueEnqSrcPick).op
    storeQueue(storeQueueEnqPtr).data := loadQueue(storeQueueEnqSrcPick).data
    storeQueue(storeQueueEnqPtr).isMMIO := Mux(havePendingStqEnq, loadQueue(loadDmemPtr).isMMIO, paddrIsMMIO)
    storeQueue(storeQueueEnqPtr).valid := true.B && !(loadQueue(storeQueueEnqSrcPick).brMask(branchIndex) && bruFlush)
  }

  Debug(){
    when(storeQueueEnqueue || storeQueueAMOEnqueue){
      printf("[ENSTQ] pc %x ldqidx %x valid %x enqp %x head %x time %x\n", loadQueue(ldqidxResp).pc, ldqidxResp, loadQueue(ldqidxResp).valid, storeQueueEnqPtr, storeHeadPtr, GTimer())
    }
  }

  // printf("[PSTQ] time %x alloc %x head %x mid %x flush %x\n", GTimer(), storeAlloc, storeHeadPtr, storeCmtPtr, io.flush)

  Debug(){
    printf("[LSU STQ] time %d\n", GTimer())
    printf("[LSU STQ] pc           id vaddr        paddr        func    op      data             mmio v mask \n")
    for(i <- 0 to (storeQueueSize - 1)){
      printf(
        "[LSU STQ] 0x%x %x 0x%x 0x%x %b %b %x mmio:%b %b %x %d", 
        storeQueue(i).pc, storeQueue(i).prfidx, storeQueue(i).vaddr, storeQueue(i).paddr, storeQueue(i).func, storeQueue(i).op, storeQueue(i).data, storeQueue(i).isMMIO, storeQueue(i).valid, storeQueue(i).brMask, i.U
      )
      // when(storeAlloc === i.U){printf(" alloc")}
      when(storeHeadPtr === i.U){printf(" head")}
      when(storeCmtPtr === i.U){printf(" cmt")}
      // when(storeReqPtr === i.U){printf(" req")}
      printf("\n")
    }
  }

  //-------------------------------------------------------
  // Load / Store Pipeline
  //-------------------------------------------------------

  //-------------------------------------------------------
  // LSU Stage 1: enqueue
  // Generate addr, add uop to ls queue
  //-------------------------------------------------------

  // Exception check, fix mop
  // Misalign exception generation 
  val addrAligned = LookupTree(size, List(
    "b00".U   -> true.B,              //b
    "b01".U   -> (addr(0) === 0.U),   //h
    "b10".U   -> (addr(1,0) === 0.U), //w
    "b11".U   -> (addr(2,0) === 0.U)  //d
  ))
  findLoadAddrMisaligned  := valid && !storeReq && !amoReq && !addrAligned
  findStoreAddrMisaligned := valid && (storeReq || amoReq) && !addrAligned
  // when(findLoadAddrMisaligned || findStoreAddrMisaligned){memop(1,0) := "b01".U} // Just load

  //-------------------------------------------------------
  // LSU Stage 2,3,4,5: mem req
  // Send request to TLB/Cache, and wait for response
  //-------------------------------------------------------


  //-------------------------------------------------------
  // DTLB Access
  //-------------------------------------------------------
  // Send request to dtlb
  val dtlbUserBundle = Wire(new DCacheUserBundle)
  dtlbUserBundle.ldqidx :=  Mux(havePendingDtlbReq, loadDtlbPtr, loadHeadPtr)
  dtlbUserBundle.op := Mux(havePendingDtlbReq, loadQueue(loadDtlbPtr).op, memop)
  val pfType = Mux(havePendingDtlbReq, LSUOpType.needMemWrite(loadQueue(loadDtlbPtr).func), LSUOpType.needMemWrite(func)) // 0: load pf 1: store pf

  io.dtlb.req.bits.apply(
    addr = Mux(havePendingDtlbReq, loadQueue(loadDtlbPtr).vaddr(VAddrBits-1, 0), addr(VAddrBits-1, 0)), 
    size = 0.U, wdata = 0.U, wmask = 0.U, 
    cmd = pfType, 
    user = dtlbUserBundle.asUInt
  )
  io.dtlb.req.valid := havePendingDtlbReq || io.in.fire() && dtlbEnable
  io.dtlb.resp.ready := true.B

  val loadPF = WireInit(false.B)
  val storePF = WireInit(false.B)
  BoringUtils.addSink(loadPF, "loadPF") // FIXIT: this is nasty
  BoringUtils.addSink(storePF, "storePF") // FIXIT: this is nasty
  // val dtlbPF = loadPF || storePF
  Debug(){
    when(io.flush){
      printf("[DTLB FLUSH] %d\n", GTimer())
    }
    when(io.dtlb.req.fire()){
      printf("[DTLB REQ] %d: req paddr for %x ldqid %x\n", GTimer(), io.dtlb.req.bits.addr, loadDtlbPtr)
    }
    when(io.dtlb.resp.fire()){
      printf("[DTLB RESP] %d: get paddr: %x for %x ldqid %x exc l %b s %b\n", GTimer(), io.dtlb.resp.bits.rdata, loadQueue(tlbRespLdqidx).vaddr, tlbRespLdqidx, loadPF, storePF)
    }
  }
  when(io.dtlb.resp.fire()){
    loadQueue(tlbRespLdqidx).paddr := io.dtlb.resp.bits.rdata // FIXIT
    loadQueue(tlbRespLdqidx).tlbfin := true.B
    loadQueue(tlbRespLdqidx).isMMIO := paddrIsMMIO
    loadQueue(tlbRespLdqidx).loadPageFault := loadPF
    loadQueue(tlbRespLdqidx).storePageFault := storePF
  }
  // assert(loadQueue(tlbRespLdqidx).valid && !loadQueue(tlbRespLdqidx).tlbfin || !io.dtlb.resp.fire())
  // assert(!(!dtlbEnable && io.dtlb.resp.fire()))

  //-------------------------------------------------------
  // Mem Req
  //-------------------------------------------------------

  // Mem req has the following attributes:
  // * commitToCDB
  // * commitToSTQ
  // * commitToTLB
  // * commitToVPU
  // * reqLOAD
  // * reqSTORE
  // * reqAtomALU  

  // Mem req srcs are distingushed by attribute:
  // doNothing   0000: bubble: just go throught the pipeline and do nothing
  // commitToCDB 0001: load: will commit to CDB the cycle it is finished
  // commitToSTQ 0010: store: will update store queue counter when it is finished
  // commitToTLB 0100: pagewalker
  // commitToVPU 1000: vpu

  // An inst can fire a mem request if:
  // * pagewalker is not working
  // * no atom inst is on the fly //TODO
  // * it is a load inst or
  // * it is a store inst and it is being retired from ROB

  // Note: Atom inst will block the pipeline, for now

  val loadDMemReq  = Wire(new MemReq)
  val storeDMemReq = Wire(new MemReq)
  val noDMemReq    = Wire(new MemReq)

  // TODO: refactor with utils.Arbitor

  // val pageTableWalkerWorking = RegInit(false.B)
  // val tlbReadygo   = tlbDMemReq.valid
  val storeReadygo = storeDMemReq.valid && (!loadDMemReq.valid || storeQueueFull)
  val loadReadygo  = loadDMemReq.valid && !storeQueueFull

  val memReq = Mux1H(List(
    loadReadygo -> loadDMemReq,
    storeReadygo -> storeDMemReq,
    (!loadReadygo && !storeReadygo) -> noDMemReq
  ))

  // loadDMemReq
  val loadDMemReqSrcPick = Mux(havePendingDmemReq, loadDmemPtr, io.dtlb.resp.bits.user.get.asTypeOf(new DCacheUserBundle).ldqidx)
  val loadDTlbRespReqValid = dtlbEnable && io.dtlb.resp.fire() && MEMOpID.needLoad(dtlbRespUser.op) && !dmemReqFromLoadQueue && 
    !loadPF && !storePF && !loadQueue(tlbRespLdqidx).loadAddrMisaligned && !loadQueue(tlbRespLdqidx).storeAddrMisaligned &&
    !bruFlush && loadQueue(tlbRespLdqidx).valid && tlbRespLdqidx === loadDmemPtr
  val loadSideUserBundle = Wire(new DCacheUserBundle)
  loadSideUserBundle.ldqidx := loadDMemReqSrcPick
  loadSideUserBundle.op := loadQueue(loadDMemReqSrcPick).op

  loadDMemReq.addr := Mux(havePendingDmemReq, loadQueue(loadDmemPtr).paddr, io.dtlb.resp.bits.rdata)
  loadDMemReq.size := loadQueue(loadDMemReqSrcPick).size
  loadDMemReq.wdata := loadQueue(loadDMemReqSrcPick).data
  loadDMemReq.valid := havePendingDmemReq || loadDTlbRespReqValid
  loadDMemReq.wmask := genWmask(loadDMemReq.addr, loadDMemReq.size)
  loadDMemReq.cmd := SimpleBusCmd.read
  loadDMemReq.user := loadSideUserBundle

  // storeDMemReq
  val storeSideUserBundle = Wire(new DCacheUserBundle)
  storeSideUserBundle.ldqidx := DontCare
  storeSideUserBundle.op := MEMOpID.storec

  storeDMemReq.addr := storeQueue(0.U).paddr
  storeDMemReq.size := storeQueue(0.U).size
  storeDMemReq.wdata := genWdata(storeQueue(0.U).data, storeQueue(0.U).size)
  storeDMemReq.wmask := storeQueue(0.U).wmask
  storeDMemReq.cmd := SimpleBusCmd.write
  storeDMemReq.user := storeSideUserBundle
  storeDMemReq.valid := haveUnrequiredStore
  when(LSUOpType.isAMO(storeQueue(0.U).func)){
    storeDMemReq.wdata := io.atomData
  }

  // noDMemReq
  noDMemReq := DontCare
  noDMemReq.valid := false.B

  // Debug(){
    // printf("[DREQ] addr %x, size %x, wdata %x, wmask %x, cmd %x, user %x %x %b, valid %x\n",
    //   memReq.addr, memReq.size, memReq.wdata, memReq.wmask, memReq.cmd, memReq.user.asUInt, memReq.user.ldqidx, memReq.user.op, memReq.valid
    // )
  // }

  // Send request to dmem
  
  def genWmask(addr: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> 0x1.U, //0001 << addr(2:0)
      "b01".U -> 0x3.U, //0011
      "b10".U -> 0xf.U, //1111
      "b11".U -> 0xff.U //11111111
    )) << addr(2, 0)
  }
  
  def genWdata(data: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> Fill(8, data(7, 0)),
      "b01".U -> Fill(4, data(15, 0)),
      "b10".U -> Fill(2, data(31, 0)),
      "b11".U -> data
    ))
  }

  // when(mem.fire() && !pageTableWalkerWorking) push forward LQ/SQ pointer
  // TODO: apply user
  dmem.req.bits.apply(addr = memReq.addr(VAddrBits-1, 0), size = memReq.size, wdata = memReq.wdata,
    wmask = genWmask(memReq.addr, memReq.size), cmd = memReq.cmd, user = memReq.user.asUInt)
  dmem.req.valid := memReq.valid // tlbReadygo || loadReadygo || storeReadygo
  dmem.resp.ready := true.B

  //-------------------------------------------------------
  // Mem Resp
  //-------------------------------------------------------

  // Out of order dequeue
  // TBD

  // Load Queue dequeue
  // In-order dequeue
  // If it is a valid store inst, add it to store queue
  // If an inst is marked as `finished`, it will be commited to CDB in the next cycle

  // MMIO check
  val lsuMMIO = WireInit(false.B)
  BoringUtils.addSink(lsuMMIO, "lsuMMIO")

  // Store addr backward match
  // If match, get data from store queue, and mark that inst as resped
  val dataBackVec = Wire(Vec(XLEN/8, (UInt((XLEN/8).W))))
  val dataBack = dataBackVec.asUInt
  val forwardVec = VecInit(List.tabulate(storeQueueSize)(i => {
    i.U < storeHeadPtr && io.dmem.req.bits.addr(PAddrBits-1, log2Up(XLEN/8)) === storeQueue(i).paddr(PAddrBits-1, log2Up(XLEN/8)) && storeQueue(i).valid
  }))
  val forwardWmask = List.tabulate(storeQueueSize)(i => storeQueue(i).wmask & Fill(XLEN/8, forwardVec(i))).foldRight(0.U)((sum, i) => sum | i)
  for(j <- (0 to (XLEN/8 - 1))){
    dataBackVec(j) := MuxCase( 
      // default = dmem.resp.bits.rdata(8*(j+1)-1, 8*j), 
      default = 0.U,
      mapping = List.tabulate(storeQueueSize)(i => {
        (forwardVec(i) && storeQueue(i).wmask(j), storeQueue(i).data(8*(j+1)-1, 8*j))
      }).reverse
    )
  }

  // write back to load queue
  when(dmem.req.fire() && MEMOpID.needLoad(opReq)){
    loadQueue(loadDmemPtr).fdata := dataBack
    loadQueue(loadDmemPtr).fmask := forwardWmask
  }

  val rdataFwdSelVec = Wire(Vec(XLEN/8, (UInt((XLEN/8).W))))
  val rdataFwdSel = rdataFwdSelVec.asUInt
  for(j <- (0 until (XLEN/8))){
    rdataFwdSelVec(j) := Mux(loadQueue(ldqidxResp).fmask(j), loadQueue(ldqidxResp).fdata(8*(j+1)-1, 8*j), dmem.resp.bits.rdata(8*(j+1)-1, 8*j))
  }

  when(dmem.resp.fire()){
    when(MEMOpID.commitToCDB(opResp) || MEMOpID.commitToVPU(opResp)){
      when(MEMOpID.needLoad(opResp)){loadQueue(ldqidxResp).data := rdataFwdSel}
      loadQueue(ldqidxResp).finished := true.B
    }
  }

  //-------------------------------------------------------
  // LSU Stage 4: Atom and CDB broadcast
  // Atom ALU gets data and writes its result to temp reg
  //-------------------------------------------------------

  // Load Data Selection
  val rdata = rdataFwdSel
  val rdataSel = LookupTree(loadQueue(ldqidxResp).vaddr(2, 0), List(
    "b000".U -> rdata(63, 0),
    "b001".U -> rdata(63, 8),
    "b010".U -> rdata(63, 16),
    "b011".U -> rdata(63, 24),
    "b100".U -> rdata(63, 32),
    "b101".U -> rdata(63, 40),
    "b110".U -> rdata(63, 48),
    "b111".U -> rdata(63, 56)
  ))
  val rdataPartialLoad = LookupTree(loadQueue(ldqidxResp).func, List(
      LSUOpType.lb   -> SignExt(rdataSel(7, 0) , XLEN),
      LSUOpType.lh   -> SignExt(rdataSel(15, 0), XLEN),
      LSUOpType.lw   -> SignExt(rdataSel(31, 0), XLEN),
      LSUOpType.ld   -> SignExt(rdataSel(63, 0), XLEN),
      LSUOpType.lbu  -> ZeroExt(rdataSel(7, 0) , XLEN),
      LSUOpType.lhu  -> ZeroExt(rdataSel(15, 0), XLEN),
      LSUOpType.lwu  -> ZeroExt(rdataSel(31, 0), XLEN)
  ))
  val atomDataPartialLoad = Mux(loadQueue(ldqidxResp).size(0), SignExt(rdataSel(63, 0), XLEN), SignExt(rdataSel(31, 0), XLEN))

  // Atom
  val atomALU = Module(new AtomALU)
  atomALU.io.src1 := atomDataPartialLoad
  atomALU.io.src2 := loadQueue(ldqidxResp).asrc//io.wdata FIXIT: use a single reg
  atomALU.io.func := loadQueue(ldqidxResp).func
  atomALU.io.isWordOp := !loadQueue(ldqidxResp).size(0)  //recover atomWidthW from size
  // When an atom inst reaches here, store its result to store buffer,
  // then commit it to CDB, set atom-on-the-fly to false
  val atomDataReg = RegEnable(genWdata(atomALU.io.result, loadQueue(ldqidxResp).size), io.out.fire() && LSUOpType.isAMO(loadQueue(ldqidxResp).func))
  io.atomData := atomDataReg

  // Commit to CDB
  io.out.bits := MuxCase(
      default = rdataPartialLoad,
      mapping = List(
        (loadQueue(writebackSelect).loadPageFault || loadQueue(writebackSelect).storePageFault || loadQueue(writebackSelect).loadAddrMisaligned || loadQueue(writebackSelect).storeAddrMisaligned) -> loadQueue(writebackSelect).vaddr,
        LSUOpType.isSC(loadQueue(writebackSelect).func) -> !MEMOpID.needStore(loadQueue(writebackSelect).op),
        LSUOpType.isAtom(loadQueue(writebackSelect).func) -> atomDataPartialLoad
      )
  )

  // when(LSUOpType.isAMO(loadQueue(loadTailPtr).func) && io.out.fire()){
  //   printf("[AMO] %d: pc %x %x %x func %b word %b op %b res %x addr %x:%x\n", GTimer(), loadQueue(loadTailPtr).pc, atomALU.io.src1, atomALU.io.src2, loadQueue(loadTailPtr).func, atomALU.io.isWordOp, loadQueue(loadTailPtr).op, atomALU.io.result, loadQueue(loadTailPtr).vaddr, loadQueue(loadTailPtr).paddr)
  // }

  io.uopOut := DontCare
  io.isMMIO := loadQueue(writebackSelect).isMMIO
  io.exceptionVec.map(_ := false.B)
  io.exceptionVec(loadPageFault) := loadQueue(writebackSelect).loadPageFault
  io.exceptionVec(storePageFault) := loadQueue(writebackSelect).storePageFault
  io.exceptionVec(loadAddrMisaligned) := loadQueue(writebackSelect).loadAddrMisaligned
  io.exceptionVec(storeAddrMisaligned) := loadQueue(writebackSelect).storeAddrMisaligned
  io.uopOut.decode.cf.pc := loadQueue(writebackSelect).pc
  io.uopOut.prfDest := loadQueue(writebackSelect).prfidx
  io.uopOut.decode.ctrl.rfWen := loadQueue(writebackSelect).rfWen && !loadQueue(writebackSelect).loadAddrMisaligned && !loadQueue(writebackSelect).storeAddrMisaligned && !loadQueue(writebackSelect).loadPageFault && !loadQueue(writebackSelect).storePageFault

  io.in.ready := !loadQueueFull
  io.out.valid := havePendingCDBCmt || haveLoadResp
  assert(!(io.out.valid && !io.out.ready))

  when(io.flush){
    loadHeadPtr := nextLoadDmemPtr
    loadDtlbPtr := nextLoadDmemPtr
    for(i <- 0 to (loadQueueSize - 1)){
      loadQueue(i).valid := false.B
      loadQueue(i).tlbfin := false.B
    }
  }

  BoringUtils.addSource(io.out.fire() && io.isMMIO, "perfCntCondMmmioInstr")
  BoringUtils.addSource(loadQueueFull, "perfCntCondMmemqFull")
  BoringUtils.addSource(storeQueueFull, "perfCntCondMstqFull")
  BoringUtils.addSource(io.in.fire() && MEMOpID.needLoad(memop), "perfCntCondMloadCnt")
  BoringUtils.addSource(io.in.fire() && MEMOpID.needStore(memop), "perfCntCondMstoreCnt")
  BoringUtils.addSource(dmem.req.fire() && MEMOpID.needLoad(opReq) && forwardWmask.orR, "perfCntCondMmemSBL")
  BoringUtils.addSource(storeQueueFull, "perfCntCondMpendingLS")
  BoringUtils.addSource(storeQueueFull, "perfCntCondMpendingSCmt")
  BoringUtils.addSource(storeQueueFull, "perfCntCondMpendingSReq")
  val ldqValid = VecInit((0 until loadQueueSize).map(i => loadQueue(i).valid))
  BoringUtils.addSource(PopCount(ldqValid.asUInt), "perfCntSrcMpendingLS")
  BoringUtils.addSource(storeCmtPtr, "perfCntSrcMpendingSCmt")
  BoringUtils.addSource(storeHeadPtr, "perfCntSrcMpendingSReq")

  val reqpc = Mux(storeReadygo, storeQueue(0.U).pc, Mux(havePendingDmemReq, loadQueue(loadDmemPtr).pc, Mux(dtlbEnable, loadQueue(dtlbRespUser.ldqidx).pc, loadQueue(dtlbRespUser.ldqidx).pc)))
  Debug(){
    printf("[DMEM] req v %x r %x addr %x data %x op %b id %x  resp v %x r %x data %x op %b id %x time %x\n",
      dmem.req.valid, dmem.req.ready, dmem.req.bits.addr, dmem.req.bits.wdata, dmem.req.bits.user.get.asTypeOf(new DCacheUserBundle).op, dmem.req.bits.user.get.asTypeOf(new DCacheUserBundle).ldqidx,
      dmem.resp.valid, dmem.resp.ready, dmem.resp.bits.rdata, dmem.resp.bits.user.get.asTypeOf(new DCacheUserBundle).op, dmem.resp.bits.user.get.asTypeOf(new DCacheUserBundle).ldqidx,
      GTimer()
    )
    when(dmem.req.fire()){
      printf("[LSU DREQ] ")
      when(loadReadygo){printf("loadDMemReq")}
      when(storeReadygo){printf("storeDMemReq")}
      when((!loadReadygo && !storeReadygo)){printf("noDMemReq")}
      printf(" pc %x addr 0x%x size %x wdata %x cmd %x ldqidx %x memop %b spending %x lpending %x time %d\n", reqpc, dmem.req.bits.addr, dmem.req.bits.size, dmem.req.bits.wdata, dmem.req.bits.cmd, memReq.user.ldqidx, memReq.user.op, storeCmtPtr - 0.U, loadHeadPtr - loadDmemPtr, GTimer())
    }
    when(dmem.resp.fire()){
      printf("[LSU DRESP] memdata %x fwddata %x:%b data %x ldqidx %x memop %b isMMIO %x time %d\n", dmem.resp.bits.rdata, loadQueue(dmemUserOut.ldqidx).fdata, loadQueue(dmemUserOut.ldqidx).fmask, rdataFwdSel, dmemUserOut.ldqidx, dmemUserOut.op, lsuMMIO, GTimer())
    }
    when(dmem.req.fire() && !MEMOpID.needStore(opReq) && forwardWmask.orR){
      printf("[LSU FWD] dataBack %x forwardWmask %b\n", dataBack, forwardWmask)
    }
  }

  val printMemTrace = false
  val addrTrack = List(
    "h12345678".U,
    "h8332b9c8".U,
    "h8332b9ca".U,
    "h8332b9cc".U,
    "h8332b9ce".U
  )

  when(printMemTrace.B || addrTrack.map(i => dmem.req.bits.addr === i).foldRight(false.B)((sum, i) => sum | i)){
    when(dmem.req.fire()){
      printf("[LSU DREQ TRACE] ")
      when(loadReadygo){printf("loadDMemReq")}
      when(storeReadygo){printf("storeDMemReq")}
      when((!loadReadygo && !storeReadygo)){printf("noDMemReq")}
      printf(" pc %x addr 0x%x size %x wdata %x cmd %x ldqidx %x memop %b spending %x lpending %x time %d\n", reqpc, dmem.req.bits.addr, dmem.req.bits.size, dmem.req.bits.wdata, dmem.req.bits.cmd, memReq.user.ldqidx, memReq.user.op, storeCmtPtr - 0.U, loadHeadPtr - loadDmemPtr, GTimer())
  }
    when(dmem.resp.fire()){
      printf("[LSU DRESP TRACE] memdata %x fwddata %x:%b data %x ldqidx %x memop %b isMMIO %x time %d\n", dmem.resp.bits.rdata, loadQueue(dmemUserOut.ldqidx).fdata, loadQueue(dmemUserOut.ldqidx).fmask, rdataFwdSel, dmemUserOut.ldqidx, dmemUserOut.op, lsuMMIO, GTimer())
    }
    when(dmem.req.fire() && !MEMOpID.needStore(opReq) && forwardWmask.orR){
      printf("[LSU FWD TRACE] dataBack %x forwardWmask %b\n", dataBack, forwardWmask)
    }
  }

}
