package noop
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import bus.simplebus._

// Out Of Order Load/Store Unit
// This version has not enable loadQueue / AtomInst

// LSU    | AddrGen |        | Forward | LoadCmt/AtomALU |
// DMEM   | Load 1  | Load 2 | Load 3  |                 |
// TLB    | TLB  1  | TLB  2 | TLB  3  |                 |

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
  val uopIn = Input(new RenamedDecodeIO)
  val uopOut = Output(new RenamedDecodeIO)
  val isMMIO = Output(Bool())
  val exceptionVec = Output(Vec(16, Bool()))
  val scommit = Input(Bool())
  val atomData = Output(UInt(XLEN.W))
  // val loadPageFault = Output(Bool()) 
  // val storePageFault = Output(Bool()) 
  // val loadAddrMisaligned = Output(Bool()) 
  // val storeAddrMisaligned = Output(Bool())
  val flush = Input(Bool())
}

class StoreQueueEntry extends NOOPBundle{
  val pc       = UInt(VAddrBits.W)
  val prfidx   = UInt(prfAddrWidth.W) // for debug
  val wmask   = UInt((XLEN/8).W) // for store queue forwarding
  val vaddr    = UInt(VAddrBits.W)
  val paddr    = UInt(PAddrBits.W)
  val func     = UInt(7.W)
  val op       = UInt(7.W)
  val data     = UInt(XLEN.W)
  val isMMIO   = Bool()
  // val valid    = Bool()
  // val finished = Bool()
}

class LoadQueueEntry extends NOOPBundle{
  val pc       = UInt(VAddrBits.W)
  val prfidx   = UInt(prfAddrWidth.W)
  val vaddr    = UInt(VAddrBits.W) // for debug
  val paddr    = UInt(PAddrBits.W)
  val func     = UInt(7.W)
  val op       = UInt(7.W)
  val data     = UInt(XLEN.W)
  val asrc     = UInt(XLEN.W) // alusrc2 for atom inst
  val awidthW  = Bool() // atomWidthW
  val rfWen    = Bool()
  val isMMIO   = Bool()
  val valid    = Bool() // for debug
  val finished = Bool() // for debug
  val loadPageFault  = Bool()
  val storePageFault  = Bool()
  val loadAddrMisaligned  = Bool()
  val storeAddrMisaligned = Bool()
}

class DCacheUserBundle extends NOOPBundle {
  // val pc       = UInt(VAddrBits.W)
  // val prfidx   = UInt(prfAddrWidth.W)
  val ldqidx   = UInt(5.W) //TODO
  val op       = UInt(7.W)
  // val func     = UInt(7.W)
  // val loadAddrMisaligned  = Bool()
  // val storeAddrMisaligned = Bool()
}
// or use LoadQueueEntry instead

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
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  val dmem = io.dmem
  // Gen result
  val dmemUserOut = dmem.resp.bits.user.get.asTypeOf(new DCacheUserBundle)
  val opResp = dmem.resp.bits.user.get.asTypeOf(new DCacheUserBundle).op
  val opReq = dmem.req.bits.user.get.asTypeOf(new DCacheUserBundle).op
  val ldqidxResp = dmemUserOut.ldqidx
  // if loadqueue is enabled, dcache user bit only contains op and ldqidx

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

  val addr = src1 + src2
  val data = io.uopIn.decode.data.src2
  val size = func(1,0)
  val memop = Wire(UInt(7.W))
  memop := Cat(
    false.B, // commitToVPU
    false.B, // commitToTLB
    false.B, // commitToSTQ
    io.in.valid, // commitToCDB
    amoReq, // needAlu
    !findStoreAddrMisaligned && (storeReq || amoReq || scReq), // needStore
    loadReq || amoReq || lrReq // needLoad
    // TODO: Fix findStoreAddrMisaligned
  )

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
  setLr := DontCare
  setLrVal := DontCare
  setLrAddr := DontCare
  lr := DontCare
  lrAddr := DontCare

  // PF signal from TLB
  // TODO: add a TLB bus instead of using BoringUtils.addSink/Src
  val dtlbFinish = WireInit(false.B)
  val dtlbPF = WireInit(false.B)
  val dtlbEnable = WireInit(false.B)
  BoringUtils.addSink(dtlbFinish, "DTLBFINISH")
  BoringUtils.addSink(dtlbPF, "DTLBPF")
  BoringUtils.addSink(dtlbEnable, "DTLBENABLE")

  // L/S Queue

  //                           Load Queue
  // ------------------------------------------------------------
  // |   not used   |   waiting    |   dmemreq   |   not used   |
  // ------------------------------------------------------------
  //                |              |             |
  //              head            mid           tail

  val loadQueue = Reg(Vec(loadQueueSize, new LoadQueueEntry)) // TODO: replace it with utils.queue
  // Load Queue contains Load, Store and TLB request
  // Store insts will access TLB in load1 - load3 stage
  // loadQueue should be called 'loadStoreQueue', in some ways
  val loadHead  = RegInit(0.U((log2Up(loadQueueSize)).W))
  val loadMid   = RegInit(0.U((log2Up(loadQueueSize)).W))
  val loadTail  = RegInit(0.U((log2Up(loadQueueSize)).W))
  val loadQueueFull = loadHead === (loadTail - 1.U) //TODO: fix it with maybe_full logic
  val loadQueueEmpty = loadHead === loadTail //TODO: fix it with maybe_full logic
  val havePendingDemReq = loadMid =/= loadHead
  val havePendingCDBCmt = loadQueue(loadTail).finished && loadQueue(loadTail).valid

  // load queue enqueue
  val loadQueueEnqueue = io.in.fire()
  when(loadQueueEnqueue){ loadHead := loadHead + 1.U }
  // move loadMid ptr
  when(dmem.req.fire() && MEMOpID.commitToCDB(dmem.req.bits.user.get.asTypeOf(new DCacheUserBundle).op)){
    loadMid := loadMid + 1.U
  }
  // load queue dequeue
  when(io.out.fire()){
    loadTail := loadTail + 1.U
    loadQueue(loadTail).valid := false.B
    loadQueue(loadTail).finished := false.B
  }

  Debug(){
    printf("[LSU LDQ] time %d\n", GTimer())
    printf("[LSU LDQ] pc           id vaddr        func    op      data             mmio   valid   finished   exc \n")
    for(i <- 0 to (loadQueueSize - 1)){
      printf(
        "[LSU LDQ] 0x%x %x 0x%x %b %b %x mmio:%b valid:%b finished:%b exc:%b%b%b%b %d", 
        loadQueue(i).pc, loadQueue(i).prfidx, loadQueue(i).vaddr, loadQueue(i).func, loadQueue(i).op, loadQueue(i).data, loadQueue(i).isMMIO, loadQueue(i).valid, loadQueue(i).finished, loadQueue(i).loadPageFault, loadQueue(i).storePageFault, loadQueue(i).loadAddrMisaligned, loadQueue(i).storeAddrMisaligned, i.U
      )
      when(loadQueue(i).valid){printf(" valid")}
      when(loadHead === i.U){printf(" head")}
      when(loadMid === i.U){printf(" mid")}
      when(loadTail === i.U){printf(" tail")}
      printf("\n")
    }
  }

  Debug(){
    when(loadQueueEnqueue){
      printf("[ENLDQ] pc %x ldqidx %x time %x\n", io.uopIn.decode.cf.pc, loadHead, GTimer())
    }
  }

  //                               Store Queue
  //              ------------------------------------------------------------
  // ---> Enqueue |   not used   |   commited   |   retiring   |   retired   |  --> Dequeue
  //              ------------------------------------------------------------
  //                    |        |               |              |
  //                  alloc     head            mid            tail
  val storeQueue = Reg(Vec(storeQueueSize, new StoreQueueEntry))
  // Store Queue contains store insts that have finished TLB lookup stage
  // There are 2 types of store insts in this queue: ROB-commited (retired) / CDB-commited (commited)
  // CDB-commited insts have already gotten their paddr from TLB, 
  // but whether these insts will be canceled is still pending for judgement.
  // ROB-commited insts are those insts already retired from ROB
  val storeAlloc   = RegInit(0.U((log2Up(storeQueueSize)+1).W))
  val storeHead    = RegInit(0.U((log2Up(storeQueueSize)+1).W))
  val storeMid     = RegInit(0.U((log2Up(storeQueueSize)+1).W))
  val storeTail    = RegInit(0.U((log2Up(storeQueueSize)+1).W))
  val nextStoreMid = Wire(UInt((log2Up(storeQueueSize)+1).W))
  val haveUnconfirmedStore = storeHead =/= storeMid
  val haveUnrequiredStore = storeMid =/= storeTail
  val haveUnfinishedStore = 0.U =/= storeTail
  val storeQueueFull = storeAlloc === storeQueueSize.U 

  // alloc a slot when a store tlb request is sent
  val storeQueueAlloc = dmem.req.fire() && MEMOpID.commitToCDB(opReq) && MEMOpID.needStore(opReq)
  // when a store inst get its result from TLB, add it to store queue
  val storeQueueEnqueue = dmem.resp.fire() && MEMOpID.commitToCDB(opResp) && MEMOpID.needStore(opResp) && loadQueue(ldqidxResp).valid// && !storeQueueFull
  assert(!(storeQueueEnqueue && storeQueueFull))
  // when a store inst is retired, commit 1 term in Store Queue
  val storeQueueConfirm = io.scommit // TODO: Argo only support 1 scommit / cycle
  // when a store inst actually writes data to dmem, mark it as `waiting for dmem resp`
  val storeQueueReqsend = dmem.req.fire() && MEMOpID.commitToSTQ(opReq)
  // when dmem try to commit to store queue, i.e. dmem report a write op is finished, dequeue
  val storeQueueDequeue = dmem.resp.fire() && MEMOpID.commitToSTQ(opResp)
  when(storeQueueDequeue){
    // storeQueue := Cat(storeQueue(0), storeQueue(storeQueueSize-1, 1))
    List.tabulate(storeQueueSize - 1)(i => {
      storeQueue(i) := storeQueue(i+1)
    })
  }

  // move storeTail ptr
  when(storeQueueDequeue && !storeQueueReqsend){storeTail := storeTail - 1.U}
  when(!storeQueueDequeue && storeQueueReqsend){storeTail := storeTail + 1.U}

  // move storeMid ptr
  nextStoreMid := storeMid
  when(storeQueueDequeue && !storeQueueConfirm){nextStoreMid := storeMid - 1.U}
  when(!storeQueueDequeue && storeQueueConfirm){nextStoreMid := storeMid + 1.U}
  storeMid := nextStoreMid
  
  // move storeHead ptr
  when(storeQueueDequeue && !storeQueueEnqueue){storeHead := storeHead - 1.U}
  when(!storeQueueDequeue && storeQueueEnqueue){storeHead := storeHead + 1.U}
  when(io.flush){storeHead := nextStoreMid}

  // alloc store queue slot
  when(storeQueueDequeue && !storeQueueAlloc){storeAlloc := storeAlloc - 1.U}
  when(!storeQueueDequeue && storeQueueAlloc){storeAlloc := storeAlloc + 1.U}
  when(io.flush){storeAlloc := nextStoreMid}
  assert(!(storeQueueFull && storeQueueAlloc))

  // printf("[PSTQ] time %x alloc %x head %x mid %x tail %x flush %x\n", GTimer(), storeAlloc, storeHead, storeMid, storeTail, io.flush)

  Debug(){
    printf("[LSU STQ] time %d\n", GTimer())
    printf("[LSU STQ] pc           id vaddr        func    op      data             mmio \n")
    for(i <- 0 to (storeQueueSize - 1)){
      printf(
        "[LSU STQ] 0x%x %x 0x%x %b %b %x mmio:%b %d", 
        storeQueue(i).pc, storeQueue(i).prfidx, storeQueue(i).vaddr, storeQueue(i).func, storeQueue(i).op, storeQueue(i).data, storeQueue(i).isMMIO, i.U
      )
      when(storeAlloc === i.U){printf(" alloc")}
      when(storeHead === i.U){printf(" head")}
      when(storeMid === i.U){printf(" mid")}
      when(storeTail === i.U){printf(" tail")}
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
  val addrAligned = LookupTree(func(1,0), List(
    "b00".U   -> true.B,              //b
    "b01".U   -> (addr(0) === 0.U),   //h
    "b10".U   -> (addr(1,0) === 0.U), //w
    "b11".U   -> (addr(2,0) === 0.U)  //d
  ))
  findLoadAddrMisaligned  := valid && !storeReq && !amoReq && !addrAligned
  findStoreAddrMisaligned := valid && (storeReq || amoReq) && !addrAligned
  // when(findLoadAddrMisaligned || findStoreAddrMisaligned){memop(1,0) := "b01".U} // Just load

  // load queue enqueue
  // Head ++ 
  when(loadQueueEnqueue){
    loadQueue(loadHead).pc := io.uopIn.decode.cf.pc
    loadQueue(loadHead).prfidx := io.uopIn.prfDest
    loadQueue(loadHead).vaddr := addr
    loadQueue(loadHead).paddr := addr
    loadQueue(loadHead).func := func
    loadQueue(loadHead).op := memop
    // loadQueue(loadHead).size := size
    loadQueue(loadHead).data := genWdata(io.wdata, size)
    loadQueue(loadHead).asrc := io.wdata // FIXIT
    loadQueue(loadHead).awidthW := atomWidthW // FIXIT
    loadQueue(loadHead).rfWen := io.uopIn.decode.ctrl.rfWen
    loadQueue(loadHead).isMMIO := false.B // FIXIT
    loadQueue(loadHead).valid := true.B
    loadQueue(loadHead).finished := false.B
    loadQueue(loadHead).loadPageFault := false.B
    loadQueue(loadHead).storePageFault := false.B
    loadQueue(loadHead).loadAddrMisaligned := findLoadAddrMisaligned
    loadQueue(loadHead).storeAddrMisaligned := findStoreAddrMisaligned
  }

  //-------------------------------------------------------
  // LSU Stage 1,2,3: mem req
  // Send request to TLB/Cache, and wait for response
  //-------------------------------------------------------

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

  // DTLB is ignored
  val loadDMemReq = Wire(new MemReq)
  val storeDMemReq = Wire(new MemReq)
  val tlbDMemReq = Wire(new MemReq)
  val noDMemReq = Wire(new MemReq)

  // TODO: refactor with utils.Arbitor

  val pageTableWalkerWorking = RegInit(false.B)
  val tlbReadygo   = tlbDMemReq.valid
  val storeReadygo = storeDMemReq.valid && !tlbDMemReq.valid && !pageTableWalkerWorking
  val loadReadygo  = loadDMemReq.valid && !tlbDMemReq.valid && !storeDMemReq.valid && !pageTableWalkerWorking && !storeQueueFull

  val memReq = Mux1H(List(
    tlbReadygo -> tlbDMemReq,
    loadReadygo -> loadDMemReq,
    storeReadygo -> storeDMemReq,
    (!tlbReadygo && !loadReadygo && !storeReadygo) -> noDMemReq
  ))

  val loadSideUserBundle = Wire(new DCacheUserBundle)
  // loadSideUserBundle.pc := io.uopIn.decode.cf.pc
  // loadSideUserBundle.prfidx := io.uopIn.decode.prfDest
  loadSideUserBundle.ldqidx := loadMid
  loadSideUserBundle.op := Mux(havePendingDemReq, loadQueue(loadMid).op, memop)
  // loadSideUserBundle.func := func
  // loadSideUserBundle.loadAddrMisaligned := loadAddrMisaligned
  // loadSideUserBundle.storeAddrMisaligned := storeAddrMisaligned

  // TODO
  val storeSideUserBundle = Wire(new DCacheUserBundle)
  // storeSideUserBundle.pc := io.uopIn.decode.cf.pc
  // storeSideUserBundle.prfidx := io.uopIn.decode.prfDest
  storeSideUserBundle.ldqidx := DontCare
  storeSideUserBundle.op := MEMOpID.storec
  // storeSideUserBundle.func := func
  // storeSideUserBundle.loadAddrMisaligned := loadAddrMisaligned
  // storeSideUserBundle.storeAddrMisaligned := storeAddrMisaligned

  val tlbSideUserBundle = Wire(new DCacheUserBundle)
  tlbSideUserBundle := DontCare
  tlbSideUserBundle.op := MEMOpID.idle // FIXIT

  // TODO: fix atom addr src

  // loadDMemReq
  // TODO: store should not access cache here
  loadDMemReq.addr := addr
  loadDMemReq.size := size
  loadDMemReq.wdata := genWdata(io.wdata, size)
  loadDMemReq.valid := io.in.fire() && MEMOpID.commitToCDB(memop)
  when(havePendingDemReq){
    loadDMemReq.addr := loadQueue(loadMid).vaddr
    loadDMemReq.size := loadQueue(loadMid).func(1,0)
    loadDMemReq.wdata := loadQueue(loadMid).data
    loadDMemReq.valid := true.B
  }
  loadDMemReq.wmask := genWmask(loadDMemReq.addr, loadDMemReq.size)
  loadDMemReq.cmd := SimpleBusCmd.read //TODO: only MEMOpID.needLoad(memop) need load
  loadDMemReq.user := loadSideUserBundle

  Debug(){
    when(dmem.req.fire() && MEMOpID.commitToCDB(opReq)){
      when(havePendingDemReq){
        printf("[LSU DREQ] pc 0x%x\n", loadQueue(loadMid).pc)
      }.otherwise{
        printf("[LSU DREQ] pc 0x%x\n", io.uopIn.decode.cf.pc)
      }
    }
  }

  // storeDMemReq
  // TODO: store queue
  storeDMemReq.addr := storeQueue(storeTail).vaddr //TODO: fixit
  storeDMemReq.size := storeQueue(storeTail).func(1,0)
  storeDMemReq.wdata := genWdata(storeQueue(storeTail).data, storeQueue(storeTail).func(1,0))
  storeDMemReq.wmask := genWmask(storeQueue(storeTail).vaddr, storeQueue(storeTail).func(1,0))
  storeDMemReq.cmd := SimpleBusCmd.write
  storeDMemReq.user := storeSideUserBundle
  storeDMemReq.valid := haveUnrequiredStore

  // tlbDMemReq
  // TODO
  tlbDMemReq := DontCare
  tlbDMemReq.valid := false.B

  // noDMemReq
  noDMemReq := DontCare
  noDMemReq.valid := false.B

  Debug(){
    printf("[DREQ] addr %x, size %x, wdata %x, wmask %x, cmd %x, user %x %x %b, valid %x\n",
      memReq.addr, memReq.size, memReq.wdata, memReq.wmask, memReq.cmd, memReq.user.asUInt, memReq.user.ldqidx, memReq.user.op, memReq.valid
    )
  }

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

  // Page Walker FSM
  // TODO

  //-------------------------------------------------------
  // Mem Resp
  //-------------------------------------------------------

  // if TLB fail, Page Walker FSM takes control

  // Out of order dequeue
  // TBD

  // Load Queue dequeue
  // In-order dequeue
  // If TLB hit, dequeue inst will be marked as finished in Load Queue
  // If TLB miss, reset Load Queue Pointer
  // If it is a valid store inst, add it to store queue
  // If an inst is marked as `finished`, it will be commited to CDB in the next cycle

  val vaddrBack = DontCare //TODO
  val paddrBack = loadQueue(dmemUserOut.ldqidx).paddr //DontCare //TODO

  // MMIO check
  val lsuMMIO = WireInit(false.B)
  BoringUtils.addSink(lsuMMIO, "lsuMMIO")
  //dmem.resp.bits.user.asTypeOf(DCacheUserBundle).isMMIO
  //dmem.resp.bits.isMMIO

  // Store addr backward match
  // If match, get data from store queue, and mark that inst as resped
  val dataBackVec = Wire(Vec(XLEN/8, (UInt((XLEN/8).W))))
  val dataBack = dataBackVec.asUInt
  val forwardVec = VecInit(List.tabulate(storeQueueSize)(i => {
    i.U < storeHead && paddrBack === storeQueue(i).paddr
  }))
  for(j <- (0 to (XLEN/8 - 1))){
    dataBackVec(j) := MuxCase( 
      default = dmem.resp.bits.rdata(8*(j+1)-1, 8*j), 
      mapping = List.tabulate(storeQueueSize)(i => {
        (forwardVec(i) && storeQueue(i).wmask(j), storeQueue(i).data(8*(j+1)-1, 8*j))
      })
    )
  }

  // write back to load queue
  when(dmem.resp.fire()){
    when(MEMOpID.commitToCDB(opResp)){
      when(!MEMOpID.needStore(opResp)){loadQueue(ldqidxResp).data := dataBack}
      loadQueue(ldqidxResp).finished := true.B
      loadQueue(ldqidxResp).isMMIO := lsuMMIO
      // loadQueue(ldqidxResp).paddr := paddr //TODO
    }
    when(MEMOpID.commitToVPU(opResp)){
      // TODO
    }
  }

  // write back to store queue
  val storeQueueEnqPtr = Mux(storeQueueDequeue, storeHead - 1.U, storeHead)
  when(storeQueueEnqueue){
    storeQueue(storeQueueEnqPtr).pc := loadQueue(ldqidxResp).pc
    storeQueue(storeQueueEnqPtr).prfidx := loadQueue(ldqidxResp).prfidx
    storeQueue(storeQueueEnqPtr).wmask := genWmask(loadQueue(ldqidxResp).vaddr, loadQueue(ldqidxResp).func(1,0))
    storeQueue(storeQueueEnqPtr).vaddr := loadQueue(ldqidxResp).vaddr
    storeQueue(storeQueueEnqPtr).paddr := loadQueue(ldqidxResp).paddr //TODO: replace it with TLB result
    storeQueue(storeQueueEnqPtr).func := loadQueue(ldqidxResp).func
    storeQueue(storeQueueEnqPtr).op := loadQueue(ldqidxResp).op
    storeQueue(storeQueueEnqPtr).data := loadQueue(ldqidxResp).data
    storeQueue(storeQueueEnqPtr).isMMIO := lsuMMIO //FIXIT: this is ugly
  }
  Debug(){
    when(storeQueueEnqueue){
      printf("[ENSTQ] pc %x ldqidx %x valid %x enqp %x head %x time %x\n", loadQueue(ldqidxResp).pc, ldqidxResp, loadQueue(ldqidxResp).valid, storeQueueEnqPtr, storeHead, GTimer())
    }
  }
  assert(!(storeQueueEnqueue && !loadQueue(ldqidxResp).valid))

  //-------------------------------------------------------
  // LSU Stage 4: Atom and CDB broadcast
  // Atom ALU gets data and writes its result to temp reg
  //-------------------------------------------------------

  // Atom
  val atomALU = Module(new AtomALU)
  atomALU.io.src1 := loadQueue(loadTail).data//atomMemReg
  atomALU.io.src2 := loadQueue(loadTail).asrc//io.wdata
  atomALU.io.func := loadQueue(loadTail).func//func
  atomALU.io.isWordOp := loadQueue(loadTail).awidthW //atomWidthW
  // When an atom inst reaches here, store its result to store buffer,
  // then commit it to CDB, set atom-on-the-fly to false
  io.atomData := atomALU.io.result

  // Commit to CDB
  
  // Load Data Selection
  val rdata = loadQueue(loadTail).data
  val rdataSel = LookupTree(loadQueue(loadTail).vaddr(2, 0), List(
    "b000".U -> rdata(63, 0),
    "b001".U -> rdata(63, 8),
    "b010".U -> rdata(63, 16),
    "b011".U -> rdata(63, 24),
    "b100".U -> rdata(63, 32),
    "b101".U -> rdata(63, 40),
    "b110".U -> rdata(63, 48),
    "b111".U -> rdata(63, 56)
  ))
  val rdataPartialLoad = LookupTree(loadQueue(loadTail).func, List(
      LSUOpType.lb   -> SignExt(rdataSel(7, 0) , XLEN),
      LSUOpType.lh   -> SignExt(rdataSel(15, 0), XLEN),
      LSUOpType.lw   -> SignExt(rdataSel(31, 0), XLEN),
      LSUOpType.ld   -> SignExt(rdataSel(63, 0), XLEN),
      LSUOpType.lbu  -> ZeroExt(rdataSel(7, 0) , XLEN),
      LSUOpType.lhu  -> ZeroExt(rdataSel(15, 0), XLEN),
      LSUOpType.lwu  -> ZeroExt(rdataSel(31, 0), XLEN)
  ))
  io.out.bits := rdataPartialLoad

  //TODO: other CDB wires
  io.uopOut := DontCare
  io.isMMIO := loadQueue(loadTail).isMMIO
  io.exceptionVec.map(_ := false.B)
  io.exceptionVec(loadPageFault) := false.B // TODO: fixit
  io.exceptionVec(storePageFault) := false.B // TODO: fixit
  io.exceptionVec(loadAddrMisaligned) := loadQueue(loadTail).loadAddrMisaligned
  io.exceptionVec(storeAddrMisaligned) := loadQueue(loadTail).storeAddrMisaligned
  io.uopOut.decode.cf.pc := loadQueue(loadTail).pc
  io.uopOut.prfDest := loadQueue(loadTail).prfidx
  io.uopOut.decode.ctrl.rfWen := loadQueue(loadTail).rfWen && !loadQueue(loadTail).loadAddrMisaligned && !loadQueue(loadTail).storeAddrMisaligned && !loadQueue(loadTail).loadPageFault && !loadQueue(loadTail).storePageFault

  io.in.ready := !loadQueueFull
  io.out.valid := havePendingCDBCmt

  when(io.flush){
    // FIXIT
    loadHead := loadTail + 1.U
    loadMid := loadTail + 1.U
    loadTail := loadTail + 1.U
    for(i <- 0 to (loadQueueSize - 1)){
      loadQueue(i).valid := false.B
      loadQueue(i).finished := false.B
    }
  }

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
      when(tlbReadygo){printf("tlbDMemReq")}
      when((!tlbReadygo && !loadReadygo && !storeReadygo)){printf("noDMemReq")}
      printf(" addr 0x%x size %x wdata %x cmd %x ldqidx %x memop %b\n", dmem.req.bits.addr, dmem.req.bits.size, dmem.req.bits.wdata, dmem.req.bits.cmd, memReq.user.ldqidx, memReq.user.op)
    }

    when(dmem.resp.fire()){
      printf("[LSU DRESP] data %x fwddata %x ldqidx %x memop %b\n", dmem.resp.bits.rdata, dataBack, dmemUserOut.ldqidx, dmemUserOut.op)
    }
  }

}
