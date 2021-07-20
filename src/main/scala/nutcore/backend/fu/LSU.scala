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
import top.Settings

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

  def needMemRead(func: UInt): Bool = isLoad(func) || isAMO(func) || isLR(func)
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
  val moqSize = 8
  val storeQueueSize = 8
}

class LSUIO extends FunctionUnitIO {
  val wdata = Input(UInt(XLEN.W))
  // val instr = Input(UInt(32.W)) // Atom insts need aq rl funct3 bit from instr // TODO
  val dmem = new SimpleBusUC(addrBits = VAddrBits, userBits = DCacheUserBundleWidth)
  val dtlb = new SimpleBusUC(addrBits = VAddrBits, userBits = DCacheUserBundleWidth)
  val mispredictRec = Flipped(new MisPredictionRecIO)
  val stMaskIn = Input(UInt(robSize.W))
  val robAllocate = Input(Valid(UInt(log2Up(robSize).W)))
  val uopIn = Input(new RenamedDecodeIO)
  val uopOut = Output(new RenamedDecodeIO)
  val isMMIO = Output(Bool())
  val exceptionVec = Output(Vec(ExceptionTypes, Bool()))
  val scommit = Input(Bool())
  val commitStoreToCDB = Output(Bool())
  val haveUnfinishedStore = Output(Bool())
  val flush = Input(Bool())
}

class StoreQueueEntry extends NutCoreBundle{
  val pc       = UInt(VAddrBits.W)
  val prfidx   = UInt(prfAddrWidth.W) // for debug
  val brMask   = UInt(checkpointSize.W)
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

class moqEntry extends NutCoreBundle{
  val pc       = UInt(VAddrBits.W)
  val isRVC    = Bool()
  val prfidx   = UInt(prfAddrWidth.W)
  val brMask   = UInt(checkpointSize.W)
  val stMask   = UInt(robSize.W)
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
  val rollback = Bool()
  val loadPageFault  = Bool()
  val storePageFault  = Bool()
  val loadAddrMisaligned  = Bool()
  val storeAddrMisaligned = Bool()
}

class DCacheUserBundle extends NutCoreBundle {
  val moqidx   = UInt(5.W) //TODO
  val op       = UInt(7.W)
}

class MemReq extends NutCoreBundle {
  val addr = UInt(VAddrBits.W)
  val size = UInt(2.W)
  val wdata = UInt(XLEN.W)
  val wmask = UInt(8.W)
  val cmd = UInt(4.W)
  val user = new DCacheUserBundle
  val valid = Bool()
}

class AtomALU extends NutCoreModule {
  val io = IO(new NutCoreBundle{
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

  io.result :=  Mux(io.isWordOp, SignExt(res(31,0), 64), res(XLEN-1,0))
}

// Out Of Order Load/Store Unit
class LSU extends NutCoreModule with HasLSUConst {
  val io = IO(new LSUIO)
  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt, dtlbPF: Bool): UInt = {
    this.valid := valid && !needMispredictionRecovery(io.uopIn.brMask)
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  def needMispredictionRecovery(brMask: UInt) = {
    io.mispredictRec.valid && io.mispredictRec.redirect.valid && brMask(io.mispredictRec.checkpoint)
  }

  def updateBrMask(brMask: UInt) = {
    brMask & ~ (UIntToOH(io.mispredictRec.checkpoint) & Fill(checkpointSize, io.mispredictRec.valid))
  }

  val dmem = io.dmem
  // Gen result
  val tlbRespmoqidx = io.dtlb.resp.bits.user.get.asTypeOf(new DCacheUserBundle).moqidx
  val dmemUserOut = dmem.resp.bits.user.get.asTypeOf(new DCacheUserBundle)
  val opResp = dmem.resp.bits.user.get.asTypeOf(new DCacheUserBundle).op
  val opReq = dmem.req.bits.user.get.asTypeOf(new DCacheUserBundle).op
  val moqidxResp = dmemUserOut.moqidx

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

  //                           Memory reOrder Queue
  // ---------------------------------------------------------------------------
  // |   not used   |   waittlb    |   waitmem    |   dmemreq   |   not used   |
  // ---------------------------------------------------------------------------
  //                |              |              |             |
  //              head            tlb            mem           tail

  val moq = RegInit(VecInit(Seq.fill(moqSize)(0.U.asTypeOf(new moqEntry)))) // TODO: replace it with utils.queue
  // Memory reOrder Queue contains Load, Store and TLB request
  // Store insts will access TLB before its result being commited to CDB
  // moq should be called 'loadStoreQueue' or 'memReOrderQueue', in some ways
  val moqHeadPtr  = RegInit(0.U((log2Up(moqSize)).W))
  val moqDtlbPtr  = RegInit(0.U((log2Up(moqSize)).W))
  val moqDmemPtr  = RegInit(0.U((log2Up(moqSize)).W))
  val moqTailPtr  = RegInit(0.U((log2Up(moqSize)).W))
  val moqFull = moqHeadPtr === (moqTailPtr - 1.U) //TODO: fix it with maybe_full logic
  val moqEmpty = moqHeadPtr === moqTailPtr //TODO: fix it with maybe_full logic
  val havePendingDtlbReq = moqDtlbPtr =/= moqHeadPtr
  val havePendingDmemReq = MEMOpID.needLoad(moq(moqDmemPtr).op) && !moq(moqDmemPtr).loadPageFault && !moq(moqDmemPtr).storePageFault && !moq(moqDmemPtr).loadAddrMisaligned && !moq(moqDmemPtr).storeAddrMisaligned && !moq(moqDmemPtr).finished && moq(moqDmemPtr).valid && moq(moqDmemPtr).tlbfin
  val havePendingStoreEnq = MEMOpID.needStore(moq(moqDmemPtr).op) && !MEMOpID.needAlu(moq(moqDmemPtr).op) && moq(moqDmemPtr).valid && moq(moqDmemPtr).tlbfin
  val havePendingAMOStoreEnq = MEMOpID.needAlu(moq(moqDmemPtr).op) && moq(moqDmemPtr).valid && moq(moqDmemPtr).tlbfin
  val dmemReqFrommoq = havePendingDmemReq || havePendingStoreEnq || havePendingAMOStoreEnq
  val skipAInst = !moq(moqDmemPtr).valid && moqDmemPtr =/= moqDtlbPtr || moq(moqDmemPtr).valid && moq(moqDmemPtr).tlbfin && (moq(moqDmemPtr).loadPageFault || moq(moqDmemPtr).storePageFault || moq(moqDmemPtr).loadAddrMisaligned || moq(moqDmemPtr).storeAddrMisaligned || !moq(moqDmemPtr).op(2,0).orR)
  val haveLoadResp = io.dmem.resp.fire() && MEMOpID.commitToCDB(opResp) && moq(moqidxResp).valid //FIXIT: to use non blocking dcache, set it to false
  val havePendingCDBCmt = (0 until moqSize).map(i => moq(i).finished && moq(i).valid).reduce(_ | _)
  val pendingCDBCmtSelect = PriorityEncoder(VecInit((0 until moqSize).map(i => moq(i).finished && moq(i).valid)))
  val writebackSelect = Wire(UInt(log2Up(moqSize).W))
  writebackSelect := Mux(haveLoadResp, moqidxResp, pendingCDBCmtSelect)
  // assert(!(moq(pendingCDBCmtSelect).valid && !MEMOpID.needStore(moq(pendingCDBCmtSelect).op) && MEMOpID.needLoad(moq(pendingCDBCmtSelect).op)))

  // load queue enqueue
  val moqEnqueue = io.in.fire()
  when(moqEnqueue){ moqHeadPtr := moqHeadPtr + 1.U }
  // move moqDtlbPtr
  val dtlbReqsend = io.dtlb.req.fire() || !dtlbEnable && moqEnqueue
  when(dtlbReqsend){ moqDtlbPtr := moqDtlbPtr + 1.U }
  // move moqDmemPtr
  val moqReqsend = dmem.req.fire() && MEMOpID.commitToCDB(opReq)
  val nextmoqDmemPtr = WireInit(moqDmemPtr)
  when(moqReqsend || storeQueueEnqueue || skipAInst){
    nextmoqDmemPtr := moqDmemPtr + 1.U
  }
  moqDmemPtr := nextmoqDmemPtr

  // load queue dequeue
  when(io.out.fire()){
    moq(writebackSelect).valid := false.B
    moq(writebackSelect).finished := true.B
  }

  when((moqTailPtr =/= moqDmemPtr) && !moq(moqTailPtr).valid && moq(moqTailPtr).finished){
    moqTailPtr := moqTailPtr + 1.U
    moq(moqTailPtr).valid := false.B
    moq(moqTailPtr).finished := false.B
  }

  // when branch, invalidate insts in wrong branch direction
  List.tabulate(moqSize)(i => {
    when(needMispredictionRecovery(moq(i).brMask)){
      moq(i).valid := false.B
    }
    moq(i).brMask := updateBrMask(moq(i).brMask) // fixit, AS WELL AS STORE BRMASK !!!!!
  })
  
  // write data to moq
  val vaddrIsMMIO = AddressSpace.isMMIO(addr)
  val paddrIsMMIO = AddressSpace.isMMIO(io.dtlb.resp.bits.rdata)

  when(moqEnqueue){
    moq(moqHeadPtr).pc := io.uopIn.decode.cf.pc
    moq(moqHeadPtr).isRVC := io.uopIn.decode.cf.isRVC
    moq(moqHeadPtr).prfidx := io.uopIn.prfDest
    moq(moqHeadPtr).brMask := updateBrMask(io.uopIn.brMask)
    moq(moqHeadPtr).stMask := io.stMaskIn
    moq(moqHeadPtr).vaddr := addr
    moq(moqHeadPtr).paddr := addr
    moq(moqHeadPtr).func := func
    moq(moqHeadPtr).size := size
    moq(moqHeadPtr).op := memop
    moq(moqHeadPtr).data := genWdata(io.wdata, size)
    moq(moqHeadPtr).fdata := 0.U
    moq(moqHeadPtr).fmask := 0.U
    moq(moqHeadPtr).asrc := io.wdata // FIXIT
    moq(moqHeadPtr).rfWen := io.uopIn.decode.ctrl.rfWen
    moq(moqHeadPtr).isMMIO := vaddrIsMMIO // FIXIT
    moq(moqHeadPtr).valid := true.B
    moq(moqHeadPtr).tlbfin := !dtlbEnable
    moq(moqHeadPtr).finished := false.B
    moq(moqHeadPtr).rollback := false.B
    moq(moqHeadPtr).loadPageFault := false.B
    moq(moqHeadPtr).storePageFault := false.B
    moq(moqHeadPtr).loadAddrMisaligned := findLoadAddrMisaligned
    moq(moqHeadPtr).storeAddrMisaligned := findStoreAddrMisaligned
  }

  when(storeQueueEnqueue || skipAInst){
    moq(moqDmemPtr).finished := true.B
    // store inst does not need to access mem until it is commited
  }
  
  Debug("[LSU MOQ] pc           id vaddr        paddr        func    op      data             mmio   valid   finished    exc     mask\n")
  for(i <- 0 until moqSize){
    Debug(
      "[LSU MOQ] 0x%x %x 0x%x 0x%x %b %b %x mmio:%b valid:%b finished:%b%b exc:%b%b%b%b %x %d", 
      moq(i).pc, moq(i).prfidx, moq(i).vaddr, moq(i).paddr, moq(i).func, moq(i).op, moq(i).data, moq(i).isMMIO, moq(i).valid, moq(i).tlbfin, moq(i).finished, moq(i).loadPageFault, moq(i).storePageFault, moq(i).loadAddrMisaligned, moq(i).storeAddrMisaligned, moq(i).brMask, i.U
    )
    Debug(false, moq(i).valid, " valid")
    Debug(false, moqHeadPtr === i.U, " head")
    Debug(false, moqDtlbPtr === i.U, " tlb")
    Debug(false, moqDmemPtr === i.U, " mem")
    Debug(false, moqTailPtr === i.U, " tail")
    Debug(false, "\n")
  }

  Debug(moqEnqueue, "[ENMOQ] pc %x moqidx %x time %x\n", io.uopIn.decode.cf.pc, moqHeadPtr, GTimer())

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
  Debug(storeCmtPtr > storeHeadPtr, "retired store should be less than valid store\n")
    
  // assert(storeCmtPtr <= storeHeadPtr, "retired store should be less than valid store")

  // alloc a slot when a store tlb request is sent
  // val storeQueueAlloc = dmem.req.fire() && MEMOpID.commitToCDB(opReq) && MEMOpID.needStore(opReq)
  // after a store inst get its paddr from TLB, add it to store queue
  val dtlbRespUser = io.dtlb.resp.bits.user.get.asTypeOf(new DCacheUserBundle)
  val tlbRespStoreEnq = false.B //io.dtlb.resp.fire() && MEMOpID.needStore(dtlbRespUser.op) && !MEMOpID.needAlu(dtlbRespUser.op) && !bruFlush && moq(dtlbRespUser.moqidx).valid && tlbRespmoqidx === moqDmemPtr
  storeQueueEnqueue := havePendingStoreEnq && !storeQueueFull || !havePendingDmemReq && tlbRespStoreEnq && !storeQueueFull
  val tlbRespAMOStoreEnq = false.B //io.dtlb.resp.fire() && MEMOpID.needAlu(dtlbRespUser.op) && !bruFlush && moq(dtlbRespUser.moqidx).valid && tlbRespmoqidx === moqDmemPtr
  val storeQueueAMOEnqueue = havePendingAMOStoreEnq && moqReqsend || tlbRespAMOStoreEnq && moqReqsend
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
    storeQueue(storeQueueSize-1).valid := false.B
  }

  // move storeCmtPtr ptr
  nextStoreCmtPtr := storeCmtPtr
  when((storeQueueDequeue && !storeQueueSkipInst) && !storeQueueConfirm){nextStoreCmtPtr := storeCmtPtr - 1.U}
  when(!(storeQueueDequeue && !storeQueueSkipInst) && storeQueueConfirm){nextStoreCmtPtr := storeCmtPtr + 1.U}
  storeCmtPtr := nextStoreCmtPtr
  
  // move storeHeadPtr ptr
  when(storeQueueDequeue && !(storeQueueEnqueue || storeQueueAMOEnqueue)){storeHeadPtr := storeHeadPtr - 1.U}
  when(!storeQueueDequeue && (storeQueueEnqueue || storeQueueAMOEnqueue)){storeHeadPtr := storeHeadPtr + 1.U}
  val flushStoreHeadPtr = PriorityMux(
    (nextStoreCmtPtr === 0.U) +: (0 until storeQueueSize).map(i => {
      PopCount(VecInit((0 to i).map(j => storeQueue(j).valid))) === nextStoreCmtPtr
    }),
    (0 to storeQueueSize).map(i => i.U)
  )
  when(io.flush){storeHeadPtr := flushStoreHeadPtr}

  assert(!(storeQueueEnqueue && storeQueueAMOEnqueue))

  // when branch, invalidate insts in wrong branch direction
  List.tabulate(storeQueueSize)(i => {
    when(storeQueueDequeue){
      when(needMispredictionRecovery(storeQueue(i).brMask)){
        if(i != 0){ storeQueue(i-1).valid := false.B }
      }
      if(i != 0){ storeQueue(i-1).brMask := updateBrMask(storeQueue(i).brMask) }
    }.otherwise{
      when(needMispredictionRecovery(storeQueue(i).brMask)){
        storeQueue(i).valid := false.B
      }
      storeQueue(i).brMask := updateBrMask(storeQueue(i).brMask)
    }
  })

  // write data to store queue
  val storeQueueEnqPtr = Mux(storeQueueDequeue, storeHeadPtr - 1.U, storeHeadPtr)
  val havePendingStqEnq = havePendingStoreEnq || havePendingAMOStoreEnq
  val storeQueueEnqSrcPick = Mux(havePendingStqEnq, moqDmemPtr, dtlbRespUser.moqidx)
  when(storeQueueEnqueue || storeQueueAMOEnqueue){
    storeQueue(storeQueueEnqPtr).pc := moq(storeQueueEnqSrcPick).pc
    storeQueue(storeQueueEnqPtr).prfidx := moq(storeQueueEnqSrcPick).prfidx
    storeQueue(storeQueueEnqPtr).brMask := updateBrMask(moq(storeQueueEnqSrcPick).brMask)
    storeQueue(storeQueueEnqPtr).wmask := genWmask(moq(storeQueueEnqSrcPick).vaddr, moq(moqDmemPtr).size)
    storeQueue(storeQueueEnqPtr).vaddr := moq(storeQueueEnqSrcPick).vaddr
    storeQueue(storeQueueEnqPtr).paddr := Mux(havePendingStqEnq, moq(moqDmemPtr).paddr, io.dtlb.resp.bits.rdata)
    storeQueue(storeQueueEnqPtr).func := moq(storeQueueEnqSrcPick).func
    storeQueue(storeQueueEnqPtr).size := moq(storeQueueEnqSrcPick).size
    storeQueue(storeQueueEnqPtr).op := moq(storeQueueEnqSrcPick).op
    storeQueue(storeQueueEnqPtr).data := moq(storeQueueEnqSrcPick).data
    storeQueue(storeQueueEnqPtr).isMMIO := Mux(havePendingStqEnq, moq(moqDmemPtr).isMMIO, paddrIsMMIO)
    storeQueue(storeQueueEnqPtr).valid := true.B && !(needMispredictionRecovery(moq(storeQueueEnqSrcPick).brMask))
  }

  // detect unfinished uncache store
  // When EnableOutOfOrderMemAccess, ignore uncached mem access seq,
  // as current oo-mem-arch does not support maintain program order and idempotency for uncached mem access.
  val haveUnfinishedUCStore = if(EnableOutOfOrderMemAccess){ false.B } else { VecInit(storeQueue.map(i => i.isMMIO && i.valid)).asUInt.orR }

  // For debug
  val storeTBCV = io.dmem.req.fire() && io.dmem.req.bits.cmd === SimpleBusCmd.write
  val storeTBC = WireInit(storeQueue(0.U).pc)
  BoringUtils.addSource(storeTBCV, "GSPCV")
  BoringUtils.addSource(storeTBC, "GSPC")

  Debug(storeQueueEnqueue || storeQueueAMOEnqueue, "[ENSTQ] pc %x moqidx %x valid %x enqp %x head %x\n", moq(moqidxResp).pc, moqidxResp, moq(moqidxResp).valid, storeQueueEnqPtr, storeHeadPtr)

  // printf("[PSTQ] time %x alloc %x head %x mid %x flush %x\n", GTimer(), storeAlloc, storeHeadPtr, storeCmtPtr, io.flush)

  Debug("[LSU STQ] pc           id vaddr        paddr        func    op      data             mmio v mask \n")
  for(i <- 0 to (storeQueueSize - 1)){
    Debug(
      "[LSU STQ] 0x%x %x 0x%x 0x%x %b %b %x mmio:%b %b %x %d", 
      storeQueue(i).pc, storeQueue(i).prfidx, storeQueue(i).vaddr, storeQueue(i).paddr, storeQueue(i).func, storeQueue(i).op, storeQueue(i).data, storeQueue(i).isMMIO, storeQueue(i).valid, storeQueue(i).brMask, i.U
    )
    // Debug(storeAlloc === i.U, " alloc")
    Debug(storeHeadPtr === i.U, " head")
    Debug(storeCmtPtr === i.U, " cmt")
    // Debug(storeReqPtr === i.U, " req")
    Debug("\n")
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
  dtlbUserBundle.moqidx :=  Mux(havePendingDtlbReq, moqDtlbPtr, moqHeadPtr)
  dtlbUserBundle.op := Mux(havePendingDtlbReq, moq(moqDtlbPtr).op, memop)
  val pfType = Mux(havePendingDtlbReq, LSUOpType.needMemWrite(moq(moqDtlbPtr).func), LSUOpType.needMemWrite(func)) // 0: load pf 1: store pf

  io.dtlb.req.bits.apply(
    addr = Mux(havePendingDtlbReq, moq(moqDtlbPtr).vaddr(VAddrBits-1, 0), addr(VAddrBits-1, 0)), 
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
  Debug(io.flush, "[DTLB FLUSH]\n")
  Debug(io.dtlb.req.fire(), "[DTLB REQ] %d: req paddr for %x MOQid %x\n", GTimer(), io.dtlb.req.bits.addr, moqDtlbPtr)
  Debug(io.dtlb.resp.fire(), "[DTLB RESP] %d: get paddr: %x for %x MOQid %x exc l %b s %b\n", GTimer(), io.dtlb.resp.bits.rdata, moq(tlbRespmoqidx).vaddr, tlbRespmoqidx, loadPF, storePF)
  when(io.dtlb.resp.fire()){
    moq(tlbRespmoqidx).paddr := io.dtlb.resp.bits.rdata // FIXIT
    moq(tlbRespmoqidx).tlbfin := true.B
    moq(tlbRespmoqidx).isMMIO := paddrIsMMIO
    moq(tlbRespmoqidx).loadPageFault := loadPF
    moq(tlbRespmoqidx).storePageFault := storePF
  }
  // assert(moq(tlbRespmoqidx).valid && !moq(tlbRespmoqidx).tlbfin || !io.dtlb.resp.fire())
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
  val loadDMemReqSrcPick = Mux(havePendingDmemReq, moqDmemPtr, io.dtlb.resp.bits.user.get.asTypeOf(new DCacheUserBundle).moqidx)
  val loadDTlbRespReqValid = false.B //dtlbEnable && io.dtlb.resp.fire() && MEMOpID.needLoad(dtlbRespUser.op) && !dmemReqFrommoq && 
    //!loadPF && !storePF && !moq(tlbRespmoqidx).loadAddrMisaligned && !moq(tlbRespmoqidx).storeAddrMisaligned &&
    //!bruFlush && moq(tlbRespmoqidx).valid && tlbRespmoqidx === moqDmemPtr
  val loadSideUserBundle = Wire(new DCacheUserBundle)

  val atomData = Wire(UInt(XLEN.W))
  
  loadSideUserBundle.moqidx := loadDMemReqSrcPick
  loadSideUserBundle.op := moq(loadDMemReqSrcPick).op

  loadDMemReq.addr := Mux(havePendingDmemReq, moq(moqDmemPtr).paddr, io.dtlb.resp.bits.rdata)
  loadDMemReq.size := moq(loadDMemReqSrcPick).size
  loadDMemReq.wdata := moq(loadDMemReqSrcPick).data
  loadDMemReq.valid := (havePendingDmemReq || loadDTlbRespReqValid) && !haveUnfinishedUCStore //FIXME: only work for seq mem asscess
  loadDMemReq.wmask := genWmask(loadDMemReq.addr, loadDMemReq.size)
  loadDMemReq.cmd := SimpleBusCmd.read
  loadDMemReq.user := loadSideUserBundle

  // storeDMemReq
  val storeSideUserBundle = Wire(new DCacheUserBundle)
  storeSideUserBundle.moqidx := DontCare
  storeSideUserBundle.op := MEMOpID.storec

  storeDMemReq.addr := storeQueue(0.U).paddr
  storeDMemReq.size := storeQueue(0.U).size
  storeDMemReq.wdata := genWdata(storeQueue(0.U).data, storeQueue(0.U).size)
  storeDMemReq.wmask := storeQueue(0.U).wmask
  storeDMemReq.cmd := SimpleBusCmd.write
  storeDMemReq.user := storeSideUserBundle
  storeDMemReq.valid := haveUnrequiredStore
  when(LSUOpType.isAMO(storeQueue(0.U).func)){
    storeDMemReq.wdata := atomData
  }

  // noDMemReq
  noDMemReq := DontCare
  noDMemReq.valid := false.B

  // Debug(){
    // printf("[DREQ] addr %x, size %x, wdata %x, wmask %x, cmd %x, user %x %x %b, valid %x\n",
    //   memReq.addr, memReq.size, memReq.wdata, memReq.wmask, memReq.cmd, memReq.user.asUInt, memReq.user.moqidx, memReq.user.op, memReq.valid
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

  // Store addr forward match
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

  //                    Load Address Lookup Table
  //                      "Load Address Queue"
  // 
  // Store addr backward match
  // TODO: opt timing
  val robLoadInstVec = WireInit(0.U(robSize.W))
  BoringUtils.addSink(robLoadInstVec, "ROBLoadInstVec")
  val ldAddr = Reg(Vec(robSize, UInt((PAddrBits-3).W)))
  val ldDataMask = Reg(Vec(robSize, UInt((PAddrBits-3).W)))
  val ldStmask = Reg(Vec(robSize, UInt(robSize.W)))
  val reqPrfidx = moq(loadDMemReqSrcPick).prfidx
  val reqRobidx = reqPrfidx(prfAddrWidth-1, 1)
  when(io.robAllocate.valid){
    ldStmask(io.robAllocate.bits) := 0.U
  }
  when(io.dmem.req.fire() && MEMOpID.needLoad(opReq)){
    ldAddr(reqRobidx) := io.dmem.req.bits.addr(PAddrBits-1, 3)
    ldStmask(reqRobidx) := moq(loadDMemReqSrcPick).stMask
    ldDataMask(reqRobidx) := genWmask(io.dmem.req.bits.addr, moq(loadDMemReqSrcPick).size)
  }
  Debug(io.dmem.req.fire() && MEMOpID.needLoad(opReq), "[LSU] add load to MOQ pc:%x stmask:%x\n",io.dmem.req.bits.addr(PAddrBits-1, 3), moq(reqRobidx).stMask)
  
  val storeNeedRollback = (0 until robSize).map(i =>{
    ldAddr(i) === Mux(havePendingStqEnq, moq(moqDmemPtr).paddr(PAddrBits-1, 3), io.dtlb.resp.bits.rdata(PAddrBits-1, 3)) &&
    (ldDataMask(i) & genWmask(moq(storeQueueEnqSrcPick).vaddr, moq(moqDmemPtr).size)).orR &&
    ldStmask(i)(moq(storeQueueEnqSrcPick).prfidx(prfAddrWidth-1,1)) &&
    robLoadInstVec(i)
  }).reduce(_ || _)
  when(storeQueueEnqueue && storeNeedRollback){
    moq(storeQueueEnqSrcPick).rollback := true.B
    // printf("%d: Rollback detected at pc %x vaddr %x\n", GTimer(), moq(storeQueueEnqSrcPick).pc, moq(storeQueueEnqSrcPick).vaddr)
  }
  
  // Debug(storeQueueEnqueue, "store backward pc %x lvec %b rob %x addrtag %x\n",moq(storeQueueEnqSrcPick).pc, robLoadInstVec, moq(storeQueueEnqSrcPick).prfidx(prfAddrWidth-1,1), Mux(havePendingStqEnq, moq(moqDmemPtr).paddr(PAddrBits-1, 3), io.dtlb.resp.bits.rdata(PAddrBits-1, 3)))
  // (0 until robSize).map(i =>Debug(storeQueueEnqueue, "%x %x %x "+i+"\n",ldAddr(i),ldStmask(i),robLoadInstVec(i)))
  
  // write back to load queue
  when(dmem.req.fire() && MEMOpID.needLoad(opReq)){
    moq(moqDmemPtr).fdata := dataBack
    moq(moqDmemPtr).fmask := forwardWmask
  }

  val rdataFwdSelVec = Wire(Vec(XLEN/8, (UInt((XLEN/8).W))))
  val rdataFwdSel = rdataFwdSelVec.asUInt
  for(j <- (0 until (XLEN/8))){
    rdataFwdSelVec(j) := Mux(moq(moqidxResp).fmask(j), moq(moqidxResp).fdata(8*(j+1)-1, 8*j), dmem.resp.bits.rdata(8*(j+1)-1, 8*j))
  }

  when(dmem.resp.fire()){
    when(MEMOpID.commitToCDB(opResp) || MEMOpID.commitToVPU(opResp)){
      when(MEMOpID.needLoad(opResp)){moq(moqidxResp).data := rdataFwdSel}
      moq(moqidxResp).finished := true.B
    }
  }

  //-------------------------------------------------------
  // LSU Stage 4: Atom and CDB broadcast
  // Atom ALU gets data and writes its result to temp reg
  //-------------------------------------------------------

  // Load Data Selection
  val rdata = rdataFwdSel
  val rdataSel = LookupTree(moq(moqidxResp).vaddr(2, 0), List(
    "b000".U -> rdata(63, 0),
    "b001".U -> rdata(63, 8),
    "b010".U -> rdata(63, 16),
    "b011".U -> rdata(63, 24),
    "b100".U -> rdata(63, 32),
    "b101".U -> rdata(63, 40),
    "b110".U -> rdata(63, 48),
    "b111".U -> rdata(63, 56)
  ))
  val rdataPartialLoad = LookupTree(moq(moqidxResp).func, List(
      LSUOpType.lb   -> SignExt(rdataSel(7, 0) , XLEN),
      LSUOpType.lh   -> SignExt(rdataSel(15, 0), XLEN),
      LSUOpType.lw   -> SignExt(rdataSel(31, 0), XLEN),
      LSUOpType.ld   -> SignExt(rdataSel(63, 0), XLEN),
      LSUOpType.lbu  -> ZeroExt(rdataSel(7, 0) , XLEN),
      LSUOpType.lhu  -> ZeroExt(rdataSel(15, 0), XLEN),
      LSUOpType.lwu  -> ZeroExt(rdataSel(31, 0), XLEN)
  ))
  val atomDataPartialLoad = Mux(moq(moqidxResp).size(0), SignExt(rdataSel(63, 0), XLEN), SignExt(rdataSel(31, 0), XLEN))

  // Atom
  val atomALU = Module(new AtomALU)
  atomALU.io.src1 := atomDataPartialLoad
  atomALU.io.src2 := moq(moqidxResp).asrc//io.wdata FIXIT: use a single reg
  atomALU.io.func := moq(moqidxResp).func
  atomALU.io.isWordOp := !moq(moqidxResp).size(0)  //recover atomWidthW from size
  // When an atom inst reaches here, store its result to store buffer,
  // then commit it to CDB, set atom-on-the-fly to false
  val atomDataReg = RegEnable(genWdata(atomALU.io.result, moq(moqidxResp).size), io.out.fire() && LSUOpType.isAMO(moq(moqidxResp).func))
  atomData := atomDataReg

  // Commit to CDB
  io.out.bits := MuxCase(
      default = rdataPartialLoad,
      mapping = List(
        (moq(writebackSelect).loadPageFault || moq(writebackSelect).storePageFault || moq(writebackSelect).loadAddrMisaligned || moq(writebackSelect).storeAddrMisaligned) -> moq(writebackSelect).vaddr,
        LSUOpType.isSC(moq(writebackSelect).func) -> !MEMOpID.needStore(moq(writebackSelect).op),
        LSUOpType.isAtom(moq(writebackSelect).func) -> atomDataPartialLoad
      )
  )

  // Debug(LSUOpType.isAMO(moq(moqTailPtr).func) && io.out.fire(), "[AMO] %d: pc %x %x %x func %b word %b op %b res %x addr %x:%x\n", GTimer(), moq(moqTailPtr).pc, atomALU.io.src1, atomALU.io.src2, moq(moqTailPtr).func, atomALU.io.isWordOp, moq(moqTailPtr).op, atomALU.io.result, moq(moqTailPtr).vaddr, moq(moqTailPtr).paddr)

  io.uopOut := DontCare
  io.isMMIO := moq(writebackSelect).isMMIO
  io.exceptionVec.map(_ := false.B)
  io.exceptionVec(loadPageFault) := moq(writebackSelect).loadPageFault
  io.exceptionVec(storePageFault) := moq(writebackSelect).storePageFault
  io.exceptionVec(loadAddrMisaligned) := moq(writebackSelect).loadAddrMisaligned
  io.exceptionVec(storeAddrMisaligned) := moq(writebackSelect).storeAddrMisaligned
  io.uopOut.decode.cf.pc := moq(writebackSelect).pc
  io.uopOut.prfDest := moq(writebackSelect).prfidx
  io.uopOut.decode.ctrl.rfWen := moq(writebackSelect).rfWen && !moq(writebackSelect).loadAddrMisaligned && !moq(writebackSelect).storeAddrMisaligned && !moq(writebackSelect).loadPageFault && !moq(writebackSelect).storePageFault
  io.uopOut.decode.cf.redirect.valid := moq(writebackSelect).rollback
  io.uopOut.decode.cf.redirect.target := moq(writebackSelect).pc + Mux(moq(writebackSelect).isRVC, 2.U, 4.U)
  io.uopOut.decode.cf.redirect.rtype := 0.U // do not redirect until that inst is being commited
  io.commitStoreToCDB := MEMOpID.needStore(moq(writebackSelect).op)

  io.in.ready := !moqFull
  io.out.valid := havePendingCDBCmt || haveLoadResp
  assert(!(io.out.valid && !io.out.ready))

  when(io.flush){
    moqHeadPtr := nextmoqDmemPtr
    moqDtlbPtr := nextmoqDmemPtr
    for(i <- 0 to (moqSize - 1)){
      moq(i).valid := false.B
      moq(i).tlbfin := false.B
    }
  }

  Debug(io.out.fire() && io.uopOut.decode.cf.redirect.valid, "rollback at pc %x\n", moq(writebackSelect).pc)

  BoringUtils.addSource(io.out.fire() && io.isMMIO, "perfCntCondMmmioInstr")
  BoringUtils.addSource(moqFull, "perfCntCondMmemqFull")
  BoringUtils.addSource(storeQueueFull, "perfCntCondMstqFull")
  BoringUtils.addSource(io.in.fire() && MEMOpID.needLoad(memop), "perfCntCondMloadCnt")
  BoringUtils.addSource(io.in.fire() && MEMOpID.needStore(memop), "perfCntCondMstoreCnt")
  BoringUtils.addSource(dmem.req.fire() && MEMOpID.needLoad(opReq) && forwardWmask.orR, "perfCntCondMmemSBL")
  BoringUtils.addSource(storeQueueFull, "perfCntCondMpendingLS")
  BoringUtils.addSource(storeQueueFull, "perfCntCondMpendingSCmt")
  BoringUtils.addSource(storeQueueFull, "perfCntCondMpendingSReq")
  val MOQValid = VecInit((0 until moqSize).map(i => moq(i).valid))
  BoringUtils.addSource(PopCount(MOQValid.asUInt), "perfCntSrcMpendingLS")
  BoringUtils.addSource(storeCmtPtr, "perfCntSrcMpendingSCmt")
  BoringUtils.addSource(storeHeadPtr, "perfCntSrcMpendingSReq")

  val reqpc = Mux(storeReadygo, storeQueue(0.U).pc, Mux(havePendingDmemReq, moq(moqDmemPtr).pc, Mux(dtlbEnable, moq(dtlbRespUser.moqidx).pc, moq(dtlbRespUser.moqidx).pc)))
  Debug("[DMEM] req v %x r %x addr %x data %x op %b id %x  resp v %x r %x data %x op %b id %x\n",
    dmem.req.valid, dmem.req.ready, dmem.req.bits.addr, dmem.req.bits.wdata, dmem.req.bits.user.get.asTypeOf(new DCacheUserBundle).op, dmem.req.bits.user.get.asTypeOf(new DCacheUserBundle).moqidx,
    dmem.resp.valid, dmem.resp.ready, dmem.resp.bits.rdata, dmem.resp.bits.user.get.asTypeOf(new DCacheUserBundle).op, dmem.resp.bits.user.get.asTypeOf(new DCacheUserBundle).moqidx
  )
  when(dmem.req.fire()){
    Debug("[DREQ] ")
    Debug(false, loadReadygo, "loadDMemReq")
    Debug(false, storeReadygo, "storeDMemReq")
    Debug(false, !loadReadygo && !storeReadygo, "noDMemReq")
    Debug(false, " pc %x addr 0x%x size %x wdata %x cmd %x moqidx %x memop %b spending %x lpending %x time %d\n", reqpc, dmem.req.bits.addr, dmem.req.bits.size, dmem.req.bits.wdata, dmem.req.bits.cmd, memReq.user.moqidx, memReq.user.op, storeCmtPtr - 0.U, moqHeadPtr - moqDmemPtr, GTimer())
  }
  Debug(dmem.resp.fire(), "[DRESP] memdata %x fwddata %x:%b data %x moqidx %x memop %b isMMIO %x time %d\n", dmem.resp.bits.rdata, moq(dmemUserOut.moqidx).fdata, moq(dmemUserOut.moqidx).fmask, rdataFwdSel, dmemUserOut.moqidx, dmemUserOut.op, lsuMMIO, GTimer())
  Debug(dmem.req.fire() && !MEMOpID.needStore(opReq) && forwardWmask.orR, "[FWD] dataBack %x forwardWmask %b\n", dataBack, forwardWmask)

  val printMemTrace = false
  val addrTrack = List(
    "h12345678".U
  )

  when(printMemTrace.B || addrTrack.map(i => dmem.req.bits.addr === i).foldRight(false.B)((sum, i) => sum | i)){
    when(dmem.req.fire()){
      Debug("DREQ TRACE] ")
      Debug(false, loadReadygo, "loadDMemReq")
      Debug(false, storeReadygo, "storeDMemReq")
      Debug(false, (!loadReadygo && !storeReadygo), "noDMemReq")
      Debug(" pc %x addr 0x%x size %x wdata %x cmd %x moqidx %x memop %b spending %x lpending %x\n", reqpc, dmem.req.bits.addr, dmem.req.bits.size, dmem.req.bits.wdata, dmem.req.bits.cmd, memReq.user.moqidx, memReq.user.op, storeCmtPtr - 0.U, moqHeadPtr - moqDmemPtr)
  }}
    Debug(dmem.resp.fire(), "[LSU DRESP TRACE] memdata %x fwddata %x:%b data %x moqidx %x memop %b isMMIO %x\n", dmem.resp.bits.rdata, moq(dmemUserOut.moqidx).fdata, moq(dmemUserOut.moqidx).fmask, rdataFwdSel, dmemUserOut.moqidx, dmemUserOut.op, lsuMMIO)
    Debug(dmem.req.fire() && !MEMOpID.needStore(opReq) && forwardWmask.orR, "[LSU FWD TRACE] dataBack %x forwardWmask %b\n", dataBack, forwardWmask)
}
