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

class UnpipeLSUIO extends FunctionUnitIO {
  val wdata = Input(UInt(XLEN.W))
  val instr = Input(UInt(32.W)) // Atom insts need aq rl funct3 bit from instr
  val dmem = new SimpleBusUC(addrBits = VAddrBits)
  val isMMIO = Output(Bool())
  val dtlbPF = Output(Bool()) // TODO: refactor it for new backend
  val loadAddrMisaligned = Output(Bool()) // TODO: refactor it for new backend
  val storeAddrMisaligned = Output(Bool()) // TODO: refactor it for new backend
}

class UnpipelinedLSU extends NutCoreModule with HasLSUConst {
  val io = IO(new UnpipeLSUIO)
  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt, dtlbPF: Bool): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    dtlbPF := io.dtlbPF
    io.out.bits
  }
    val lsExecUnit = Module(new LSExecUnit)
    lsExecUnit.io.instr := DontCare
    io.dtlbPF := lsExecUnit.io.dtlbPF

    val storeReq = valid & LSUOpType.isStore(func)
    val loadReq  = valid & LSUOpType.isLoad(func)
    val atomReq  = valid & LSUOpType.isAtom(func)
    val amoReq   = valid & LSUOpType.isAMO(func)
    val lrReq   = valid & LSUOpType.isLR(func)
    val scReq   = valid & LSUOpType.isSC(func)
    if (Settings.get("HasDTLB")) {
      BoringUtils.addSource(amoReq, "ISAMO")
    }
    BoringUtils.addSource(amoReq, "ISAMO2")

    val aq = io.instr(26)
    val rl = io.instr(25)
    val funct3 = io.instr(14, 12)

    val atomWidthW = !funct3(0)
    val atomWidthD = funct3(0)

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

    // PF signal from TLB
    val dtlbFinish = WireInit(false.B)
    val dtlbPF = WireInit(false.B)
    val dtlbEnable = WireInit(false.B)
    BoringUtils.addSink(dtlbFinish, "DTLBFINISH")
    BoringUtils.addSink(dtlbPF, "DTLBPF")
    BoringUtils.addSink(dtlbEnable, "DTLBENABLE")

    // LSU control FSM state
    val s_idle :: s_exec :: s_load :: s_lr :: s_sc :: s_amo_l :: s_amo_a :: s_amo_s :: Nil = Enum(8)

    // LSU control FSM
    val state = RegInit(s_idle)
    val atomMemReg = Reg(UInt(XLEN.W))
    val atomRegReg = Reg(UInt(XLEN.W))
    val atomALU = Module(new AtomALU)
    atomALU.io.src1 := atomMemReg
    atomALU.io.src2 := io.wdata
    atomALU.io.func := func
    atomALU.io.isWordOp := atomWidthW

    val addr = if(IndependentAddrCalcState){RegNext(src1 + src2, state === s_idle)}else{DontCare}
    
    // StoreQueue
    // TODO: inst fence needs storeQueue to be finished
    // val enableStoreQueue = EnableStoreQueue // StoreQueue is disabled for page fault detection
    // if(enableStoreQueue){
    //   val storeQueue = Module(new Queue(new StoreQueueEntry, 4))
    //   storeQueue.io.enq.valid := state === s_idle && storeReq
    //   storeQueue.io.enq.bits.src1 := src1
    //   storeQueue.io.enq.bits.src2 := src2
    //   storeQueue.io.enq.bits.wdata := io.wdata
    //   storeQueue.io.enq.bits.func := func
    //   storeQueue.io.deq.ready := lsExecUnit.io.out.fire()
    // }
    
    lsExecUnit.io.in.valid     := false.B
    lsExecUnit.io.out.ready    := DontCare
    lsExecUnit.io.in.bits.src1 := DontCare
    lsExecUnit.io.in.bits.src2 := DontCare
    lsExecUnit.io.in.bits.func := DontCare
    lsExecUnit.io.wdata        := DontCare
    io.out.valid               := false.B
    io.in.ready                := false.B

    switch (state) {
      is(s_idle){ // calculate address 
        lsExecUnit.io.in.valid     := false.B
        lsExecUnit.io.out.ready    := DontCare 
        lsExecUnit.io.in.bits.src1 := DontCare
        lsExecUnit.io.in.bits.src2 := DontCare
        lsExecUnit.io.in.bits.func := DontCare
        lsExecUnit.io.wdata        := DontCare
        io.in.ready                := false.B || scInvalid
        io.out.valid               := false.B || scInvalid
        when(valid){state := s_exec}

        if(!IndependentAddrCalcState){
          lsExecUnit.io.in.valid     := io.in.valid && !atomReq
          lsExecUnit.io.out.ready    := io.out.ready 
          lsExecUnit.io.in.bits.src1 := src1 + src2
          lsExecUnit.io.in.bits.src2 := DontCare
          lsExecUnit.io.in.bits.func := func
          lsExecUnit.io.wdata        := io.wdata
          io.in.ready                := lsExecUnit.io.out.fire() || scInvalid
          io.out.valid               := lsExecUnit.io.out.valid  || scInvalid
          state := s_idle
        }

        when(amoReq){state := s_amo_l}
        when(lrReq){state := s_lr}
        when(scReq){state := Mux(scInvalid, s_idle, s_sc)}
        
      } 

      is(s_exec){
        lsExecUnit.io.in.valid     := true.B
        lsExecUnit.io.out.ready    := io.out.ready 
        lsExecUnit.io.in.bits.src1 := addr
        lsExecUnit.io.in.bits.src2 := DontCare
        lsExecUnit.io.in.bits.func := func
        lsExecUnit.io.wdata        := io.wdata
        io.in.ready                := lsExecUnit.io.out.fire() 
        io.out.valid               := lsExecUnit.io.out.valid  
        assert(!atomReq || !amoReq || !lrReq || !scReq)
        when(io.out.fire()){state := s_idle}
      }

      // is(s_load){
      //   lsExecUnit.io.in.valid     := true.B
      //   lsExecUnit.io.out.ready    := io.out.ready 
      //   lsExecUnit.io.in.bits.src1 := src1
      //   lsExecUnit.io.in.bits.src2 := src2
      //   lsExecUnit.io.in.bits.func := func
      //   lsExecUnit.io.wdata        := DontCare
      //   io.in.ready                := lsExecUnit.io.out.fire()
      //   io.out.valid               := lsExecUnit.io.out.valid
      //   when(lsExecUnit.io.out.fire()){state := s_idle}//load finished
      // }

      is(s_amo_l){
        lsExecUnit.io.in.valid     := true.B
        lsExecUnit.io.out.ready    := true.B 
        lsExecUnit.io.in.bits.src1 := src1
        lsExecUnit.io.in.bits.src2 := DontCare
        lsExecUnit.io.in.bits.func := Mux(atomWidthD, LSUOpType.ld, LSUOpType.lw)
        lsExecUnit.io.wdata        := DontCare
        io.in.ready                := false.B
        io.out.valid               := false.B
        when(lsExecUnit.io.out.fire()){
          state := s_amo_a; 
          Debug("[AMO-L] lsExecUnit.io.out.bits %x addr %x src2 %x\n", lsExecUnit.io.out.bits, lsExecUnit.io.in.bits.src1, io.wdata)
        }
        atomMemReg := lsExecUnit.io.out.bits
        atomRegReg := lsExecUnit.io.out.bits
      }

      is(s_amo_a){
        lsExecUnit.io.in.valid     := false.B
        lsExecUnit.io.out.ready    := false.B 
        lsExecUnit.io.in.bits.src1 := DontCare
        lsExecUnit.io.in.bits.src2 := DontCare
        lsExecUnit.io.in.bits.func := DontCare
        lsExecUnit.io.wdata        := DontCare
        io.in.ready                := false.B
        io.out.valid               := false.B
        state := s_amo_s
        atomMemReg := atomALU.io.result
        Debug("[AMO-A] src1 %x src2 %x res %x\n", atomMemReg, io.wdata, atomALU.io.result)
      }

      is(s_amo_s){
        lsExecUnit.io.in.valid     := true.B
        lsExecUnit.io.out.ready    := io.out.ready
        lsExecUnit.io.in.bits.src1 := src1
        lsExecUnit.io.in.bits.src2 := DontCare
        lsExecUnit.io.in.bits.func := Mux(atomWidthD, LSUOpType.sd, LSUOpType.sw)
        lsExecUnit.io.wdata        := atomMemReg
        io.in.ready                := lsExecUnit.io.out.fire()
        io.out.valid               := lsExecUnit.io.out.fire()
        when(lsExecUnit.io.out.fire()){
          state := s_idle; 
          Debug("[AMO-S] atomRegReg %x addr %x\n", atomRegReg, lsExecUnit.io.in.bits.src1)
        }
      }
      is(s_lr){
        lsExecUnit.io.in.valid     := true.B
        lsExecUnit.io.out.ready    := io.out.ready
        lsExecUnit.io.in.bits.src1 := src1
        lsExecUnit.io.in.bits.src2 := DontCare
        lsExecUnit.io.in.bits.func := Mux(atomWidthD, LSUOpType.ld, LSUOpType.lw)
        lsExecUnit.io.wdata        := DontCare
        io.in.ready                := lsExecUnit.io.out.fire()
        io.out.valid               := lsExecUnit.io.out.fire()
        when(lsExecUnit.io.out.fire()){
          state := s_idle; 
          Debug("[LR]\n")
        }
      }
      is(s_sc){
        lsExecUnit.io.in.valid     := true.B
        lsExecUnit.io.out.ready    := io.out.ready
        lsExecUnit.io.in.bits.src1 := src1
        lsExecUnit.io.in.bits.src2 := DontCare
        lsExecUnit.io.in.bits.func := Mux(atomWidthD, LSUOpType.sd, LSUOpType.sw)
        lsExecUnit.io.wdata        := io.wdata
        io.in.ready                := lsExecUnit.io.out.fire()
        io.out.valid               := lsExecUnit.io.out.fire()
        when(lsExecUnit.io.out.fire()){
          state := s_idle; 
          Debug("[SC] \n")
        }
      }
    }
    when(dtlbPF || io.loadAddrMisaligned || io.storeAddrMisaligned){
      state := s_idle
      io.out.valid := true.B
      io.in.ready := true.B
    }

  Debug(io.out.fire(), "[LSU-AGU] state %x inv %x inr %x\n", state, io.in.valid, io.in.ready)
    // controled by FSM 
    // io.in.ready := lsExecUnit.io.in.ready
    // lsExecUnit.io.wdata := io.wdata
    // io.out.valid := lsExecUnit.io.out.valid 

    //Set LR/SC bits
    setLr := io.out.fire() && (lrReq || scReq)
    setLrVal := lrReq
    setLrAddr := src1

    io.dmem <> lsExecUnit.io.dmem
    io.out.bits := Mux(scReq, scInvalid, Mux(state === s_amo_s, atomRegReg, lsExecUnit.io.out.bits))

    val lsuMMIO = WireInit(false.B)
    BoringUtils.addSink(lsuMMIO, "lsuMMIO")

    val mmioReg = RegInit(false.B)
    when (!mmioReg) { mmioReg := lsuMMIO }
    when (io.out.valid) { mmioReg := false.B }
    io.isMMIO := mmioReg && io.out.valid

    io.loadAddrMisaligned := lsExecUnit.io.loadAddrMisaligned
    io.storeAddrMisaligned := lsExecUnit.io.storeAddrMisaligned
}

class LSExecUnit extends NutCoreModule {
  val io = IO(new UnpipeLSUIO)

  val (valid, addr, func) = (io.in.valid, io.in.bits.src1, io.in.bits.func) // src1 is used as address
  def access(valid: Bool, addr: UInt, func: UInt): UInt = {
    this.valid := valid
    this.addr := addr
    this.func := func
    io.out.bits
  }

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

  def genWmask32(addr: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> 0x1.U, //0001 << addr(1:0)
      "b01".U -> 0x3.U, //0011
      "b10".U -> 0xf.U  //1111
    )) << addr(1, 0)
  }
  def genWdata32(data: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> Fill(4, data(7, 0)),
      "b01".U -> Fill(2, data(15, 0)),
      "b10".U -> data
    ))
  }

  val dmem = io.dmem
  val addrLatch = RegNext(addr)
  val isStore = valid && LSUOpType.isStore(func)
  val partialLoad = !isStore && (func =/= LSUOpType.ld)

  val s_idle :: s_wait_tlb :: s_wait_resp :: s_partialLoad :: Nil = Enum(4)
  val state = RegInit(s_idle)

  val dtlbFinish = WireInit(false.B)
  val dtlbPF = WireInit(false.B)
  val dtlbEnable = WireInit(false.B)
  if (Settings.get("HasDTLB")) {
    BoringUtils.addSink(dtlbFinish, "DTLBFINISH")
    BoringUtils.addSink(dtlbPF, "DTLBPF")
    BoringUtils.addSink(dtlbEnable, "DTLBENABLE")
  }

  io.dtlbPF := dtlbPF

  switch (state) {
    is (s_idle) { 
      when (dmem.req.fire() && dtlbEnable)  { state := s_wait_tlb  }
      when (dmem.req.fire() && !dtlbEnable) { state := s_wait_resp } 
      //when (dmem.req.fire()) { state := Mux(isStore, s_partialLoad, s_wait_resp) }
    }
    is (s_wait_tlb) {
      when (dtlbFinish && dtlbPF ) { state := s_idle }
      when (dtlbFinish && !dtlbPF) { state := s_wait_resp/*Mux(isStore, s_partialLoad, s_wait_resp) */} 
    }
    is (s_wait_resp) { when (dmem.resp.fire()) { state := Mux(partialLoad, s_partialLoad, s_idle) } }
    is (s_partialLoad) { state := s_idle }
  }

  Debug(dmem.req.fire(), "[LSU] %x, size %x, wdata_raw %x, isStore %x\n", addr, func(1,0), io.wdata, isStore)
  Debug(dmem.req.fire(), "[LSU] dtlbFinish:%d dtlbEnable:%d dtlbPF:%d state:%d addr:%x dmemReqFire:%d dmemRespFire:%d dmemRdata:%x\n",dtlbFinish, dtlbEnable, dtlbPF, state,  dmem.req.bits.addr, dmem.req.fire(), dmem.resp.fire(), dmem.resp.bits.rdata)
  Debug(dtlbFinish && dtlbEnable, "[LSU] dtlbFinish:%d dtlbEnable:%d dtlbPF:%d state:%d addr:%x dmemReqFire:%d dmemRespFire:%d dmemRdata:%x\n",dtlbFinish, dtlbEnable, dtlbPF, state,  dmem.req.bits.addr, dmem.req.fire(), dmem.resp.fire(), dmem.resp.bits.rdata)

  val size = func(1,0)
  val reqAddr  = if (XLEN == 32) SignExt(addr, VAddrBits) else addr(VAddrBits-1,0)
  val reqWdata = if (XLEN == 32) genWdata32(io.wdata, size) else genWdata(io.wdata, size)
  val reqWmask = if (XLEN == 32) genWmask32(addr, size) else genWmask(addr, size)
  dmem.req.bits.apply(
    addr = reqAddr, 
    size = size, 
    wdata = reqWdata,
    wmask = reqWmask,
    cmd = Mux(isStore, SimpleBusCmd.write, SimpleBusCmd.read))
  dmem.req.valid := valid && (state === s_idle) && !io.loadAddrMisaligned && !io.storeAddrMisaligned
  dmem.resp.ready := true.B

  io.out.valid := Mux( dtlbPF && state =/= s_idle || io.loadAddrMisaligned || io.storeAddrMisaligned, true.B, Mux(partialLoad, state === s_partialLoad, dmem.resp.fire() && (state === s_wait_resp)))
  io.in.ready := (state === s_idle) || dtlbPF

  Debug(io.out.fire(), "[LSU-EXECUNIT] state %x dresp %x dpf %x lm %x sm %x\n", state, dmem.resp.fire(), dtlbPF, io.loadAddrMisaligned, io.storeAddrMisaligned)

  val rdata = dmem.resp.bits.rdata
  val rdataLatch = RegNext(rdata)
  val rdataSel64 = LookupTree(addrLatch(2, 0), List(
    "b000".U -> rdataLatch(63, 0),
    "b001".U -> rdataLatch(63, 8),
    "b010".U -> rdataLatch(63, 16),
    "b011".U -> rdataLatch(63, 24),
    "b100".U -> rdataLatch(63, 32),
    "b101".U -> rdataLatch(63, 40),
    "b110".U -> rdataLatch(63, 48),
    "b111".U -> rdataLatch(63, 56)
  ))
  val rdataSel32 = LookupTree(addrLatch(1, 0), List(
    "b00".U -> rdataLatch(31, 0),
    "b01".U -> rdataLatch(31, 8),
    "b10".U -> rdataLatch(31, 16),
    "b11".U -> rdataLatch(31, 24)
  ))
  val rdataSel = if (XLEN == 32) rdataSel32 else rdataSel64
  val rdataPartialLoad = LookupTree(func, List(
      LSUOpType.lb   -> SignExt(rdataSel(7, 0) , XLEN),
      LSUOpType.lh   -> SignExt(rdataSel(15, 0), XLEN),
      LSUOpType.lw   -> SignExt(rdataSel(31, 0), XLEN),
      LSUOpType.lbu  -> ZeroExt(rdataSel(7, 0) , XLEN),
      LSUOpType.lhu  -> ZeroExt(rdataSel(15, 0), XLEN),
      LSUOpType.lwu  -> ZeroExt(rdataSel(31, 0), XLEN)
  ))
  val addrAligned = LookupTree(func(1,0), List(
    "b00".U   -> true.B,            //b
    "b01".U   -> (addr(0) === 0.U),   //h
    "b10".U   -> (addr(1,0) === 0.U), //w
    "b11".U   -> (addr(2,0) === 0.U)  //d
  ))

  io.out.bits := Mux(partialLoad, rdataPartialLoad, rdata(XLEN-1,0))

  io.isMMIO := DontCare

  val isAMO = WireInit(false.B)
  BoringUtils.addSink(isAMO, "ISAMO2")
  BoringUtils.addSource(addr, "LSUADDR")

  io.loadAddrMisaligned :=  valid && !isStore && !isAMO && !addrAligned
  io.storeAddrMisaligned := valid && (isStore || isAMO) && !addrAligned

  Debug(io.loadAddrMisaligned || io.storeAddrMisaligned, "misaligned addr detected\n")

  BoringUtils.addSource(dmem.isRead() && dmem.req.fire(), "perfCntCondMloadInstr")
  BoringUtils.addSource(BoolStopWatch(dmem.isRead(), dmem.resp.fire()), "perfCntCondMloadStall")
  BoringUtils.addSource(BoolStopWatch(dmem.isWrite(), dmem.resp.fire()), "perfCntCondMstoreStall")
  BoringUtils.addSource(io.isMMIO, "perfCntCondMmmioInstr")
}
