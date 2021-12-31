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
import difftest._

object physicalRFTools{
  def getPRFAddr(robIndex: UInt, bank: UInt): UInt = {
    Cat(robIndex, bank(0))
  }
}

// Reorder Buffer & Scheduler for Out Of Order Execution Backend
class ROB(implicit val p: NutCoreConfig) extends NutCoreModule with HasInstrType with HasBackendConst with HasRegFileParameter{
  val io = IO(new Bundle {
    val in = Vec(robWidth, Flipped(Decoupled(new DecodeIO)))
    val brMaskIn = Input(Vec(robWidth, UInt(checkpointSize.W)))
    val cdb = Vec(robWidth, Flipped(Valid(new OOCommitIO)))
    val mispredictRec = Flipped(new MisPredictionRecIO)
    val wb = Vec(robWidth, new WriteBackIO)
    val redirect = new RedirectIO
    val flush = Input(Bool())
    val empty = Output(Bool())
    val index = Output(UInt(log2Up(robSize).W))

    // to CSR
    val exception = Output(Bool())
    val beUop = Output(new RenamedDecodeIO)

    // to LSU
    val scommit = Output(Bool())

    // PRF
    val aprf = Output(Vec(robWidth * 2, UInt(prfAddrWidth.W))) // prf addr for current enqueue inst
    val rprf = Output(Vec(robWidth * 2, UInt(XLEN.W))) // prf value data for current enqueue inst
    val rvalid = Output(Vec(robWidth * 2, Bool()))
    val rcommited = Output(Vec(robWidth * 2, Bool()))

    // Misprediction recovery
    val updateCheckpoint = Input(Valid(UInt(log2Up(checkpointSize).W)))
    val recoverCheckpoint = Input(Valid(UInt(log2Up(checkpointSize).W)))
  })

  require(robWidth >= 2 && isPow2(robWidth))
  require(robSize >= 2 && isPow2(robSize))

  require(robWidth == 2) // current version only supports robWidth 2
  require(RetireWidth == robWidth)

  def needMispredictionRecovery(brMask: UInt) = {
    io.mispredictRec.valid && io.mispredictRec.redirect.valid && brMask(io.mispredictRec.checkpoint)
  }

  def updateBrMask(brMask: UInt) = {
    brMask & ~ (UIntToOH(io.mispredictRec.checkpoint) & Fill(checkpointSize, io.mispredictRec.valid))
  }

  // ROB entry
  val decode = Reg(Vec(robSize, Vec(robWidth, new DecodeIO)))
  val brMask = RegInit(VecInit(List.fill(robSize)(VecInit(List.fill(robWidth)(0.U(checkpointSize.W))))))
  val valid = RegInit(VecInit(List.fill(robSize)(VecInit(List.fill(robWidth)(false.B)))))
  val commited = Reg(Vec(robSize, Vec(robWidth, Bool()))) // Commited to CDB (i.e. writebacked)
  val canceled = Reg(Vec(robSize, Vec(robWidth, Bool()))) // for debug
  val redirect = Reg(Vec(robSize, Vec(robWidth, new RedirectIO)))
  val exception = Reg(Vec(robSize, Vec(robWidth, Bool()))) // Backend exception
  val isMMIO = Reg(Vec(robSize, Vec(robWidth, Bool())))
  val intrNO = Reg(Vec(robSize, Vec(robWidth, UInt(XLEN.W))))
  val prf = Mem(robSize * robWidth, UInt(XLEN.W))

  // lsroq 
  // Currently, ROB is also used as lsroq when out of order store is enabled
  val load = RegInit(VecInit(List.fill(robSize)(VecInit(List.fill(robWidth)(false.B)))))
  val store = RegInit(VecInit(List.fill(robSize)(VecInit(List.fill(robWidth)(false.B)))))

  // Almost all int/exceptions can be detected at decode stage, excepting for load/store related exception.
  // In NutCore-Argo's backend, non-l/s int/exc will be sent dircetly to CSR when dispatch,
  // while l/s exc will be sent to CSR by LSU.

  // FIXME: use additional bit to distinguish full/empty
  val ringBufferHead = RegInit(0.U(log2Up(robSize).W))
  val ringBufferTail = RegInit(0.U(log2Up(robSize).W))
  val ringBufferEmpty = ringBufferHead === ringBufferTail && !valid(ringBufferHead)(0) && !valid(ringBufferHead)(1)
  val ringBufferFull = ringBufferTail === ringBufferHead && (valid(ringBufferHead)(0) || valid(ringBufferHead)(1))
  val ringBufferAllowin = !ringBufferFull 


  io.index := ringBufferHead
  io.empty := ringBufferEmpty

  // Register Map  
  val rmtMap = Reg(Vec(NRReg, UInt(prfAddrWidth.W)))
  val rmtValid = RegInit(VecInit(Seq.fill(NRReg)(false.B)))

  sealed class Checkpoint extends NutCoreBundle {
    val map = Vec(NRReg, UInt(prfAddrWidth.W))
    val valid = Vec(NRReg, Bool())
  }

  val checkpoints= Reg(Vec(checkpointSize, new Checkpoint))
  val checkpointUpdate = Wire(new Checkpoint)
  checkpointUpdate.map := rmtMap
  checkpointUpdate.valid := rmtValid

  val rmtValidRecovery = Wire(Vec(NRReg, Bool()))
  rmtValidRecovery := checkpoints(io.recoverCheckpoint.bits).valid

  (0 until robWidth).map(i => {
    io.aprf(2*i)        := rmtMap(io.in(i).bits.ctrl.rfSrc1)
    io.aprf(2*i+1)      := rmtMap(io.in(i).bits.ctrl.rfSrc2)
    io.rprf(2*i)        := prf(io.aprf(2*i))
    io.rprf(2*i+1)      := prf(io.aprf(2*i+1))
    io.rvalid(2*i)      := rmtValid(io.in(i).bits.ctrl.rfSrc1)
    io.rvalid(2*i+1)    := rmtValid(io.in(i).bits.ctrl.rfSrc2)
    io.rcommited(2*i)   := commited(io.aprf(2*i)>>1)(io.aprf(2*i)(0)) && valid(io.aprf(2*i)>>1)(io.aprf(2*i)(0))
    io.rcommited(2*i+1) := commited(io.aprf(2*i+1)>>1)(io.aprf(2*i+1)(0)) && valid(io.aprf(2*i+1)>>1)(io.aprf(2*i+1)(0))
  })

  //---------------------------------------------------------
  // Commit logic
  //---------------------------------------------------------

  // commit to ROB
  // ROB listens to CDB (common data bus), i.e. CommitIO
  // An ROB term will be marked as commited after that inst was commited to CDB
  // This will always success

  // if ROB index == commit index && bank index == commit bank index
  // update brmask, invalidate insts in mis-predict path
  // check if commit is valid (debug only)
  for(i <- (0 to robSize - 1)){
    for(j <- (0 to robWidth - 1)){
      val robIdx = Cat(i.asUInt(log2Up(robSize).W), j.asUInt(log2Up(robWidth).W))
      for(k <- (0 to robWidth - 1)){
        when(valid(i)(j) && io.cdb(k).bits.prfidx === robIdx && io.cdb(k).valid){
        // when(true.B){
          Debug(commited(i)(j), "[ERROR] double commit robidx %d pc %x inst %x pcin %x instin %x\n", robIdx, decode(i)(j).cf.pc, decode(i)(j).cf.instr, io.cdb(k).bits.decode.cf.pc, io.cdb(k).bits.decode.cf.instr)
          Debug(io.cdb(k).bits.decode.cf.pc =/= decode(i)(j).cf.pc, "[ERROR] commit pc not match robidx %d pc %x inst %x pcin %x instin %x\n", robIdx, decode(i)(j).cf.pc, decode(i)(j).cf.instr, io.cdb(k).bits.decode.cf.pc, io.cdb(k).bits.decode.cf.instr)
          assert(io.cdb(k).bits.decode.cf.pc === decode(i)(j).cf.pc)
          assert(!commited(i)(j), "double commit")
        }
      }
      when(valid(i)(j) && needMispredictionRecovery(brMask(i)(j))){
        valid(i)(j) := false.B
        canceled(i)(j) := true.B
        Debug("[ROB] mis-prediction recovery invalidated robidx %d pc %x (%b)\n", robIdx, decode(i)(j).cf.pc, brMask(i)(j))
      }
      brMask(i)(j) := updateBrMask(brMask(i)(j))
    }
  }

  // writeback to physical RF, update ROB control bits
  for(k <- 0 until robWidth){
    val prfidx = io.cdb(k).bits.prfidx
    val index = prfidx(log2Up(robSize), log2Up(robWidth))
    val bank = prfidx(log2Up(robWidth)-1, 0)
    when(io.cdb(k).valid){
      // Mark an ROB term as commited
      when(!io.cdb(k).bits.exception){
        commited(index)(bank) := true.B
      }
      // Write result to ROB-PRF
      prf(prfidx) := io.cdb(k).bits.commits
      // Write other info which could be generated by function units
      isMMIO(index)(bank) := io.cdb(k).bits.isMMIO
      intrNO(index)(bank) := io.cdb(k).bits.intrNO
      // Write redirect info
      redirect(index)(bank) := io.cdb(k).bits.decode.cf.redirect
      redirect(index)(bank).valid := io.cdb(k).bits.decode.cf.redirect.valid
      exception(index)(bank) := io.cdb(k).bits.exception
      // Update wen
      // In several cases, FU will invalidate rfWen
      store(index)(bank) := io.cdb(k).bits.store
      decode(index)(bank).ctrl.rfWen := io.cdb(k).bits.decode.ctrl.rfWen
    }
  }

  //---------------------------------------------------------
  // Retire logic
  //---------------------------------------------------------

  // ROB Retire
  // We write back at most #bank reg results back to arch-rf.
  // Then we mark those ROB terms as finished, i.e. `!valid`
  // No more than robWidth insts can retire from ROB in a single cycle.
  val tailBankNotUsed = List.tabulate(robWidth)(i => !valid(ringBufferTail)(i) || valid(ringBufferTail)(i) && commited(ringBufferTail)(i))
  val tailTermEmpty = List.tabulate(robWidth)(i => !valid(ringBufferTail)(i)).reduce(_ && _)
  // TODO: refactor retire logic
  val skipException = valid(ringBufferTail)(0) && redirect(ringBufferTail)(0).valid && commited(ringBufferTail)(0) && exception(ringBufferTail)(1)
  val retireATerm = tailBankNotUsed.reduce(_ && _) && !tailTermEmpty || skipException
  val recycleATerm = tailTermEmpty && (ringBufferTail =/= ringBufferHead)
  when(retireATerm || recycleATerm){
    (0 until robWidth).map(i => {
      valid(ringBufferTail)(i) := false.B
      // free prf (update RMT)
      when(
        decode(ringBufferTail)(i).ctrl.rfWen && 
        rmtMap(decode(ringBufferTail)(i).ctrl.rfDest) === Cat(ringBufferTail, i.U(1.W)) && // no other in flight inst will write this reg 
        valid(ringBufferTail)(i)
      ){
        rmtValid(decode(ringBufferTail)(i).ctrl.rfDest) := false.B
      }
      // update checkpoint
      when(
        decode(ringBufferTail)(i).ctrl.rfWen && 
        valid(ringBufferTail)(i)
      ){
        (0 until checkpointSize).map(k => {
          when(checkpoints(k).map(decode(ringBufferTail)(i).ctrl.rfDest) === Cat(ringBufferTail, i.U(1.W))){
            checkpoints(k).valid(decode(ringBufferTail)(i).ctrl.rfDest) := false.B
          }
        })
        when(checkpoints(io.recoverCheckpoint.bits).map(decode(ringBufferTail)(i).ctrl.rfDest) === Cat(ringBufferTail, i.U(1.W))){
          rmtValidRecovery(decode(ringBufferTail)(i).ctrl.rfDest) := false.B
        }
        when(checkpointUpdate.map(decode(ringBufferTail)(i).ctrl.rfDest) === Cat(ringBufferTail, i.U(1.W))){
          checkpointUpdate.valid(decode(ringBufferTail)(i).ctrl.rfDest) := false.B
        }
      }
    })
    ringBufferTail := ringBufferTail + 1.U
  }

  // mispredict checkpoint recovery
  when(io.updateCheckpoint.valid){
    checkpoints(io.updateCheckpoint.bits) := checkpointUpdate
    Debug("[CP] checkpoint %d set\n", io.updateCheckpoint.bits)
    // Debug("[CP] %d %d\n", checkpointUpdate.valid(14), checkpointUpdate.map(14))
  }
  when(io.recoverCheckpoint.valid){
    rmtMap := checkpoints(io.recoverCheckpoint.bits).map
    rmtValid := rmtValidRecovery
    Debug("[CP] recover rmt to checkpoint %d\n", io.recoverCheckpoint.bits)
  }

  // retire: trigger redirect
  // exception/interrupt/branch mispredict redirect is raised by ROB
  val redirectBank = PriorityMux(
    (0 until RetireWidth).map(i => redirect(ringBufferTail)(i).valid && valid(ringBufferTail)(i)),
    (0 until RetireWidth).map(_.U)
  )
  io.redirect := redirect(ringBufferTail)(redirectBank)
  io.redirect.valid := retireATerm && List.tabulate(robWidth)(i => 
    redirect(ringBufferTail)(i).valid && valid(ringBufferTail)(i)
  ).reduce(_ || _)
  // io.redirect.rtype := 0.U

  // retire: trigger exception
  // Only l/s exception will trigger ROB flush
  // Other exception/interrupt will be stalled by Dispatch Unit until ROB is empty

  // Currently, AGU(LSU) will send exception to CDB and commit this inst.
  // However, commit with backend exception will not set commited to `true`.
  // When ROB detected a backend exception at the tail of ROB, a `trigger exception`
  // signal will be sent to CSR. CSR gets info about this inst from a special reg.
  // The inst which caused exception will be commited by CSR.

  // By doing this, we can treat CSR module as a single cycle FU.

  // TODO: delay 1 cycle for better timing performance
  io.exception := 
    valid(ringBufferTail)(0) && exception(ringBufferTail)(0) ||
    valid(ringBufferTail)(1) && exception(ringBufferTail)(1) && (!valid(ringBufferTail)(0) || commited(ringBufferTail)(0) && !redirect(ringBufferTail)(0).valid)

  // setup beUop for CSR
  // `beUop` stands for `backend exception uop`
  val exceptionSelect = PriorityMux(
    (0 until RetireWidth).map(i => exception(ringBufferTail)(i)),
    (0 until RetireWidth).map(_.U)
  )
  io.beUop := DontCare
  // io.beUop.decode := decode(ringBufferTail)(exceptionSelect)
  io.beUop.decode.cf.pc := decode(ringBufferTail)(exceptionSelect).cf.pc
  for(i <- 0 to storePageFault){io.beUop.decode.cf.exceptionVec(i) := false.B}
  io.beUop.decode.cf.exceptionVec(loadPageFault) := intrNO(ringBufferTail)(exceptionSelect)(loadPageFault)
  io.beUop.decode.cf.exceptionVec(storePageFault) := intrNO(ringBufferTail)(exceptionSelect)(storePageFault)
  io.beUop.decode.cf.exceptionVec(loadAddrMisaligned) := intrNO(ringBufferTail)(exceptionSelect)(loadAddrMisaligned)
  io.beUop.decode.cf.exceptionVec(storeAddrMisaligned) := intrNO(ringBufferTail)(exceptionSelect)(storeAddrMisaligned)
  io.beUop.decode.data.src1 := prf(Cat(ringBufferTail, exceptionSelect)) //FIXIT
  // io.beUop.decode.data.src2 := DontCare
  io.beUop.prfDest := Cat(ringBufferTail, exceptionSelect)

  assert(!(exception(ringBufferTail)(0) && exception(ringBufferTail)(1) && valid(ringBufferTail)(0) && valid(ringBufferTail)(1)))
  assert(!(exception(ringBufferTail)(0) && decode(ringBufferTail)(0).ctrl.fuType =/= FuType.lsu && valid(ringBufferTail)(0)))
  assert(!(exception(ringBufferTail)(1) && decode(ringBufferTail)(1).ctrl.fuType =/= FuType.lsu && valid(ringBufferTail)(1)))

  // Raise decoupled store request
  // If l/s are decoupled, store request is sent to store buffer here.
  // Note: only # of safe store ops is sent to LSU

  // FIXME: or we can just modify `store` bit in ROB
  val cancelScommit = RegInit(false.B)
  when(io.exception){ cancelScommit := true.B }
  when(io.flush){ cancelScommit := false.B }

  io.scommit := List.tabulate(robWidth)(i => 
    valid(ringBufferTail)(i) && 
    store(ringBufferTail)(i) && 
    List.tabulate(i)(j => (!redirect(ringBufferTail)(j).valid)).foldRight(true.B)((sum, k) => sum && k) && 
    List.tabulate(i+1)(j => (!exception(ringBufferTail)(j))).foldRight(true.B)((sum, k) => sum && k) &&
    !cancelScommit
  ).reduce(_ || _) && retireATerm
  // In current version, only one l/s inst can be sent to agu in a cycle
  // therefore, in all banks, there is no more than 1 store insts

  Debug(io.scommit, "[SCommit] %x %x %x %x\n", 
        redirect(ringBufferTail)(0).valid,
        redirect(ringBufferTail)(1).valid,
        exception(ringBufferTail)(0),
        exception(ringBufferTail)(1))

  // For debug:
  // check if store mem req is from a commited store
  val lsupc = Wire(UInt(VAddrBits.W))
  val storeTBCV = Wire(Bool())
  val Q1 = Module(new Queue(UInt(VAddrBits.W), 32, pipe = true, flow = true))
  val Q2 = Module(new Queue(UInt(VAddrBits.W), 32, pipe = true, flow = true))
  Q1.io.enq.valid := io.scommit
  Q1.io.deq.ready := storeTBCV
  Q1.io.enq.bits := decode(ringBufferTail)(0).cf.pc
  Q2.io.enq.valid := io.scommit
  Q2.io.deq.ready := storeTBCV
  Q2.io.enq.bits := decode(ringBufferTail)(1).cf.pc
  lsupc := DontCare
  storeTBCV := DontCare
  BoringUtils.addSink(lsupc, "GSPC")
  BoringUtils.addSink(storeTBCV, "GSPCV")

  when(
    storeTBCV &&
    (
      Q1.io.deq.bits =/= lsupc &&
      Q2.io.deq.bits =/= lsupc ||
      !Q1.io.deq.valid
    ) 
  ){
    Debug("[ERROR] robpc1 %x robpc2 %x v %x lsupc %x\n", 
      Q1.io.deq.bits,
      Q2.io.deq.bits,
      Q1.io.deq.valid,
      lsupc
    )
  }

  // Arch-RF write back
  for(i <- (0 to robWidth - 1)){
    // val haveRedirect = List.tabulate(i + 1)(j => redirect(ringBufferTail)(j).valid && valid(ringBufferTail)(j)).foldRight(false.B)((sum, i) => sum|i)
    io.wb(i).rfWen := retireATerm && decode(ringBufferTail)(i).ctrl.rfWen && valid(ringBufferTail)(i)
    io.wb(i).rfDest := decode(ringBufferTail)(i).ctrl.rfDest
    io.wb(i).rfData := prf(Cat(ringBufferTail, i.U))
  }

  // fix wen
  val instRedirect = (0 until RetireWidth).map(i => redirect(ringBufferTail)(i).valid && valid(ringBufferTail)(i))
  (1 until RetireWidth).map(i => {
    when(instRedirect.take(i).reduce(_ || _)){
      io.wb(i).rfWen := false.B
    }
  })

  //---------------------------------------------------------
  // Dispatch logic
  //---------------------------------------------------------

  // ROB enqueue
  val validEnqueueRequest = List.tabulate(robWidth)(i => io.in(i).valid).reduce(_ | _) //io.in(0).valid || io.in(1).valid
  when(validEnqueueRequest && ringBufferAllowin){
    ringBufferHead := ringBufferHead + 1.U
    (0 until robWidth).map(i => {
      decode(ringBufferHead)(i) := io.in(i).bits
      brMask(ringBufferHead)(i) := io.brMaskIn(i)
      valid(ringBufferHead)(i) := io.in(i).valid
      load(ringBufferHead)(i) := io.in(i).bits.ctrl.fuType === FuType.lsu && LSUOpType.needMemRead(io.in(i).bits.ctrl.fuOpType)
      store(ringBufferHead)(i) := io.in(i).bits.ctrl.fuType === FuType.lsu && LSUOpType.needMemWrite(io.in(i).bits.ctrl.fuOpType)
      commited(ringBufferHead)(i) := false.B
      canceled(ringBufferHead)(i) := false.B
      redirect(ringBufferHead)(i).valid := false.B
      exception(ringBufferHead)(i) := false.B
      when(io.in(i).valid && io.in(i).bits.ctrl.rfWen && io.in(i).bits.ctrl.rfDest =/= 0.U){
        rmtMap(io.in(i).bits.ctrl.rfDest) := Cat(ringBufferHead, i.U)
        rmtValid(io.in(i).bits.ctrl.rfDest) := true.B
        // rmtCommited(io.in(i).bits.ctrl.rfDest) := false.B
      }
    })
    when(io.in(0).valid && io.in(0).bits.ctrl.rfWen && io.in(0).bits.ctrl.rfDest =/= 0.U){
      checkpointUpdate.map(io.in(0).bits.ctrl.rfDest) := Cat(ringBufferHead, 0.U)
      checkpointUpdate.valid(io.in(0).bits.ctrl.rfDest) := true.B
    }
    when(io.in(1).valid && io.in(1).bits.ctrl.rfWen && io.in(1).bits.ctrl.rfDest =/= 0.U && !(io.in(0).valid && io.in(0).bits.ctrl.fuType === FuType.bru)){
      checkpointUpdate.map(io.in(1).bits.ctrl.rfDest) := Cat(ringBufferHead, 1.U)
      checkpointUpdate.valid(io.in(1).bits.ctrl.rfDest) := true.B
    }
  }
  (0 until robWidth).map(i => io.in(i).ready := ringBufferAllowin)
  assert(!(validEnqueueRequest && ringBufferAllowin && io.recoverCheckpoint.valid))

  // Send robInstValid signal to LSU for "backward"
  val robLoadInstVec = WireInit(VecInit((0 until robSize).map(i => {
    (0 until robWidth).map(j => valid(i)(j) && load(i)(j)).reduce(_ || _)
  })).asUInt) 
  val robStoreInstVec = WireInit(VecInit((0 until robSize).map(i => {
    (0 until robWidth).map(j => valid(i)(j) && store(i)(j)).reduce(_ || _)
  })).asUInt) 
  // TODO: use a single bit in rob misc field to save "isload"
  BoringUtils.addSource(robLoadInstVec, "ROBLoadInstVec")
  BoringUtils.addSource(robStoreInstVec, "ROBStoreInstVec")

  // reset headptr when mis-prediction recovery is triggered
  when(io.mispredictRec.valid && io.mispredictRec.redirect.valid){
    ringBufferHead := io.mispredictRec.prfidx(prfAddrWidth-1, 1) + 1.U
  }

  // flush control
  when(io.flush){
    ringBufferHead := 0.U
    ringBufferTail := 0.U
    List.tabulate(robSize)(i => valid(i)(0) := 0.U) // set valid to 0
    List.tabulate(robSize)(i => valid(i)(1) := 0.U) // set valid to 0
    List.tabulate(NRReg)(i => rmtValid(i) := false.B) // flush rmt
  }

  // Generate Debug Info
    Debug(io.in(0).fire, "[DISPATCH1] pc = 0x%x inst %x wen %x wdst %x\n", io.in(0).bits.cf.pc, io.in(0).bits.cf.instr, io.in(0).bits.ctrl.rfWen, io.in(0).bits.ctrl.rfDest)
    Debug(io.in(1).fire, "[DISPATCH2] pc = 0x%x inst %x wen %x wdst %x\n", io.in(1).bits.cf.pc, io.in(1).bits.cf.instr, io.in(1).bits.ctrl.rfWen, io.in(1).bits.ctrl.rfDest)
    Debug(io.cdb(0).valid, "[COMMIT1] pc = 0x%x inst %x wen %x wdst %x wdata = 0x%x\n", io.cdb(0).bits.decode.cf.pc, io.cdb(0).bits.decode.cf.instr, io.cdb(0).bits.decode.ctrl.rfWen, io.cdb(0).bits.decode.ctrl.rfDest, io.cdb(0).bits.commits)
    Debug(io.cdb(1).valid, "[COMMIT2] pc = 0x%x inst %x wen %x wdst %x wdata = 0x%x\n", io.cdb(1).bits.decode.cf.pc, io.cdb(1).bits.decode.cf.instr, io.cdb(1).bits.decode.ctrl.rfWen, io.cdb(1).bits.decode.ctrl.rfDest, io.cdb(1).bits.commits)
    Debug(retireATerm && valid(ringBufferTail)(0), "[RETIRE1] pc = 0x%x inst %x wen %x wdst %x wdata %x mmio %x intrNO %x\n", decode(ringBufferTail)(0).cf.pc, decode(ringBufferTail)(0).cf.instr, io.wb(0).rfWen, io.wb(0).rfDest, io.wb(0).rfData, isMMIO(ringBufferTail)(0), intrNO(ringBufferTail)(0))
    Debug(retireATerm && valid(ringBufferTail)(1), "[RETIRE2] pc = 0x%x inst %x wen %x wdst %x wdata %x mmio %x intrNO %x\n", decode(ringBufferTail)(1).cf.pc, decode(ringBufferTail)(1).cf.instr, io.wb(1).rfWen, io.wb(1).rfDest, io.wb(1).rfData, isMMIO(ringBufferTail)(1), intrNO(ringBufferTail)(1))

  Debug(){
    Debug("[ROB] ")
    for(i <- 0 to (robSize - 1)){
      when(valid(i)(0) && commited(i)(0)){Debug(false, "c")}.elsewhen(valid(i)(0)){Debug(false, "v")}.otherwise{Debug(false, "-")}
    }
    Debug("\n[ROB] ")
    for(i <- 0 to (robSize - 1)){
      when(valid(i)(1) && commited(i)(1)){Debug("c")}.elsewhen(valid(i)(1)){Debug("v")}.otherwise{Debug("-")}
    }
    Debug("\n[ROB] ")
    for(i <- 0 to (robSize - 1)){
      when(ringBufferHead === i.U){Debug("h")}
      .elsewhen(ringBufferTail === i.U){Debug("t")}
      .otherwise{Debug(" ")}
    }
    Debug("\n")
    Debug("[ROB] pc           v w c r e   pc           v w c r e\n")
    for(i <- 0 to (robSize - 1)){
      Debug("[ROB] 0x%x %d %d %d %d %d   0x%x %d %d %d %d %d  " + i, 
        decode(i)(0).cf.pc, valid(i)(0), commited(i)(0), canceled(i)(0), redirect(i)(0).valid && valid(i)(0), exception(i)(0),
        decode(i)(1).cf.pc, valid(i)(1), commited(i)(1), canceled(i)(1), redirect(i)(1).valid && valid(i)(1), exception(i)(1)
      )
      when(valid(i)(0) || valid(i)(1)){Debug("  valid")}
      when(ringBufferHead === i.U){Debug("  head")}
      when(ringBufferTail === i.U){Debug("  tail")}
      Debug("\n")
    }
    
    // for(i <- 0 to (robSize - 1)){
    //   Debug("[ROB] %b %b " + i + "\n", brMask(i)(0), brMask(i)(1))
    // }

    // for(i <- 0 until NRReg){
    //   Debug("[ROB] prMap %b %d " + i + "\n", rmtValid(i), rmtMap(i))
    // }

    // Debug("[RMT INFO]")
    // for(i <- 0 to (NRReg - 1)){
    //  // if(i % 6 == 0)Debug("\n")
    //   when(rmtValid(i)){
    //     Debug("%d -> %d %b  ", i.U, rmtMap(i), commited(i.U>>1)(i.U(0)))
    //   }
    // }
    // Debug("\n")
  }

  val retireMultiTerms = retireATerm && valid(ringBufferTail)(0) && valid(ringBufferTail)(1) && !instRedirect(0)
  BoringUtils.addSource(retireATerm, "perfCntCondMinstret")
  BoringUtils.addSource(retireMultiTerms, "perfCntCondMultiCommit")
  
  if (!p.FPGAPlatform) {
    for (i <- 0 until RetireWidth) {
      val difftest_commit = Module(new DifftestInstrCommit)
      difftest_commit.io.clock    := clock
      difftest_commit.io.coreid   := 0.U
      difftest_commit.io.index    := i.U

      difftest_commit.io.valid    := {if (i == 0) RegNext(retireATerm) else RegNext(retireMultiTerms)}
      difftest_commit.io.pc       := RegNext(SignExt(decode(ringBufferTail)(i).cf.pc, AddrBits))
      difftest_commit.io.instr    := RegNext(decode(ringBufferTail)(i).cf.instr)
      difftest_commit.io.skip     := RegNext(isMMIO(ringBufferTail)(i) && valid(ringBufferTail)(i))
      difftest_commit.io.isRVC    := RegNext(decode(ringBufferTail)(i).cf.isRVC)
      difftest_commit.io.wen      := RegNext(io.wb(i).rfWen && io.wb(i).rfDest =/= 0.U) // && valid(ringBufferTail)(i) && commited(ringBufferTail)(i)
      // difftest.io.wdata    := RegNext(io.wb(i).rfData)
      difftest_commit.io.wdest    := RegNext(io.wb(i).rfDest)
      difftest_commit.io.wpdest   := RegNext(io.wb(i).rfDest)

      val difftest_wb = Module(new DifftestIntWriteback)
      difftest_wb.io.clock := clock
      difftest_wb.io.coreid := 0.U
      difftest_wb.io.valid := RegNext(io.wb(i).rfWen && io.wb(i).rfDest =/= 0.U)
      difftest_wb.io.dest := RegNext(io.wb(i).rfDest)
      difftest_wb.io.data := RegNext(io.wb(i).rfData)
    }
  } else {
    BoringUtils.addSource(retireATerm, "ilaWBUvalid")
    BoringUtils.addSource(SignExt(decode(ringBufferTail)(0).cf.pc, AddrBits), "ilaWBUpc")
    BoringUtils.addSource(io.wb(0).rfWen, "ilaWBUrfWen")
    BoringUtils.addSource(io.wb(0).rfDest, "ilaWBUrfDest")
    BoringUtils.addSource(io.wb(0).rfData, "ilaWBUrfData")
  }

  Debug(io.empty, "empty\n")
  Debug(io.redirect.valid && io.redirect.rtype === 1.U, "mbr finished\n")


  // sim pref counter
  val retireBruInst = (retireATerm && (decode(ringBufferTail)(0).ctrl.fuType === FuType.bru || decode(ringBufferTail)(1).ctrl.fuType === FuType.bru))
  val retireBruInstRedirect = retireBruInst && (redirect(ringBufferTail)(0).valid || redirect(ringBufferTail)(1).valid)
  BoringUtils.addSource(retireBruInst, "perfCntCondMbruCmt")
  BoringUtils.addSource(retireBruInstRedirect, "perfCntCondMbruCmtWrong")

}
