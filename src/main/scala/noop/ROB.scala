package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

trait HasROBConst{
  // val multiIssue = true
  val robSize = 16
  val robWidth = 2
  val NRrmsq = 4
  val prfAddrWidth = log2Up(robSize) + 1
}

object physicalRFTools{
  def getPRFAddr(robIndex: UInt, bank: UInt): UInt = {
    Cat(robIndex, bank(0))
  }
}

class ROB extends NOOPModule with HasInstrType with HasROBConst{
  val io = IO(new Bundle {
    val in = Vec(robWidth, Flipped(Decoupled(new DecodeIO)))
    val out = Vec(robWidth, Decoupled(new DecodeIO))
    val commit = Vec(2, Flipped(Decoupled(new OOCommitIO)))
    val wb = Vec(2, new WriteBackIO)
    val redirect = new RedirectIO
    val flush = Input(Bool())
    val index = Output(UInt(log2Up(robSize).W))

    // to CSR
    val lsexc = Decoupled(new DecodeIO) // for l/s exception

    // to LSU
    val scommit = Output(Bool())

    // PRF
  })

  val decode = Vec(robSize, Vec(robWidth, Reg(new DecodeIO)))
  val valid = Vec(robSize, Vec(robWidth, RegInit(false.B)))
  val commited = Vec(robSize, Vec(robWidth, Reg(Bool())))
  val redirect = Vec(robSize, Vec(robWidth, Reg(Bool())))
  val redirectTarget = Vec(robSize, Vec(robWidth, Reg(UInt(VAddrBits.W))))
  // val prf = Vec(robSize, Vec(robWidth, Reg(UInt(XLEN.W))))
  val lsexc = Vec(robSize, Vec(robWidth, Reg(Bool()))) // load/store exception bit
  // Almost all int/exceptions can be detected at decode stage, excepting for load/store related exception.
  // In NOOP-Argo's backend, non-l/s int/exc will be sent dircetly to CSR,
  // while l/s exc has a special lsexc bit in ROB.
  // Detailed exception info is stored in independent lsexcReg to minimize space cost.
  // When ROB trys to retire an inst with lsexc bit, it will flush itself, read detailed info from lsexcReg,
  // then send a pseudoinst to CSR to trigger exception.
  // See `ROB-retire` for further information.

  val ringBufferHead = RegInit(0.U(log2Up(robSize).W))
  val ringBufferTail = RegInit(0.U(log2Up(robSize).W))
  val ringBufferEmpty = ringBufferHead === ringBufferTail && !valid(ringBufferHead)(0) && !valid(ringBufferHead)(1)
  val ringBufferFull = ringBufferTail === ringBufferHead && (valid(ringBufferHead)(0) || valid(ringBufferHead)(1))
  val ringBufferAllowin = !ringBufferFull 

  io.index := ringBufferHead

  def forAllROBBanks(func: Int => _) = {
    List.tabulate(robWidth)(func)
  }

  // ROB enqueue
  val validEnqueueRequest = List.tabulate(robWidth)(i => io.in(i).valid).foldRight(false.B)((sum,i)=>sum|i) //io.in(0).valid || io.in(1).valid
  when(validEnqueueRequest && ringBufferAllowin){
    ringBufferHead := ringBufferHead + 1.U
    forAllROBBanks((i: Int) => decode(ringBufferHead)(i) := io.in(i).bits)
    forAllROBBanks((i: Int) => valid(ringBufferHead)(i) := io.in(i).valid)
    forAllROBBanks((i: Int) => commited(ringBufferHead)(i) := false.B)
    forAllROBBanks((i: Int) => redirect(ringBufferHead)(i) := false.B)
  }
  forAllROBBanks((i: Int) => io.in(i).ready := ringBufferAllowin)

  // commit to ROB
  // ROB listens to CDB (common data bus), i.e. CommitIO
  // An ROB term will be marked as commited after that inst was commited to CDB
  // This will always success

  // if ROB index == commit index && bank index == commit bank index
  for(i <- (0 to robSize)){
    for(j <- (0 to robWidth)){
      val robIdx = Cat(i.U, j.U)
      for(k <- (0 to robWidth)){
        when(valid(i)(j) && io.commit(k).bits.prfidx === robIdx){
          assert(!commited(i)(j))
          // Mark an ROB term as commited
          commited(i)(j) := true.B
          // Write result to ROB-PRF
          // prf(i)(j) := io.commit(k).bits.commits
          // TODO: UPDATE PRF, PRF r/w
        }
      }
    }
  }

  // write redirect info
  // TODO

  // ROB Retire
  // We write back at most #bank reg results back to arch-rf.
  // Then we mark those ROB terms as finished, i.e. `!valid`
  // No more than robWidth insts can retire from ROB in a single cycle.
  val tailBankNotUsed = List.tabulate(robWidth)(i => !valid(ringBufferTail)(i) || valid(ringBufferTail)(i) && commited(ringBufferTail)(i))
  val tailTermEmpty = !valid(ringBufferTail).asUInt.orR
  val retireATerm = tailBankNotUsed.foldRight(true.B)((sum, i) => sum & i) && !tailTermEmpty
  when(retireATerm){
    forAllROBBanks((i: Int) => 
      valid(ringBufferTail)(i) := false.B
      // when(decode(ringBufferTail)(i).ctrl.rfWen){
      //   // TODO: free prf in EXU (update RMT)
      // }
    )
  }

  // speculative execution

  // register map queue
  val rmq = Module(new Queue(UInt(VAddrBits.W), NRrmsq)) //TODO
  
  // when speculated branch enters ROB, add current reg map into rmq
  // when a speculated branch inst retires, delete it from rmq
  // TODO
  rmq.io.enq.valid := List.tabulate(robWidth)(i => io.in(i).valid && io.in(i).bits.ctrl.isSpecExec).foldRight(false.B)((sum, i) => sum || i)
  rmq.io.enq.bits := DontCare //TODO
  rmq.io.deq.ready := List.tabulate(robWidth)(i => 
    valid(ringBufferTail)(i) && decode(ringBufferTail)(i).ctrl.isSpecExec
  ).foldRight(false.B)((sum, i) => sum || i)

  // rmap recover := rmq.io.deq.bits

  // retire: trigger redirect
  // TODO

  // l/s exception detection
  // TODO
  // CSR gets decode from io.lsexc.bits
  io.lsexc.valid := List.tabulate(robWidth)(i => 
    valid(ringBufferTail)(i) && decode(ringBufferTail)(i).ctrl.isSpecExec
  ).foldRight(false.B)((sum, i) => sum || i)
  io.lsexc.bits := Mux(decode(ringBufferTail)(0).ctrl.isSpecExec,
    decode(ringBufferTail)(0),
    decode(ringBufferTail)(1)
  )// TODO: currently only support robWidth = 2

  // Arch-RF write back
  // If l/s are decoupled, store request is sent to store buffer here.
  // Note: only # of safe store ops is sent to LSU
  io.scommit := List.tabulate(robWidth)(i => 
    valid(ringBufferTail)(i) && 
    decode(ringBufferTail)(i).ctrl.fuType === FuType.lsu && 
    LSUOpType.isStore(decode(ringBufferTail)(i).ctrl.fuOpType)
  ).foldRight(false.B)((sum, i) => sum || i)
  // In current version, only one l/s inst can be sent to agu in a cycle
  // therefore, in all banks, there is no more than 1 store insts

  // Generate Debug Trace
  // TODO

  // flush control
  when(io.flush){
    ringBufferHead := 0.U
    ringBufferTail := 0.U
    List.tabulate(robSize)(i => valid(i) := 0.U) // set valid to 0
  }

  // redirect controlled by ROB 
  // exception/interrupt/branch mispredict redirect is raised by ROB
  // TODO
  io.redirect := io.commit(0).bits.decode.cf.redirect
  io.redirect.valid := io.commit(0).bits.decode.cf.redirect.valid && io.commit(0).valid

}
