package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

trait HasROBConst{
  // val multiIssue = true
  val robSize = 8
  val robWidth = 2
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
    val commit = Vec(2, Flipped(Decoupled(new CommitIO)))
    val wb = Vec(2, new WriteBackIO)
    val redirect = new RedirectIO
    val flush = Input(Bool())
    val index = Output(UInt(log2Up(robSize).W))
  })

  val decode = Vec(robSize, Vec(robWidth, Reg(new DecodeIO)))
  val valid = Vec(robSize, Vec(robWidth, RegInit(false.B)))
  val commited = Vec(robSize, Vec(robWidth, Reg(Bool())))

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
  }
  forAllROBBanks((i: Int) => io.in(i).ready := ringBufferAllowin)

  // commit to ROB
  // ROB listens to CDB (common data bus), i.e. CommitIO
  // An ROB term will be marked as commited after that inst was commited to CDB
  // This will always success

  // if ROB index == commit index && bank index == commit bank index
  // mark an ROB term as commited

  // Write result to ROB

  // ROB commit & dequeue
  // Here we do the real commit work.
  // We write back at most #bank reg results back to arch-rf.
  // Then we mark those ROB terms as finished, i.e. `!valid`

  // Arch-RF write back

  // If l/s are decoupled, store request is sent to store buffer here.
  // Note: only # of safe store ops is sent to LSU
  // TODO

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
