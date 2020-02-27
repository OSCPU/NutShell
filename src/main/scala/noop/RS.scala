package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

trait HasRSConst{
  val rsSize = 4
  val rsCommitWidth = 2
}

// Reservation Station
class RS extends NOOPModule with HasRSConst with HasROBConst {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new RenamedDecodeIO))
    val out = Decoupled(new RenamedDecodeIO)
    val cdb = Vec(rsCommitWidth, Flipped(Valid(new OOCommitIO)))
    val flush = Input(Bool())
    val empty = Output(Bool())
  })

  val decode  = Mem(rsSize, new RenamedDecodeIO) // TODO: decouple DataSrcIO from DecodeIO
  // val decode  = Reg(Vec(rsSize, new RenamedDecodeIO)) // TODO: decouple DataSrcIO from DecodeIO
  val valid   = RegInit(VecInit(Seq.fill(rsSize)(false.B)))
  val src1Rdy = RegInit(VecInit(Seq.fill(rsSize)(false.B)))
  val src2Rdy = RegInit(VecInit(Seq.fill(rsSize)(false.B)))
  // val prfDest = Reg(Vec(rsSize, UInt(prfAddrWidth.W)))
  val prfSrc1 = Reg(Vec(rsSize, UInt(prfAddrWidth.W)))
  val prfSrc2 = Reg(Vec(rsSize, UInt(prfAddrWidth.W)))
  val src1    = Reg(Vec(rsSize, UInt(XLEN.W)))
  val src2    = Reg(Vec(rsSize, UInt(XLEN.W)))
//   val imm = Vec(rsSize, Reg(UInt(XLEN.W)))

  val instRdy = List.tabulate(rsSize)(i => src1Rdy(i) && src2Rdy(i) && valid(i))

  val rsEmpty = !valid.asUInt.orR
  val rsFull = valid.asUInt.andR
  val rsAllowin = !rsFull
  val rsReadygo = instRdy.foldRight(false.B)((sum, i) => sum|i)

  // Listen to Common Data Bus
  // Here we listen to commit signal chosen by ROB?
  // If prf === src, mark it as `ready`

  List.tabulate(rsSize)(i => 
    when(valid(i)){
      List.tabulate(rsCommitWidth)(j =>
        when(!src1Rdy(i) && prfSrc1(i) === io.cdb(j).bits.prfidx && io.cdb(j).valid){
            src1Rdy(i) := true.B
            src1(i) := io.cdb(j).bits.commits
        }
      )
      List.tabulate(rsCommitWidth)(j =>
        when(!src2Rdy(i) && prfSrc2(i) === io.cdb(j).bits.prfidx && io.cdb(j).valid){
            src2Rdy(i) := true.B
            src2(i) := io.cdb(j).bits.commits
        }
      )
    }
  )

  // RS enqueue
  io.in.ready := rsAllowin
  io.empty := rsEmpty
  val emptySlot = ~valid.asUInt
  val enqueueSelect = PriorityEncoder(emptySlot) // TODO: replace PriorityEncoder with other logic

  when(io.in.fire()){
    decode(enqueueSelect) := io.in.bits
    valid(enqueueSelect) := true.B
    prfSrc1(enqueueSelect) := io.in.bits.prfSrc1
    prfSrc2(enqueueSelect) := io.in.bits.prfSrc2
    src1Rdy(enqueueSelect) := io.in.bits.src1Rdy
    src2Rdy(enqueueSelect) := io.in.bits.src2Rdy
    src1(enqueueSelect) := io.in.bits.decode.data.src1
    src2(enqueueSelect) := io.in.bits.decode.data.src2
  }

  // RS dequeue
  io.out.valid := rsReadygo
  val dequeueSelect = PriorityEncoder(instRdy) // TODO: replace PriorityEncoder with other logic
  when(io.out.fire()){
    valid(dequeueSelect) := false.B
  }

  io.out.bits := decode(dequeueSelect)
  io.out.bits.decode.data.src1 := src1(dequeueSelect)
  io.out.bits.decode.data.src2 := src2(dequeueSelect)

  when(io.flush){
    List.tabulate(rsSize)(i => 
      valid(i) := false.B
    )
  }

}