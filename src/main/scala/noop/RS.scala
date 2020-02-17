package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

trait HasRSConst{
  val rsSize = 2
  val rsCommitWidth = 2
}

// Reservation Station
class RS extends NOOPModule with HasRSConst with HasROBConst {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new RenamedDecodeIO))
    val out = Decoupled(new RenamedDecodeIO)
    val commit = Vec(rsCommitWidth, Flipped(Decoupled(new OOCommitIO)))
    val flush = Input(Bool())
  })

  val decode = Vec(rsSize, Reg(new RenamedDecodeIO)) // TODO: decouple DataSrcIO from DecodeIO
  val valid = Vec(rsSize, RegInit(false.B))
  val src1Rdy = Vec(rsSize, RegInit(false.B))
  val src2Rdy = Vec(rsSize, RegInit(false.B))
  val prfDest = Vec(rsSize, Reg(UInt(prfAddrWidth.W)))
  val prfSrc1 = Vec(rsSize, Reg(UInt(prfAddrWidth.W)))
  val prfSrc2 = Vec(rsSize, Reg(UInt(prfAddrWidth.W)))
  val src1 = Vec(rsSize, Reg(UInt(XLEN.W)))
  val src2 = Vec(rsSize, Reg(UInt(XLEN.W)))
//   val imm = Vec(rsSize, Reg(UInt(XLEN.W)))

  val instRdy = List.tabulate(rsSize)(i => src1Rdy(i) && src2Rdy(i) && valid(i))

  val ringBufferHead = RegInit(0.U(log2Up(rsSize).W)) //TODO
  val ringBufferTail = RegInit(0.U(log2Up(rsSize).W)) //TODO
  val ringBufferEmpty = ringBufferHead === ringBufferTail && !valid(ringBufferHead) //TODO
  val ringBufferFull = ringBufferTail === ringBufferHead && valid(ringBufferHead) //TODO
  val ringBufferAllowin = !ringBufferFull //TODO
  val ringBufferReadygo = !ringBufferFull //TODO

  // Listen to Common Data Bus
  // Here we listen to commit signal chosen by ROB?
  // If prf === src, mark it as `ready`

  List.tabulate(rsSize)(i => 
    when(valid(i)){
      List.tabulate(rsCommitWidth)(j =>
        when(!src1Rdy(i) && src1(i) === io.commit(j).bits.prfidx && io.commit(j).valid){
            src1Rdy(i) := true.B
            src1(i) := io.commit(j).bits.commits
        }
      )
      List.tabulate(rsCommitWidth)(j =>
        when(!src2Rdy(i) && src2(i) === io.commit(j).bits.prfidx && io.commit(j).valid){
            src2Rdy(i) := true.B
            src2(i) := io.commit(j).bits.commits
        }
      )
    }
  )

  // RS enqueue
  io.in.ready := ringBufferAllowin
  when(io.in.fire()){
    decode(ringBufferHead) := io.in.bits
    ringBufferHead := ringBufferHead + 1.U
    prfSrc1 := io.in.bits.prfSrc1
    prfSrc2 := io.in.bits.prfSrc2
    src1Rdy := io.in.bits.src1Rdy
    src2Rdy := io.in.bits.src2Rdy
    src1 := io.in.bits.decode.data.src1
    src2 := io.in.bits.decode.data.src2
  }

  // RS dequeue
  io.out.valid := false.B //TODO
  when(io.out.fire()){
    //TODO
  }

  val EnableOutOfOrderDequeue = true
  val dequeueSelect = 
    if(EnableOutOfOrderDequeue){
      PriorityEncoder(instRdy) // TODO: replace PriorityEncoder with other logic
    }else{
      ringBufferTail
    }
  io.out.bits.decode := decode(dequeueSelect)
  io.out.bits.prfDest := prfDest(dequeueSelect)
  io.out.bits.prfSrc1 := DontCare
  io.out.bits.prfSrc2 := DontCare

}