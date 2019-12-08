package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

trait HasIBUFConst{
  val multiIssue = true
  val instUnitWidth = 16 //bit
  val ibufBitSize = 256 //bit
  val ibufSize = ibufBitSize / instUnitWidth
}

class IBF extends NOOPModule with HasInstrType with HasIBUFConst{
  val io = IO(new Bundle {
    // val in = Flipped(Decoupled(new CtrlFlowIO))
    val in = Flipped(Decoupled(new FrontendIO))
    val out = Decoupled(new CtrlFlowIO)
    val out1 = Decoupled(new CtrlFlowIO)
    val out2 = Decoupled(new CtrlFlowIO)
    val flush = Input(Bool())
    val redirect = new RedirectIO
  })

  //ibuf reg
  // val instBuffer = RegInit(0.U(ibufBitSize.W))
  val ringInstBuffer = RegInit(VecInit(Seq.fill(ibufSize)(0.U(16.W))))
  val pcRingMeta = RegInit(VecInit(Seq.fill(ibufSize)(0.U(VAddrBits.W))))
  val npcRingMeta = RegInit(VecInit(Seq.fill(ibufSize)(0.U(VAddrBits.W))))
  val validRingMeta = RegInit(VecInit(Seq.fill(ibufSize)(false.B)))
  val branchRingMeta = RegInit(VecInit(Seq.fill(ibufSize)(false.B)))
  val ipfRingMeta = RegInit(VecInit(Seq.fill(ibufSize)(false.B)))
  val ringBufferHead = RegInit(0.U(log2Up(ibufSize).W))
  val ringBufferTail = RegInit(0.U(log2Up(ibufSize).W))
  val ringBufferEmpty = ringBufferHead === ringBufferTail && !validRingMeta(ringBufferHead)
  val ringBufferAllowin = (0 to 3).map(i => !validRingMeta(ringBufferHead+i.U)).foldRight(true.B)((sum,i)=>sum&i)

  //ibuf decode
  val instr = io.in.instr
  val instrVec = Wire(Vec(4, UInt(16.W)))
  val isRVC = Wire(Vec(4, Bool()))
  val instValid = io.in.instValid
  val brIdx = io.in.brIdx
  val icachePF = io.in.icachePF
  instrVec := instr.asTypeOf(Vec(4, UInt(16.W)))
  (0 to 3).map(i => isRVC(i.U) := instrVec(i.U)(1,0) =/= "b11".U)
  
  //ibuf enqueue
  //if valid & ringBufferAllowin, enqueue
  val needEnqueue = Wire(Vec(4, Bool()))
  needEnqueue(0) := instValid(0)
  needEnqueue(1) := instValid(1) && !(brIdx(0) && isRVC(0))
  needEnqueue(2) := instValid(2) && !(brIdx(0)) && !(brIdx(1) && isRVC(1))
  needEnqueue(3) := instValid(3) && !(brIdx(0)) && !(brIdx(1)) && !(brIdx(2) && isRVC(2))

  // NOTE: needEnqueue is always of fmt "0?1?0?"
  // therefore we first shift input data, then enqueue
  val enqueueSize = List.tabulate(4)(i => needEnqueue(i).asUInt).foldRight(0.U)((sum, i)=>sum+i) // count(true) in needEnqueue
  val shiftSize = Mux(needEnqueue(0), 0.U, Mux(needEnqueue(1), 1.U, Mux(needEnqueue(2), 2.U, 3.U))) // count 0 in low addr in needEnqueue
  val enqueueFire = (0 to 3).map(i => enqueueSize >= (i+1).U)

  val ibufWen = io.in.fire() // i.e. ringBufferAllowin && io.in.valid
  def ibufWrite(targetSlot: Int, shiftSize: UInt){
      ringInstBuffer(targetSlot.U + ringBufferHead) := instrVec(shiftSize + targetSlot.U)
      pcRingMeta(targetSlot.U + ringBufferHead) := io.in.bits.pc(shiftSize + targetSlot.U)
      npcRingMeta(targetSlot.U + ringBufferHead) := io.in.bits.npc(shiftSize + targetSlot.U)
      validRingMeta(targetSlot.U + ringBufferHead) := true.B
      branchRingMeta(targetSlot.U + ringBufferHead) := io.in.bits.brIdx(shiftSize + targetSlot.U)
      ipfRingMeta(targetSlot.U + ringBufferHead) := io.in.bits.icachePF(shiftSize + targetSlot.U)
  }
  when(ibufWen){
    when(enqueueFire(0)){ibufWrite(0, enqueueSrc(0))}
    when(enqueueFire(1)){ibufWrite(1, enqueueSrc(1))}
    when(enqueueFire(2)){ibufWrite(2, enqueueSrc(2))}
    when(enqueueFire(3)){ibufWrite(3, enqueueSrc(3))}
    ringBufferHead := ringBufferHead + enqueueSize
    Debug(){
      printf("[IBUF] ibuf enqueue: \n")
      when(enqueueFire(0)){printf("[IBUF] inst %x pc %x npc %x br %x ipf %x eqsrc %x\n", instrVec(enqueueSrc(0)), io.in,bits.pc(enqueueSrc(0)), io.in,bits.npc(enqueueSrc(0)), io.in.bits.brIdx(enqueueSrc(0)), io.in.bits.icachePF(enqueueSrc(0)), enqueueSrc(0))}
      when(enqueueFire(1)){printf("[IBUF] inst %x pc %x npc %x br %x ipf %x eqsrc %x\n", instrVec(enqueueSrc(1)), io.in,bits.pc(enqueueSrc(1)), io.in,bits.npc(enqueueSrc(1)), io.in.bits.brIdx(enqueueSrc(1)), io.in.bits.icachePF(enqueueSrc(1)), enqueueSrc(1))}
      when(enqueueFire(2)){printf("[IBUF] inst %x pc %x npc %x br %x ipf %x eqsrc %x\n", instrVec(enqueueSrc(2)), io.in,bits.pc(enqueueSrc(2)), io.in,bits.npc(enqueueSrc(2)), io.in.bits.brIdx(enqueueSrc(2)), io.in.bits.icachePF(enqueueSrc(2)), enqueueSrc(2))}
      when(enqueueFire(3)){printf("[IBUF] inst %x pc %x npc %x br %x ipf %x eqsrc %x\n", instrVec(enqueueSrc(3)), io.in,bits.pc(enqueueSrc(3)), io.in,bits.npc(enqueueSrc(3)), io.in.bits.brIdx(enqueueSrc(3)), io.in.bits.icachePF(enqueueSrc(3)), enqueueSrc(3))}
    }
  }

  io.in.ready := ringBufferAllowin // || used to be !io.in.valid, do not know what's for

  //ibuf dequeue
  //there are 2 dequeue sockets

  //dequeue inst select
  val dequeueInstrVec = Wire(Vec(4, UInt(16.W)))
  val dequeueIsValid = Wire(Vec(4, Bool()))
  val dequeueIsRVC = Wire(Vec(4, Bool()))
  (0 to 3).map(i => dequeueInstrVec(i.U) := ringInstBuffer(i.U + ringBufferTail))
  (0 to 3).map(i => dequeueIsValid(i.U) := validRingMeta(i.U + ringBufferTail))
  (0 to 3).map(i => dequeueIsRVC(i.U) := dequeueInstrVec(1,0)=/="b11".U)

  //dequeue socket 1
  io.out1.bits := DontCare
  io.out1.bits.redirect.valid := false.B
  io.out1.bits.pc := pcRingMeta(ringBufferTail)
  io.out1.bits.pnpc := npcRingMeta(ringBufferTail)
  io.out1.bits.instr := Cat(ringInstBuffer(ringBufferTail+1.U), ringInstBuffer(ringBufferTail))
  io.out1.bits.brIdx := branchRingMeta(ringBufferTail)

  io.out1.valid := dequeueIsValid(0) && (dequeueIsRVC(0) || dequeueIsValid(1))
  io.out1.bits.exceptionVec.map(_ => false.B)
  io.out1.bits.exceptionVec(instrPageFault) := ipfRingMeta(ringBufferTail)
  val dequeueSize1 = Mux(io.out1.fire(), 0.U, Mux(dequeueIsRVC(0), 1.U, 2.U)) // socket 2 will use dequeueSize1 to get its inst

  //dequeue socket 2
  val inst2_StartIndex = ringBufferTail + dequeueSize1
  io.out2.bits := DontCare
  io.out2.bits.redirect.valid := false.B
  io.out2.bits.pc := pcRingMeta(inst2_StartIndex)
  io.out2.bits.pnpc := npcRingMeta(inst2_StartIndex)
  io.out2.bits.instr := Cat(ringInstBuffer(inst2_StartIndex+1.U), ringInstBuffer(inst2_StartIndex))
  io.out2.bits.brIdx := branchRingMeta(inst2_StartIndex)

  io.out2.valid := dequeueIsValid(dequeueSize1) && (dequeueIsRVC(dequeueSize1) || dequeueIsValid(dequeueSize1 + 1.U))
  io.out2.bits.exceptionVec.map(_ => false.B)
  io.out2.bits.exceptionVec(instrPageFault) := ipfRingMeta(inst2_StartIndex)
  val dequeueSize2 = Mux(io.out2.fire(), 0.U, Mux(dequeueIsRVC(inst2_StartIndex), 1.U, 2.U)) // socket 2 will use dequeueSize1 to get its inst

  val dequeueSize = dequeueSize1 + dequeueSize2

  //dequeue control
  val dequeueFire = dequeueSize > 0.U
  when(dequeueFire){
    when(dequeueSize >= 1.U){validRingMeta(0.U + ringBufferTail) := false.B}
    when(dequeueSize >= 2.U){validRingMeta(1.U + ringBufferTail) := false.B}
    when(dequeueSize >= 3.U){validRingMeta(2.U + ringBufferTail) := false.B}
    when(dequeueSize >= 4.U){validRingMeta(3.U + ringBufferTail) := false.B}
    ringBufferTail := ringBufferTail + dequeueSize;
  }

  //redirect at ibuf is no longer necessary
  io.redirect.target := DontCare
  io.redirect.valid := false

}
