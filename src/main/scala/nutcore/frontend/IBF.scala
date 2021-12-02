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

trait HasIBUFConst{
  // val multiIssue = true
  val instUnitWidth = 16 //bit
  val ibufBitSize = 128 //256 //bit
  val ibufSize = ibufBitSize / instUnitWidth
}

// 2-width Instruction Align Buffer
class IBF extends NutCoreModule with HasInstrType with HasIBUFConst{
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new InstFetchIO))
    val out = Vec(2, Decoupled(new CtrlFlowIO))
    val flush = Input(Bool())
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
  val instr = io.in.bits.instr
  val instrVec = Wire(Vec(4, UInt(16.W)))
  val isRVC = Wire(Vec(4, Bool()))
  val instValid = io.in.bits.instValid
  val brIdx = io.in.bits.brIdx // NOTE: brIdx == false.B if !instValid
  val icachePF = io.in.bits.icachePF
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
  // val enqueueSize = List.tabulate(4)(i => needEnqueue(i).asUInt).foldRight(0.U)((sum, i)=>sum+&i) // count(true) in needEnqueue
  val enqueueSize = needEnqueue(0).asUInt+&needEnqueue(1).asUInt+&needEnqueue(2).asUInt+&needEnqueue(3).asUInt // count(true) in needEnqueue
  val shiftSize = Mux(needEnqueue(0), 0.U, Mux(needEnqueue(1), 1.U, Mux(needEnqueue(2), 2.U, 3.U))) // count 0 in low addr in needEnqueue
  val enqueueFire = (0 to 3).map(i => enqueueSize >= (i+1).U)

  val ibufWen = io.in.fire // i.e. ringBufferAllowin && io.in.valid
  def ibufWrite(targetSlot: Int, shiftSize: UInt){
      ringInstBuffer(targetSlot.U + ringBufferHead) := instrVec(shiftSize + targetSlot.U)
      pcRingMeta(targetSlot.U + ringBufferHead) := Cat(io.in.bits.pc(VAddrBits-1, 3), shiftSize + targetSlot.U, 0.U(1.W))
      npcRingMeta(targetSlot.U + ringBufferHead) := io.in.bits.pnpc
      validRingMeta(targetSlot.U + ringBufferHead) := true.B
      branchRingMeta(targetSlot.U + ringBufferHead) := io.in.bits.brIdx(shiftSize + targetSlot.U)
      ipfRingMeta(targetSlot.U + ringBufferHead) := io.in.bits.icachePF
  }
  when(ibufWen){
    when(enqueueFire(0)){ibufWrite(0, shiftSize)}
    when(enqueueFire(1)){ibufWrite(1, shiftSize)}
    when(enqueueFire(2)){ibufWrite(2, shiftSize)}
    when(enqueueFire(3)){ibufWrite(3, shiftSize)}
    ringBufferHead := ringBufferHead + enqueueSize
    Debug("ibuf enqueue:\n")
    Debug("instValid %b brIdx %b isRVC %b needEnqueue %b enqueueSize %x shiftSize %x\n", instValid.asUInt,brIdx.asUInt,isRVC.asUInt,needEnqueue.asUInt,enqueueSize.asUInt,shiftSize.asUInt)
    Debug(enqueueFire(0), "inst %x pc %x npc %x br %x ipf %x eqsrc %x\n", instrVec(shiftSize+0.U), Cat(io.in.bits.pc(VAddrBits-1, 3), shiftSize + 0.U, 0.U(1.W)), io.in.bits.pnpc, io.in.bits.brIdx(shiftSize+0.U), io.in.bits.icachePF, shiftSize+0.U)
    Debug(enqueueFire(1), "inst %x pc %x npc %x br %x ipf %x eqsrc %x\n", instrVec(shiftSize+1.U), Cat(io.in.bits.pc(VAddrBits-1, 3), shiftSize + 1.U, 0.U(1.W)), io.in.bits.pnpc, io.in.bits.brIdx(shiftSize+1.U), io.in.bits.icachePF, shiftSize+1.U)
    Debug(enqueueFire(2), "inst %x pc %x npc %x br %x ipf %x eqsrc %x\n", instrVec(shiftSize+2.U), Cat(io.in.bits.pc(VAddrBits-1, 3), shiftSize + 2.U, 0.U(1.W)), io.in.bits.pnpc, io.in.bits.brIdx(shiftSize+2.U), io.in.bits.icachePF, shiftSize+2.U)
    Debug(enqueueFire(3), "inst %x pc %x npc %x br %x ipf %x eqsrc %x\n", instrVec(shiftSize+3.U), Cat(io.in.bits.pc(VAddrBits-1, 3), shiftSize + 3.U, 0.U(1.W)), io.in.bits.pnpc, io.in.bits.brIdx(shiftSize+3.U), io.in.bits.icachePF, shiftSize+3.U)
  }

  io.in.ready := ringBufferAllowin || !io.in.valid// used to be !io.in.valid, do not know what's for

  //ibuf dequeue
  //there are 2 dequeue sockets

  //dequeue inst select
  val dequeueInstrVec = Wire(Vec(4, UInt(16.W)))
  val dequeueIsValid = Wire(Vec(4, Bool()))
  val dequeueIsRVC = Wire(Vec(4, Bool()))
  (0 to 3).map(i => dequeueInstrVec(i.U) := ringInstBuffer(i.U + ringBufferTail))
  (0 to 3).map(i => dequeueIsValid(i.U) := validRingMeta(i.U + ringBufferTail))
  (0 to 3).map(i => dequeueIsRVC(i.U) := dequeueInstrVec(i.U)(1,0)=/="b11".U)

  //dequeue socket 1
  io.out(0).bits := DontCare
  io.out(0).bits.redirect.valid := false.B
  io.out(0).bits.pc := pcRingMeta(ringBufferTail)
  io.out(0).bits.pnpc := npcRingMeta(ringBufferTail)
  io.out(0).bits.instr := Cat(ringInstBuffer(ringBufferTail+1.U), ringInstBuffer(ringBufferTail))
  io.out(0).bits.brIdx := branchRingMeta(ringBufferTail)
  io.out(0).bits.isRVC := dequeueIsRVC(0)
  io.out(0).bits.crossPageIPFFix := !ipfRingMeta(ringBufferTail) && !dequeueIsRVC(0) && ipfRingMeta(ringBufferTail + 1.U)

  io.out(0).valid := dequeueIsValid(0) && (dequeueIsRVC(0) || dequeueIsValid(1)) && !io.flush
  io.out(0).bits.exceptionVec.map(_ => false.B)
  io.out(0).bits.exceptionVec(instrPageFault) := ipfRingMeta(ringBufferTail) || !dequeueIsRVC(0) && ipfRingMeta(ringBufferTail + 1.U)
  val dequeueSize1 = Mux(io.out(0).fire, Mux(dequeueIsRVC(0), 1.U, 2.U), 0.U) // socket 2 will use dequeueSize1 to get its inst
    Debug(io.out(0).fire, "dequeue: bufferhead %x buffertail %x\n", ringBufferHead, ringBufferTail)
    Debug(io.out(0).fire, "dequeue1: inst %x pc %x npc %x br %x ipf %x(%x)\n", io.out(0).bits.instr, io.out(0).bits.pc, io.out(0).bits.pnpc, io.out(0).bits.brIdx, io.out(0).bits.exceptionVec(instrPageFault), io.out(0).bits.crossPageIPFFix)

  //dequeue socket 2
  val inst2_StartIndex = ringBufferTail + dequeueSize1
  io.out(1).bits := DontCare
  io.out(1).bits.redirect.valid := false.B
  io.out(1).bits.pc := pcRingMeta(inst2_StartIndex)
  io.out(1).bits.pnpc := npcRingMeta(inst2_StartIndex)
  // io.out(1).bits.pnpc := npcRingMeta(inst2_StartIndex)
  io.out(1).bits.instr := Cat(ringInstBuffer(inst2_StartIndex+1.U), ringInstBuffer(inst2_StartIndex))
  io.out(1).bits.brIdx := branchRingMeta(inst2_StartIndex)
  io.out(1).bits.isRVC := dequeueIsRVC(dequeueSize1)
  io.out(1).bits.crossPageIPFFix := !ipfRingMeta(inst2_StartIndex) && !dequeueIsRVC(dequeueSize1) && ipfRingMeta(inst2_StartIndex + 1.U)

  if(EnableMultiIssue){
    io.out(1).valid := dequeueIsValid(dequeueSize1) && (dequeueIsRVC(dequeueSize1) || dequeueIsValid(dequeueSize1 + 1.U)) && !io.flush
  }else{
    io.out(1).valid := false.B
  }
  io.out(1).bits.exceptionVec.map(_ => false.B)
  io.out(1).bits.exceptionVec(instrPageFault) := ipfRingMeta(inst2_StartIndex) || !dequeueIsRVC(dequeueSize1) && ipfRingMeta(inst2_StartIndex + 1.U)
  val dequeueSize2 = Mux(io.out(1).fire, Mux(dequeueIsRVC(dequeueSize1), 1.U, 2.U), 0.U) // socket 2 will use dequeueSize1 to get its inst
  Debug(io.out(1).fire, "dequeue2: inst %x pc %x npc %x br %x ipf %x(%x)\n", io.out(1).bits.instr, io.out(1).bits.pc, io.out(1).bits.pnpc, io.out(1).bits.brIdx, io.out(1).bits.exceptionVec(instrPageFault), io.out(1).bits.crossPageIPFFix)

  val dequeueSize = dequeueSize1 +& dequeueSize2

  //dequeue control
  val dequeueFire = dequeueSize > 0.U
  when(dequeueFire){
    when(dequeueSize >= 1.U){validRingMeta(0.U + ringBufferTail) := false.B}
    when(dequeueSize >= 2.U){validRingMeta(1.U + ringBufferTail) := false.B}
    when(dequeueSize >= 3.U){validRingMeta(2.U + ringBufferTail) := false.B}
    when(dequeueSize >= 4.U){validRingMeta(3.U + ringBufferTail) := false.B}
    ringBufferTail := ringBufferTail + dequeueSize;
    Debug("ibuf dequeue %x*16 bits\n", dequeueSize)
  }

  //flush control
  when(io.flush){
    ringBufferHead := 0.U
    ringBufferTail := 0.U
    List.tabulate(ibufSize)(i => validRingMeta(i) := 0.U) // set valid to 0
  }

  //redirect at ibuf is no longer necessary
  // io.redirect.target := DontCare
  // io.redirect.valid := false.B

}
