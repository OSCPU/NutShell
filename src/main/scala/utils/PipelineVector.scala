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

object PipelineVector2Connect {
  def apply[T <: Data](gen: T, in1: DecoupledIO[T], in2: DecoupledIO[T], out1: DecoupledIO[T], out2: DecoupledIO[T], flush: Bool, bufferSize: Int) = {

    //ring buffer
    val dataBuffer = RegInit(VecInit(Seq.fill(bufferSize)(0.U.asTypeOf(gen))))
    val ringBufferHead = RegInit(0.U(log2Up(bufferSize).W))
    val ringBufferTail = RegInit(0.U(log2Up(bufferSize).W))
    val ringBufferEmpty = ringBufferHead === ringBufferTail
    val ringBufferAllowin = (0 to 1).map(i => (ringBufferHead + (i+1).U) =/= ringBufferTail).foldRight(true.B)((sum,i)=>sum&i)
    
    //enqueue
    val needEnqueue = Wire(Vec(2, Bool()))
    needEnqueue(0) := in1.valid
    needEnqueue(1) := in2.valid

    val enqueueSize = needEnqueue(0).asUInt+&needEnqueue(1).asUInt // count(true) in needEnqueue
    val enqueueFire = (0 to 1).map(i => enqueueSize >= (i+1).U)

    val wen = in1.fire || in2.fire // i.e. ringBufferAllowin && in.valid
    when(wen){
        when(enqueueFire(0)){dataBuffer(0.U + ringBufferHead) := Mux(needEnqueue(0), in1.bits, in2.bits)}
        when(enqueueFire(1)){dataBuffer(1.U + ringBufferHead) := in2.bits}
        ringBufferHead := ringBufferHead + enqueueSize
    }

    in1.ready := ringBufferAllowin || !in1.valid
    in2.ready := ringBufferAllowin || !in2.valid

    //dequeue socket 1
    val deq1_StartIndex = ringBufferTail
    out1.bits := dataBuffer(deq1_StartIndex)
    out1.valid := ringBufferHead =/= deq1_StartIndex

    //dequeue socket 2
    val deq2_StartIndex = ringBufferTail + 1.U
    out2.bits := dataBuffer(deq2_StartIndex)
    out2.valid := ringBufferHead =/= deq2_StartIndex && out1.valid

    //dequeue control
    val dequeueSize = out1.fire.asUInt +& out2.fire.asUInt
    val dequeueFire = dequeueSize > 0.U
    when(dequeueFire){
        ringBufferTail := ringBufferTail + dequeueSize;
    }

    //flush control
    when(flush){
        ringBufferHead := 0.U
        ringBufferTail := 0.U
    }

    Debug(){
        printf("[DPQ] size %x head %x tail %x enq %x deq %x\n", (bufferSize.asUInt +& ringBufferHead.asUInt - ringBufferTail.asUInt) % bufferSize.asUInt, ringBufferHead, ringBufferTail ,enqueueSize, dequeueSize)
    }

  }
}
