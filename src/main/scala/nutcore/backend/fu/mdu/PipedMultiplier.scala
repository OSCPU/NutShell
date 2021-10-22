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

class PipedMultiplier(len: Int) extends Multiplier(len) {
  val latency = 1

  def DSPInPipe[T <: Data](a: T) = RegNext(a)
  def DSPOutPipe[T <: Data](a: T) = RegNext(RegNext(RegNext(a)))
  val mulRes = (DSPInPipe(io.in.bits(0)).asSInt * DSPInPipe(io.in.bits(1)).asSInt)
  io.out.bits := DSPOutPipe(mulRes).asUInt
  io.out.valid := DSPOutPipe(DSPInPipe(io.in.fire))

  val busy = RegInit(false.B)
  when (io.in.valid && !busy) { busy := true.B }
  when (io.out.valid) { busy := false.B }
  io.in.ready := (if (latency == 0) true.B else !busy)
}
