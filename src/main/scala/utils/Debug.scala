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

package utils

import chisel3._
import chisel3.util._

import nutcore.NutCoreConfig

object Debug {
  def apply(flag: Boolean = NutCoreConfig().EnableDebug, cond: Bool = true.B)(body: => Unit): Any =
    if (flag) { when (cond && GTimer() > 0.U) { body } } // 2645000
    // if (flag) { when (cond && GTimer() > 11125600.U) { body } } // 2645000
}

object ShowType {
  def apply[T: Manifest](t: T) = println(manifest[T])
}
