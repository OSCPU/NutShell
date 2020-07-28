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

object PreDecode {
  def C_JAL      = BitPat("b001_?_??_???_??_???_01")
  def C_J        = BitPat("b101_?_??_???_??_???_01")
  def C_BEQZ     = BitPat("b110_?_??_???_??_???_01")
  def C_BNEZ     = BitPat("b111_?_??_???_??_???_01")
  def RVI_BRANCH = BitPat("b???_?_??_???_11_???_11")

  val branchTable = Array(
    C_JAL      -> List(true.B),
    C_J        -> List(true.B),
    C_BEQZ     -> List(true.B),
    C_BNEZ     -> List(true.B),
    RVI_BRANCH -> List(true.B)
  )
}