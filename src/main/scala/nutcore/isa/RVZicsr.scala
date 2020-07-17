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

object RVZicsrInstr extends HasInstrType {
  def CSRRW   = BitPat("b????????????_?????_001_?????_1110011")
  def CSRRS   = BitPat("b????????????_?????_010_?????_1110011")
  def CSRRC   = BitPat("b????????????_?????_011_?????_1110011")
  def CSRRWI  = BitPat("b????????????_?????_101_?????_1110011")
  def CSRRSI  = BitPat("b????????????_?????_110_?????_1110011")
  def CSRRCI  = BitPat("b????????????_?????_111_?????_1110011")

  val table = Array(
    CSRRW          -> List(InstrI, FuType.csr, CSROpType.wrt),
    CSRRS          -> List(InstrI, FuType.csr, CSROpType.set),
    CSRRC          -> List(InstrI, FuType.csr, CSROpType.clr),
    CSRRWI         -> List(InstrI, FuType.csr, CSROpType.wrti),
    CSRRSI         -> List(InstrI, FuType.csr, CSROpType.seti),
    CSRRCI         -> List(InstrI, FuType.csr, CSROpType.clri)
  )
}
