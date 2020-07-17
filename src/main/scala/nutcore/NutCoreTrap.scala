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

object NutCoreTrap extends HasInstrType {
  def StateGoodTrap  = 0.U
  def StateBadTrap   = 1.U
  def StateInvOpcode = 2.U
  def StateRunning   = 3.U

  def TRAP    = BitPat("b????????????_?????_000_?????_1101011")
  val table = Array(TRAP -> List(InstrI, FuType.csr, CSROpType.set))
}

class Monitor extends BlackBox {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val reset = Input(Bool())
    val isNutCoreTrap = Input(Bool())
    val trapCode = Input(UInt(32.W))
    val trapPC = Input(UInt(64.W))
    val cycleCnt = Input(UInt(64.W))
    val instrCnt = Input(UInt(64.W))
  })
}
