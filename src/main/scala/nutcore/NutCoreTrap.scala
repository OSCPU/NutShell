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

  // custom-trap: opcode=1101011, funct3=000. Matches NEMU nemu_trap (e.g. 0x0005006b).
  // DUT executes it as add (typically rd=x0). DiffTest uses a0 to tell control
  // markers (0x100/0x101, keep running) from exit codes (0/1, stop the sim).
  def TRAP    = BitPat("b????????????_?????_000_?????_1101011")
  val table = Array(TRAP -> List(InstrI, FuType.alu, ALUOpType.add))
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
