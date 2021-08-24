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

import top.Settings

object Priviledged extends HasInstrType {
  def ECALL   = BitPat("b000000000000_00000_000_00000_1110011")
  def EBREAK  = BitPat("b000000000001_00000_000_00000_1110011")
  def MRET    = BitPat("b001100000010_00000_000_00000_1110011")
  def SRET    = BitPat("b000100000010_00000_000_00000_1110011")
  def URET    = BitPat("b000000000010_00000_000_00000_1110011")
  def SFANCE_VMA = BitPat("b0001001_?????_?????_000_00000_1110011")
  def FENCE   = BitPat("b????????????_?????_000_?????_0001111")
  def WFI     = BitPat("b0001000_00101_00000_000_00000_1110011") 

  val table_u = Array(
    URET           -> List(InstrI, FuType.csr, CSROpType.jmp)
  )

  val table_s = Array(
    SRET           -> List(InstrI, FuType.csr, CSROpType.jmp),
    SFANCE_VMA     -> List(InstrR, FuType.mou, MOUOpType.sfence_vma)
  )

  val table = Array(
    ECALL          -> List(InstrI, FuType.csr, CSROpType.jmp),
    EBREAK         -> List(InstrI, FuType.csr, CSROpType.jmp),
    MRET           -> List(InstrI, FuType.csr, CSROpType.jmp),
    FENCE          -> List(InstrS, FuType.mou, MOUOpType.fence), // nop    InstrS -> !wen
    WFI            -> List(InstrI, FuType.alu, ALUOpType.add) // nop
    // FENCE          -> List(InstrB, FuType.mou, MOUOpType.fencei)
  ) ++ (if (!Settings.get("MmodeOnly")) table_s else Nil) ++ (if (Settings.get("EnableRVN")) table_u else Nil)
}
