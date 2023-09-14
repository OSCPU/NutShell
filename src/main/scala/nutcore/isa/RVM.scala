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

object RV32MInstr extends HasInstrType with HasNutCoreParameter {
  def MUL     = BitPat("b0000001_?????_?????_000_?????_0110011")
  def MULH    = BitPat("b0000001_?????_?????_001_?????_0110011")
  def MULHSU  = BitPat("b0000001_?????_?????_010_?????_0110011")
  def MULHU   = BitPat("b0000001_?????_?????_011_?????_0110011")
  def DIV     = BitPat("b0000001_?????_?????_100_?????_0110011")
  def DIVU    = BitPat("b0000001_?????_?????_101_?????_0110011")
  def REM     = BitPat("b0000001_?????_?????_110_?????_0110011")
  def REMU    = BitPat("b0000001_?????_?????_111_?????_0110011")
  def MULW    = BitPat("b0000001_?????_?????_000_?????_0111011")
  def DIVW    = BitPat("b0000001_?????_?????_100_?????_0111011")
  def DIVUW   = BitPat("b0000001_?????_?????_101_?????_0111011")
  def REMW    = BitPat("b0000001_?????_?????_110_?????_0111011")
  def REMUW   = BitPat("b0000001_?????_?????_111_?????_0111011")

  val mulTable = Array(
    MUL            -> List(InstrR, FuType.mdu, MDUOpType.mul),
    MULH           -> List(InstrR, FuType.mdu, MDUOpType.mulh),
    MULHSU         -> List(InstrR, FuType.mdu, MDUOpType.mulhsu),
    MULHU          -> List(InstrR, FuType.mdu, MDUOpType.mulhu)
  )
  val divTable = Array(
    DIV            -> List(InstrR, FuType.mdu, MDUOpType.div),
    DIVU           -> List(InstrR, FuType.mdu, MDUOpType.divu),
    REM            -> List(InstrR, FuType.mdu, MDUOpType.rem),
    REMU           -> List(InstrR, FuType.mdu, MDUOpType.remu)
  )
  val table = mulTable ++ (if (HasDiv) divTable else Array.empty)
}

object RV64MInstr extends HasInstrType with HasNutCoreParameter {
  def MULW    = BitPat("b0000001_?????_?????_000_?????_0111011")
  def DIVW    = BitPat("b0000001_?????_?????_100_?????_0111011")
  def DIVUW   = BitPat("b0000001_?????_?????_101_?????_0111011")
  def REMW    = BitPat("b0000001_?????_?????_110_?????_0111011")
  def REMUW   = BitPat("b0000001_?????_?????_111_?????_0111011")

  val mulTable = Array(
    MULW           -> List(InstrR, FuType.mdu, MDUOpType.mulw)
  )
  val divTable = Array(
    DIVW           -> List(InstrR, FuType.mdu, MDUOpType.divw),
    DIVUW          -> List(InstrR, FuType.mdu, MDUOpType.divuw),
    REMW           -> List(InstrR, FuType.mdu, MDUOpType.remw),
    REMUW          -> List(InstrR, FuType.mdu, MDUOpType.remuw)
  )
  val table = mulTable ++ (if (HasDiv) divTable else Array.empty)
}

object RVMInstr extends HasNutCoreParameter {
  val table = RV32MInstr.table ++ (if (XLEN == 64) RV64MInstr.table else Array.empty)
}
