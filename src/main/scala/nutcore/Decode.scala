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

trait HasInstrType {
  def InstrN  = "b0000".U
  def InstrI  = "b0100".U
  def InstrR  = "b0101".U
  def InstrS  = "b0010".U
  def InstrB  = "b0001".U
  def InstrU  = "b0110".U
  def InstrJ  = "b0111".U
  def InstrA  = "b1110".U
  def InstrSA = "b1111".U // Atom Inst: SC
  def InstrVSI= "b1100".U // vector - vsetvli
  def InstrVS = "b1001".U // src1:scala  - src2:vector
  def InstrVV = "b1010".U // src1:vector - src2:vector
  def InstrVI = "b1010".U // src1:imm    - src2:vector. use InstrVV
  def InstrNS = "b1011".U // src1:scala  - src2:none
  def InstrSS = "b0011".U // src1:scala  - src2:scala
  def InstrVSB= "b1101".U // src1:None   - src2:vector, write back

  def isrfWen(instrType : UInt): Bool = instrType(2)
}

// trait CompInstConst {
//   val RVCRegNumTable = Array(
//     BitPat("b000") -> 8.U,
//     BitPat("b001") -> 9.U,
//     BitPat("b010") -> 10.U,
//     BitPat("b011") -> 11.U,
//     BitPat("b100") -> 12.U,
//     BitPat("b101") -> 13.U,
//     BitPat("b110") -> 14.U,
//     BitPat("b111") -> 15.U
//   )
// }

object SrcType {
  def reg = "b0".U
  def pc  = "b1".U
  def imm = "b1".U
  def vreg = "b1".U
  def none = "b1".U
  def apply() = UInt(1.W)
}

object FuType extends HasNutCoreConst {
  def num = 7
  def alu = "b000".U
  def lsu = "b001".U
  def mdu = "b010".U
  def csr = "b011".U
  def mou = "b100".U
  def vmu = "b101".U
  def vxu = "b110".U
  def bru = if(IndependentBru) "b111".U
            else               alu
  def apply() = UInt(log2Up(num).W)
}

object FuOpType {
  def apply() = UInt(9.W) // longer to fit VXU
}

object Instructions extends HasInstrType with HasNutCoreParameter {
  def NOP = 0x00000013.U
  val DecodeDefault = List(InstrN, FuType.csr, CSROpType.jmp)
  def DecodeTable = RVIInstr.table ++ NutCoreTrap.table ++
    (if (HasMExtension) RVMInstr.table else Nil) ++
    (if (HasCExtension) RVCInstr.table else Nil) ++
    Priviledged.table ++
    RVAInstr.table ++
    RVVInstr.table ++
    RVZicsrInstr.table ++ RVZifenceiInstr.table ++
    DasicsInstr.table
}

object CInstructions extends HasInstrType with HasNutCoreParameter{
  def NOP = 0x00000013.U
  val DecodeDefault = List(RVCInstr.ImmNone, RVCInstr.DtCare, RVCInstr.DtCare, RVCInstr.DtCare)
  // val DecodeDefault = List(InstrN, FuType.csr, CSROpType.jmp)
  def CExtraDecodeTable = RVCInstr.cExtraTable
}
