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

package device

import chisel3._
import chisel3.util._

import bus.axi4._
import utils._

class AXI4Flash extends AXI4SlaveModule(new AXI4Lite) {
  val jmpToDramInstr1 = "h0010029b".U  // addiw t0,zero,1
  val jmpToDramInstr2 = "h01f29293".U  // slli  t0,t0,0x1f
  val jmpToDramInstr3 = "h00028067".U  // jr t0

  val mapping = Map(
    RegMap(0x0, jmpToDramInstr1, RegMap.Unwritable),
    RegMap(0x4, jmpToDramInstr2, RegMap.Unwritable),
    RegMap(0x8, jmpToDramInstr3, RegMap.Unwritable)
  )
  def getOffset(addr: UInt) = addr(12,0)

  val rdata = Wire(UInt(64.W))
  RegMap.generate(mapping, getOffset(raddr), rdata,
    getOffset(waddr), in.w.fire, in.w.bits.data, MaskExpand(in.w.bits.strb))

  in.r.bits.data := RegEnable(RegNext(Fill(2, rdata(31,0))), ren)
}
