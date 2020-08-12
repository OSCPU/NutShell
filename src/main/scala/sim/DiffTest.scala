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

package sim

import chisel3._
import chisel3.util._

import device._
import bus.axi4._
import utils._

class DiffTestCtrlIO extends Bundle {
  val enable = Output(Bool())
}

class AXI4DiffTestCtrl extends AXI4SlaveModule(new AXI4Lite, new DiffTestCtrlIO) {
  val enable = RegInit(true.B)

  val mapping = Map(
    RegMap(0x0, enable)
  )

  def getOffset(addr: UInt) = addr(3, 0)
  RegMap.generate(mapping, getOffset(raddr), in.r.bits.data,
    getOffset(waddr), in.w.fire(), in.w.bits.data, MaskExpand(in.w.bits.strb))

  io.extra.get.enable := enable
}
