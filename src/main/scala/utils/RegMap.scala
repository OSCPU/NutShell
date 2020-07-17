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

import top.Settings

object RegMap {
  def Unwritable = null
  def apply(addr: Int, reg: UInt, wfn: UInt => UInt = (x => x)) = (addr, (reg, wfn))
  def generate(mapping: Map[Int, (UInt, UInt => UInt)], raddr: UInt, rdata: UInt,
    waddr: UInt, wen: Bool, wdata: UInt, wmask: UInt):Unit = {
    val chiselMapping = mapping.map { case (a, (r, w)) => (a.U, r, w) }
    rdata := LookupTree(raddr, chiselMapping.map { case (a, r, w) => (a, r) })
    chiselMapping.map { case (a, r, w) =>
      if (w != null) when (wen && waddr === a) { r := w(MaskData(r, wdata, wmask)) }
    }
  }
  def generate(mapping: Map[Int, (UInt, UInt => UInt)], addr: UInt, rdata: UInt,
    wen: Bool, wdata: UInt, wmask: UInt):Unit = generate(mapping, addr, rdata, addr, wen, wdata, wmask)
}

object MaskedRegMap {
  def Unwritable = null
  def NoSideEffect: UInt => UInt = (x=>x)
  def WritableMask = Fill(if (Settings.get("IsRV32")) 32 else 64, true.B)
  def UnwritableMask = 0.U(if (Settings.get("IsRV32")) 32.W else 64.W)
  def apply(addr: Int, reg: UInt, wmask: UInt = WritableMask, wfn: UInt => UInt = (x => x), rmask: UInt = WritableMask) = (addr, (reg, wmask, wfn, rmask))
  def generate(mapping: Map[Int, (UInt, UInt, UInt => UInt, UInt)], raddr: UInt, rdata: UInt,
    waddr: UInt, wen: Bool, wdata: UInt):Unit = {
    val chiselMapping = mapping.map { case (a, (r, wm, w, rm)) => (a.U, r, wm, w, rm) }
    rdata := LookupTree(raddr, chiselMapping.map { case (a, r, wm, w, rm) => (a, r & rm) })
    chiselMapping.map { case (a, r, wm, w, rm) =>
      if (w != null && wm != UnwritableMask) when (wen && waddr === a) { r := w(MaskData(r, wdata, wm)) }
    }
  }
  def isIllegalAddr(mapping: Map[Int, (UInt, UInt, UInt => UInt, UInt)], addr: UInt):Bool = {
    val illegalAddr = Wire(Bool())
    val chiselMapping = mapping.map { case (a, (r, wm, w, rm)) => (a.U, r, wm, w, rm) }
    illegalAddr := LookupTreeDefault(addr, true.B, chiselMapping.map { case (a, r, wm, w, rm) => (a, false.B) })
    illegalAddr
  }
  def generate(mapping: Map[Int, (UInt, UInt, UInt => UInt, UInt)], addr: UInt, rdata: UInt,
    wen: Bool, wdata: UInt):Unit = generate(mapping, addr, rdata, addr, wen, wdata)
}
