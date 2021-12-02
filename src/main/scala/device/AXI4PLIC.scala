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

class PlicIO(val nrIntr: Int, val nrHart: Int) extends Bundle {
  val intrVec = Input(UInt(nrIntr.W))
  val meip = Output(Vec(nrHart, Bool()))
}

class AXI4PLIC(nrIntr: Int, nrHart: Int) extends AXI4SlaveModule(new AXI4Lite, new PlicIO(nrIntr, nrHart)) {
  require(nrIntr < 1024)
  require(nrHart <= 15872)
  val addressSpaceSize = 0x4000000
  val addressBits = log2Up(addressSpaceSize)
  def getOffset(addr: UInt) = addr(addressBits-1,0)

  val priority = List.fill(nrIntr)(Reg(UInt(32.W)))
  val priorityMap = priority.zipWithIndex.map{ case (r, intr) => RegMap((intr + 1) * 4, r) }.toMap

  val nrIntrWord = (nrIntr + 31) / 32  // roundup
  // pending bits are updated in the unit of bit by PLIC,
  // so define it as vectors of bits, instead of UInt(32.W)
  val pending = List.fill(nrIntrWord)(RegInit(0.U.asTypeOf(Vec(32, Bool()))))
  val pendingMap = pending.zipWithIndex.map { case (r, intrWord) =>
    RegMap(0x1000 + intrWord * 4, Cat(r.reverse), RegMap.Unwritable)
  }.toMap

  val enable = List.fill(nrHart)( List.fill(nrIntrWord)(RegInit(0.U(32.W))) )
  val enableMap = enable.zipWithIndex.map { case (l, hart) =>
    l.zipWithIndex.map { case (r, intrWord) => RegMap(0x2000 + hart * 0x80 + intrWord * 4, r) }
  }.reduce(_ ++ _).toMap

  val threshold = List.fill(nrHart)(Reg(UInt(32.W)))
  val thresholdMap = threshold.zipWithIndex.map {
    case (r, hart) => RegMap(0x200000 + hart * 0x1000, r)
  }.toMap

  val inHandle = RegInit(0.U.asTypeOf(Vec(nrIntr + 1, Bool())))
  def completionFn(wdata: UInt) = {
    inHandle(wdata(31,0)) := false.B
    0.U
  }

  val claimCompletion = List.fill(nrHart)(Reg(UInt(32.W)))
  val claimCompletionMap = claimCompletion.zipWithIndex.map {
    case (r, hart) => {
      val addr = 0x200004 + hart * 0x1000
      when (in.r.fire && (getOffset(raddr) === addr.U)) { inHandle(r) := true.B }
      RegMap(addr, r, completionFn)
    }
  }.toMap

  io.extra.get.intrVec.asBools.zipWithIndex.map { case (intr, i) => {
    val id = i + 1
    when (intr) { pending(id / 32)(id % 32) := true.B }
    when (inHandle(id)) { pending(id / 32)(id % 32) := false.B }
  } }

  val pendingVec = Cat(pending.map(x => Cat(x.reverse)))
  claimCompletion.zipWithIndex.map { case (r, hart) => {
    val takenVec = pendingVec & Cat(enable(hart))
    r := Mux(takenVec === 0.U, 0.U, PriorityEncoder(takenVec))
  } }

  val mapping = priorityMap ++ pendingMap ++ enableMap ++ thresholdMap ++ claimCompletionMap

  val rdata = Wire(UInt(32.W))
  RegMap.generate(mapping, getOffset(raddr), rdata,
    getOffset(waddr), in.w.fire, in.w.bits.data, MaskExpand(in.w.bits.strb >> waddr(2,0)))
  // narrow read
  in.r.bits.data := Fill(2, rdata)

  io.extra.get.meip.zipWithIndex.map { case (ip, hart) => ip := claimCompletion(hart) =/= 0.U }
}
