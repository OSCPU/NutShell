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
import chisel3.util.experimental.BoringUtils

import bus.axi4._
import utils._
import top.Settings

class AssertionIO extends Bundle {
  val req  = Output(Bool())
  val data = Input(UInt(32.W))
  val ack  = Input(Bool())
  val addr = Output(UInt(32.W))
}

class AXI4CNT(sim: Boolean = false) extends AXI4SlaveModule(new AXI4Lite, new AssertionIO) {
  def getOffset(addr: UInt) = addr(11,0)
  val assertIO = io.extra.get
  val regBuffer = RegInit(0.U(32.W))
  when(assertIO.ack){
    regBuffer := assertIO.data
  }

  val mapping = Map(
    RegMap(0x0, regBuffer)
  )
  ren := io.in.ar.fire()
  RegMap.generate(mapping, getOffset(raddr), in.r.bits.data,
    getOffset(waddr), in.w.fire(), in.w.bits.data,0.U)
  assertIO.addr := raddr
  assertIO.req := ren
}
object AXI4CNTTop extends App {
  Settings.settings += "IsRV32" -> true // ugly solution by modify the NutShell Setting
  Driver.execute(args, ()=> new AXI4CNT)
}
