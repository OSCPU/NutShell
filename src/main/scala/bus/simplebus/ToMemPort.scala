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

package bus.simplebus

import chisel3._
import chisel3.util._

import bus.memport._
import utils._
import bus.memport.MemoryOpConstants

object MemPortConsts extends MemoryOpConstants{}

class SimpleBus2MemPortConverter(outType: MemPortIo) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new SimpleBusUC)
    val out = Flipped(Flipped(outType))
  })

  io.in.req.ready := io.out.req.ready
  io.in.resp.valid := io.out.resp.valid
  io.out.req.valid := io.in.req.valid
  io.out.resp.ready := io.in.resp.ready

  io.out.req.bits.addr := io.in.req.bits.addr
  io.out.req.bits.data := io.in.req.bits.wdata
  io.out.req.bits.fcn := Mux(io.in.req.bits.isRead(), MemPortConsts.M_XRD, MemPortConsts.M_XWR)
  io.out.req.bits.typ := MemPortConsts.MT_W

  io.in.resp.bits.rdata := io.out.resp.bits.data
  io.in.resp.bits.cmd := SimpleBusCmd.readLast
}

object SimpleBus2MemPortConverter {
  def apply(in: SimpleBusUC, outType: MemPortIo): MemPortIo = {
    val bridge = Module(new SimpleBus2MemPortConverter(outType))
    bridge.io.in <> in
    bridge.io.out
  }
}