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

class DMABundle extends Bundle {
  val dma = new AXI4
}

class AXI4DMA extends AXI4SlaveModule(new AXI4Lite, new DMABundle) {
  val step = 4 // unit: byte. step = 8 is not supported now.
  val stepBits = step * 8
  val autoStart = false

  val dest = Reg(UInt(32.W))
  val src = Reg(UInt(32.W))
  val len = RegInit(0.U(32.W))

  val dma = io.extra.get.dma
  val data = Reg(UInt(stepBits.W))

  val s_idle :: s_read_req :: s_read_wait_resp :: s_write_req :: s_write_wait_resp :: Nil = Enum(5)
  val state = RegInit(s_idle)

  if (autoStart) {
    when (state === s_idle && len === 0.U) {
      len := 32768.U
      dest := "h62000000".U
      src  := "h62000000".U
      printf("\n@")
    }
  }
  when (state === s_idle && len =/= 0.U) { state := s_read_req }
  when (state === s_read_req && dma.ar.fire) { state := s_read_wait_resp }
  when (state === s_read_wait_resp && dma.r.fire) {
    data := dma.r.bits.data.asTypeOf(Vec(8 / step, UInt(stepBits.W)))(src(2, log2Ceil(step)))
    state := s_write_req
  }

  val wSend = Wire(Bool())
  val wlast = dma.w.bits.last
  val awAck = BoolStopWatch(dma.aw.fire, wSend)
  val wAck = BoolStopWatch(dma.w.fire && wlast, wSend)
  wSend := (dma.aw.fire && dma.w.fire && wlast) || (awAck && wAck)

  when (state === s_write_req && wSend) { state := s_write_wait_resp }
  when (state === s_write_wait_resp && dma.b.fire) {
    len := len - step.U
    dest := dest + step.U
    src := src + step.U
    state := Mux(len <= step.U, s_idle, s_read_req)
  }

  dma.ar.bits.prot := AXI4Parameters.PROT_PRIVILEDGED
  dma.ar.bits.id := 0.U
  dma.ar.bits.size := log2Ceil(step).U
  dma.ar.bits.burst := AXI4Parameters.BURST_INCR
  dma.ar.bits.lock := false.B
  dma.ar.bits.cache := 0.U
  dma.ar.bits.qos := 0.U
  dma.ar.bits.user := 0.U
  dma.ar.bits.len := 0.U
  dma.ar.bits.addr := src
  dma.ar.valid := (state === s_read_req)
  dma.r.ready := (state === s_read_wait_resp)

  dma.aw.bits := dma.ar.bits
  dma.aw.bits.addr := dest
  dma.aw.valid := (state === s_write_req) && !awAck
  dma.w.valid := (state === s_write_req) && !wAck
  dma.w.bits.data := Fill(8 / step, data)
  dma.w.bits.strb := Fill(step, "b1".U) << (dest(2,log2Ceil(step)) * step.U)
  dma.w.bits.last := true.B
  dma.b.ready := (state === s_write_wait_resp)

  val mapping = Map(
    RegMap(0x0, dest),
    RegMap(0x4, src),
    RegMap(0x8, len)
  )

  RegMap.generate(mapping, raddr(3,0), in.r.bits.data,
    waddr(3,0), in.w.fire, in.w.bits.data, MaskExpand(in.w.bits.strb >> waddr(2,0)))
}
