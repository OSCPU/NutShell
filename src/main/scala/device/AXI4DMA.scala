package device

import chisel3._
import chisel3.util._

import bus.axi4._
import utils._

class DMABundle extends Bundle {
  val dma = new AXI4
}

class AXI4DMA extends AXI4SlaveModule(new AXI4Lite, new DMABundle) {
  val dest = Reg(UInt(32.W))
  val src = Reg(UInt(32.W))
  val len = RegInit(0.U(32.W))

  val dma = io.extra.get.dma
  val data = Reg(UInt(8.W))

  val s_idle :: s_read_req :: s_read_wait_resp :: s_write_req :: s_write_wait_resp :: Nil = Enum(5)
  val state = RegInit(s_idle)

  when (state === s_idle && len =/= 0.U) { state := s_read_req }
  when (state === s_read_req && dma.ar.fire()) { state := s_read_wait_resp }
  when (state === s_read_wait_resp && dma.r.fire()) {
    data := dma.r.bits.data.asTypeOf(Vec(8, UInt(8.W)))(src(2,0))
    state := s_write_req
  }

  val wSend = Wire(Bool())
  val wlast = dma.w.bits.last
  val awAck = BoolStopWatch(dma.aw.fire(), wSend)
  val wAck = BoolStopWatch(dma.w.fire() && wlast, wSend)
  wSend := (dma.aw.fire() && dma.w.fire() && wlast) || (awAck && wAck)

  when (state === s_write_req && wSend) { state := s_write_wait_resp }
  when (state === s_write_wait_resp && dma.b.fire()) {
    len := len - 1.U
    dest := dest + 1.U
    src := src + 1.U
    state := Mux(len === 1.U, s_idle, s_read_req)
  }

  dma.ar.bits.prot := AXI4Parameters.PROT_PRIVILEDGED
  dma.ar.bits.id := 0.U
  dma.ar.bits.size := "b00".U // 8 bit
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
  dma.w.bits.data := Fill(8, data)
  dma.w.bits.strb := "b1".U << dest(2, 0)
  dma.w.bits.last := true.B
  dma.b.ready := (state === s_write_wait_resp)

  val mapping = Map(
    RegMap(0x0, dest),
    RegMap(0x4, src),
    RegMap(0x8, len)
  )

  RegMap.generate(mapping, raddr(3,0), in.r.bits.data,
    waddr(3,0), in.w.fire(), in.w.bits.data, MaskExpand(in.w.bits.strb >> waddr(2,0)))
}
