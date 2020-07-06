package sim

import chisel3._
import chisel3.util._

import device._
import bus.axi4._
import utils._

class MeipGenIO extends Bundle {
  val meip = Output(Bool())
}

class AXI4MeipGen extends AXI4SlaveModule(new AXI4Lite, new MeipGenIO) {
  val meip = RegInit(false.B)

  val mapping = Map(
    RegMap(0x0, meip)
  )

  def getOffset(addr: UInt) = addr(3, 0)
  RegMap.generate(mapping, getOffset(raddr), in.r.bits.data,
    getOffset(waddr), in.w.fire(), in.w.bits.data, MaskExpand(in.w.bits.strb))

  io.extra.get.meip := meip
}
