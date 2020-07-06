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
