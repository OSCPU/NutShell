package top

import chisel3._
import chisel3.util._

import bus.simplebus._
import device._

class SimMMIO extends Module {
  val io = IO(new Bundle {
    val rw = Flipped(new SimpleBusUC)
    val difftestCtrl = new DiffTestCtrlIO
  })

  val devAddrSpace = List(
    (0x40600000L, 0x10L), // uart
    (0x41000000L, 0x400000L), // vmem
    (0x40800000L, 0x8L),  // vga ctrl
    (0x40000000L, 0x1000L),  // flash
    (0x40002000L, 0x1000L), // dummy sdcard
    (0x42000000L, 0x1000L)  // DiffTestCtrl
  )

  val xbar = Module(new SimpleBusCrossbar1toN(devAddrSpace))
  xbar.io.in <> io.rw

  val uart = Module(new AXI4UART)
  val vga = Module(new AXI4VGA(sim = true))
  val flash = Module(new AXI4Flash)
  val sd = Module(new AXI4DummySD)
  val difftestCtrl = Module(new AXI4DiffTestCtrl)
  uart.io.in <> xbar.io.out(0).toAXI4Lite()
  vga.io.in.fb <> xbar.io.out(1).toAXI4Lite()
  vga.io.in.ctrl <> xbar.io.out(2).toAXI4Lite()
  flash.io.in <> xbar.io.out(3).toAXI4Lite()
  sd.io.in <> xbar.io.out(4).toAXI4Lite()
  difftestCtrl.io.in <> xbar.io.out(5).toAXI4Lite()
  io.difftestCtrl <> difftestCtrl.io.extra.get
  vga.io.vga := DontCare
}
