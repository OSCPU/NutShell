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

import bus.simplebus._
import bus.axi4._
import device._
import difftest._

class SimMMIO extends Module {
  val io = IO(new Bundle {
    val rw = Flipped(new SimpleBusUC)
    val meip = Output(Bool())
    val dma = new AXI4
    val uart = new UARTIO
  })

  val devAddrSpace = List(
    (0x40600000L, 0x10L), // uart
    (0x50000000L, 0x400000L), // vmem
    (0x40001000L, 0x8L),  // vga ctrl
    (0x40000000L, 0x1000L),  // flash
    (0x40002000L, 0x1000L), // dummy sdcard
    (0x40004000L, 0x1000L), // meipGen
    (0x40003000L, 0x1000L)  // dma
  )

  val xbar = Module(new SimpleBusCrossbar1toN(devAddrSpace))
  xbar.io.in <> io.rw

  val uart = Module(new AXI4UART)
  val vga = Module(new AXI4VGA(sim = true))
  val flash = Module(new AXI4Flash)
  val sd = Module(new AXI4DummySD)
  val meipGen = Module(new AXI4MeipGen)
  val dma = Module(new AXI4DMA)
  uart.io.in <> xbar.io.out(0).toAXI4Lite()
  vga.io.in.fb <> xbar.io.out(1).toAXI4Lite()
  vga.io.in.ctrl <> xbar.io.out(2).toAXI4Lite()
  flash.io.in <> xbar.io.out(3).toAXI4Lite()
  sd.io.in <> xbar.io.out(4).toAXI4Lite()
  meipGen.io.in <> xbar.io.out(5).toAXI4Lite()
  dma.io.in <> xbar.io.out(6).toAXI4Lite()
  io.dma <> dma.io.extra.get.dma
  io.meip := meipGen.io.extra.get.meip
  uart.io.extra.get <> io.uart
  vga.io.vga := DontCare
}
