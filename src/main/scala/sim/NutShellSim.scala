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

import system._
import nutcore.NutCoreConfig

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import bus.axi4._
import device.AXI4RAMSim
import nutcore._
import utils.GTimer
import top.Settings

class DiffTestIO extends Bundle {
  val r = Output(Vec(32, UInt(64.W)))
  val commit = Output(Bool())
  val isMultiCommit = Output(Bool())
  val thisPC = Output(UInt(64.W))
  val thisINST = Output(UInt(32.W))
  val isMMIO = Output(Bool())
  val isRVC = Output(Bool())
  val isRVC2 = Output(Bool())
  val intrNO = Output(UInt(64.W))
  
  val priviledgeMode = Output(UInt(2.W))
  val mstatus = Output(UInt(64.W))
  val sstatus = Output(UInt(64.W))
  val mepc = Output(UInt(64.W))
  val sepc = Output(UInt(64.W))
  val mcause = Output(UInt(64.W))
  val scause = Output(UInt(64.W))
}

class LogCtrlIO extends Bundle {
  val log_begin, log_end = Input(UInt(64.W))
  val log_level = Input(UInt(64.W)) // a cpp uint
}

/*
class ysyxSoCFull extends BlackBox with HasNutCoreParameter {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val cpu_reset = Output(Bool())
    val cpu_intr = Output(Bool())
    val cpu_master_0_awready = Output(Bool())
    val cpu_master_0_awvalid = Input(Bool())
    val cpu_master_0_awid = Input(UInt(4.W))
    val cpu_master_0_awaddr = Input(UInt(32.W))
    val cpu_master_0_awlen = Input(UInt(8.W))
    val cpu_master_0_awsize = Input(UInt(3.W))
    val cpu_master_0_awburst = Input(UInt(2.W))
    val cpu_master_0_wready = Output(Bool())
    val cpu_master_0_wvalid = Input(Bool())
    val cpu_master_0_wdata = Input(UInt(64.W))
    val cpu_master_0_wstrb = Input(UInt(8.W))
    val cpu_master_0_wlast = Input(Bool())
    val cpu_master_0_bready = Input(Bool())
    val cpu_master_0_bvalid = Output(Bool())
    val cpu_master_0_bid = Output(UInt(4.W))
    val cpu_master_0_bresp = Output(UInt(2.W))
    val cpu_master_0_arready = Output(Bool())
    val cpu_master_0_arvalid = Input(Bool())
    val cpu_master_0_arid = Input(UInt(4.W))
    val cpu_master_0_araddr = Input(UInt(32.W))
    val cpu_master_0_arlen = Input(UInt(8.W))
    val cpu_master_0_arsize = Input(UInt(3.W))
    val cpu_master_0_arburst = Input(UInt(2.W))
    val cpu_master_0_rready = Input(Bool())
    val cpu_master_0_rvalid = Output(Bool())
    val cpu_master_0_rid = Output(UInt(4.W))
    val cpu_master_0_rdata = Output(UInt(64.W))
    val cpu_master_0_rresp = Output(UInt(2.W))
    val cpu_master_0_rlast = Output(Bool())

    val cpu_slave_awready = Input(Bool())
    val cpu_slave_awvalid = Output(Bool())
    val cpu_slave_awid = Output(UInt(4.W))
    val cpu_slave_awaddr = Output(UInt(32.W))
    val cpu_slave_awlen = Output(UInt(8.W))
    val cpu_slave_awsize = Output(UInt(3.W))
    val cpu_slave_awburst = Output(UInt(2.W))
    val cpu_slave_wready = Input(Bool())
    val cpu_slave_wvalid = Output(Bool())
    val cpu_slave_wdata = Output(UInt(64.W))
    val cpu_slave_wstrb = Output(UInt(8.W))
    val cpu_slave_wlast = Output(Bool())
    val cpu_slave_bready = Output(Bool())
    val cpu_slave_bvalid = Input(Bool())
    val cpu_slave_bid = Input(UInt(4.W))
    val cpu_slave_bresp = Input(UInt(2.W))
    val cpu_slave_arready = Input(Bool())
    val cpu_slave_arvalid = Output(Bool())
    val cpu_slave_arid = Output(UInt(4.W))
    val cpu_slave_araddr = Output(UInt(32.W))
    val cpu_slave_arlen = Output(UInt(8.W))
    val cpu_slave_arsize = Output(UInt(3.W))
    val cpu_slave_arburst = Output(UInt(2.W))
    val cpu_slave_rready = Output(Bool())
    val cpu_slave_rvalid = Input(Bool())
    val cpu_slave_rid = Input(UInt(4.W))
    val cpu_slave_rdata = Input(UInt(64.W))
    val cpu_slave_rresp = Input(UInt(2.W))
    val cpu_slave_rlast = Input(Bool())

    val uart_rx = Input(Bool())
    val uart_tx = Output(Bool())
  })
}
*/

class NutShellSimTop extends Module {
  val io = IO(new Bundle{
    val difftest = new DiffTestIO
    val logCtrl = new LogCtrlIO
    val difftestCtrl = new DiffTestCtrlIO
  })

  lazy val config = NutCoreConfig(FPGAPlatform = false)
  val nutshell = Module(new NutShell()(config))

  if (Settings.get("SoCTest")) {

    nutshell.io := DontCare
    io.difftestCtrl.enable := false.B

    /*
    val ysyxSoC = Module(new ysyxSoCFull())
    ysyxSoC.io.clock := clock
    ysyxSoC.io.reset := reset
    nutshell.reset := ysyxSoC.io.cpu_reset
    nutshell.io.interrupt := ysyxSoC.io.cpu_intr

    ysyxSoC.io.cpu_master_0_awready <> nutshell.io.master.aw.ready
    ysyxSoC.io.cpu_master_0_awvalid <> nutshell.io.master.aw.valid
    ysyxSoC.io.cpu_master_0_awid <> nutshell.io.master.aw.bits.id
    ysyxSoC.io.cpu_master_0_awaddr <> nutshell.io.master.aw.bits.addr
    ysyxSoC.io.cpu_master_0_awlen <> nutshell.io.master.aw.bits.len
    ysyxSoC.io.cpu_master_0_awsize <> nutshell.io.master.aw.bits.size
    ysyxSoC.io.cpu_master_0_awburst <> nutshell.io.master.aw.bits.burst
    ysyxSoC.io.cpu_master_0_wready <> nutshell.io.master.w.ready
    ysyxSoC.io.cpu_master_0_wvalid <> nutshell.io.master.w.valid
    ysyxSoC.io.cpu_master_0_wdata <> nutshell.io.master.w.bits.data
    ysyxSoC.io.cpu_master_0_wstrb <> nutshell.io.master.w.bits.strb
    ysyxSoC.io.cpu_master_0_wlast <> nutshell.io.master.w.bits.last
    ysyxSoC.io.cpu_master_0_bready <> nutshell.io.master.b.ready
    ysyxSoC.io.cpu_master_0_bvalid <> nutshell.io.master.b.valid
    ysyxSoC.io.cpu_master_0_bid <> nutshell.io.master.b.bits.id
    ysyxSoC.io.cpu_master_0_bresp <> nutshell.io.master.b.bits.resp
    ysyxSoC.io.cpu_master_0_arready <> nutshell.io.master.ar.ready
    ysyxSoC.io.cpu_master_0_arvalid <> nutshell.io.master.ar.valid
    ysyxSoC.io.cpu_master_0_arid <> nutshell.io.master.ar.bits.id
    ysyxSoC.io.cpu_master_0_araddr <> nutshell.io.master.ar.bits.addr
    ysyxSoC.io.cpu_master_0_arlen <> nutshell.io.master.ar.bits.len
    ysyxSoC.io.cpu_master_0_arsize <> nutshell.io.master.ar.bits.size
    ysyxSoC.io.cpu_master_0_arburst <> nutshell.io.master.ar.bits.burst
    ysyxSoC.io.cpu_master_0_rready <> nutshell.io.master.r.ready
    ysyxSoC.io.cpu_master_0_rvalid <> nutshell.io.master.r.valid
    ysyxSoC.io.cpu_master_0_rid <> nutshell.io.master.r.bits.id
    ysyxSoC.io.cpu_master_0_rdata <> nutshell.io.master.r.bits.data
    ysyxSoC.io.cpu_master_0_rresp <> nutshell.io.master.r.bits.resp
    ysyxSoC.io.cpu_master_0_rlast <> nutshell.io.master.r.bits.last

    // TODO: leave cpu_slave hanging
    ysyxSoC.io.uart_rx := 1.U
    */

    dontTouch(nutshell.io)

  } else {
    val mem = Module(new AXI4RAMSim(memByte = 128 * 1024 * 1024, useBlackBox = true))
    // Be careful with the commit checking of emu.
    // A large delay will make emu incorrectly report getting stuck.
    val memdelay = Module(new AXI4Delayer(0))
    val mmio = Module(new SimMMIO)

    nutshell.io.slave <> mmio.io.dma
    
    memdelay.io.in <> nutshell.io.master
    mem.io.in <> memdelay.io.out
    
    mmio.io.rw <> nutshell.io.mmio
    nutshell.io.interrupt := mmio.io.meip
    io.difftestCtrl <> mmio.io.difftestCtrl
  }

  val difftest = WireInit(0.U.asTypeOf(new DiffTestIO))
  BoringUtils.addSink(difftest.commit, "difftestCommit")
  BoringUtils.addSink(difftest.isMultiCommit, "difftestMultiCommit")
  BoringUtils.addSink(difftest.thisPC, "difftestThisPC")
  BoringUtils.addSink(difftest.thisINST, "difftestThisINST")
  BoringUtils.addSink(difftest.isMMIO, "difftestIsMMIO")
  BoringUtils.addSink(difftest.isRVC, "difftestIsRVC")
  BoringUtils.addSink(difftest.isRVC2, "difftestIsRVC2")
  BoringUtils.addSink(difftest.intrNO, "difftestIntrNO")
  BoringUtils.addSink(difftest.r, "difftestRegs")
  BoringUtils.addSink(difftest.priviledgeMode, "difftestMode")
  BoringUtils.addSink(difftest.mstatus, "difftestMstatus")
  BoringUtils.addSink(difftest.sstatus, "difftestSstatus") 
  BoringUtils.addSink(difftest.mepc, "difftestMepc")
  BoringUtils.addSink(difftest.sepc, "difftestSepc")
  BoringUtils.addSink(difftest.mcause, "difftestMcause")
  BoringUtils.addSink(difftest.scause, "difftestScause")
  io.difftest := difftest

  val log_begin, log_end, log_level = WireInit(0.U(64.W))
  log_begin := io.logCtrl.log_begin
  log_end := io.logCtrl.log_end
  log_level := io.logCtrl.log_level

  assert(log_begin <= log_end)
  BoringUtils.addSource((GTimer() >= log_begin) && (GTimer() < log_end), "DISPLAY_ENABLE")

  // make BoringUtils not report boring exception when EnableDebug is set to false
  val dummyWire = WireInit(false.B)
  BoringUtils.addSink(dummyWire, "DISPLAY_ENABLE")

}
