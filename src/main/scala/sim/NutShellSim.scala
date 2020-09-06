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
import device.AXI4RAM
import nutcore._
import utils.GTimer

class DiffTestIO extends NutCoreBundle {
  val r = Output(Vec(32, UInt(XLEN.W)))                  // Regfile
  val commit = Output(UInt(32.W))                        // number of insts committed in current cycle
  val thisPC = Output(UInt(XLEN.W))                      // PC for the 1st inst committed in this cycle
  val thisINST = Output(UInt(32.W))                      // Raw instruction for the 1st inst committed in this cycle
  val skip = Output(UInt(32.W))                          // If an inst should be skipped in difftest (Vector)
  val wen = Output(UInt(32.W))                           // Regfile.wen (Vector)
  val wdata = Output(Vec(DifftestWidth, UInt(XLEN.W)))   // Regfile.wdata (Vector)
  val wdst = Output(Vec(DifftestWidth, UInt(32.W)))      // Regfile.wdst (Vector)
  val wpc = Output(Vec(DifftestWidth, UInt(XLEN.W)))     // PC for insts committed in this cycle (Vector)
  val isRVC = Output(Bool())                             // Insts committed in this cycle is RVC (Vector)
  val intrNO = Output(UInt(64.W))                        // Int/exc number

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

class NutShellSimTop extends Module {
  val io = IO(new Bundle{
    val difftest = new DiffTestIO
    val logCtrl = new LogCtrlIO
    val difftestCtrl = new DiffTestCtrlIO
  })

  lazy val config = NutCoreConfig(FPGAPlatform = false)
  val soc = Module(new NutShell()(config))
  val mem = Module(new AXI4RAM(memByte = 128 * 1024 * 1024, useBlackBox = true))
  // Be careful with the commit checking of emu.
  // A large delay will make emu incorrectly report getting stuck.
  val memdelay = Module(new AXI4Delayer(0))
  val mmio = Module(new SimMMIO)

  soc.io.frontend <> mmio.io.dma

  memdelay.io.in <> soc.io.mem
  mem.io.in <> memdelay.io.out

  mmio.io.rw <> soc.io.mmio

  soc.io.meip := mmio.io.meip

  val difftest = WireInit(0.U.asTypeOf(new DiffTestIO))
  BoringUtils.addSink(difftest.r, "DIFFTEST_r")
  BoringUtils.addSink(difftest.commit, "DIFFTEST_commit")
  BoringUtils.addSink(difftest.thisPC, "DIFFTEST_thisPC")
  BoringUtils.addSink(difftest.thisINST, "DIFFTEST_thisINST")
  BoringUtils.addSink(difftest.skip, "DIFFTEST_skip")
  BoringUtils.addSink(difftest.wen, "DIFFTEST_wen")
  BoringUtils.addSink(difftest.wdata, "DIFFTEST_wdata")
  BoringUtils.addSink(difftest.wdst, "DIFFTEST_wdst")
  BoringUtils.addSink(difftest.wpc, "DIFFTEST_wpc")
  BoringUtils.addSink(difftest.isRVC, "DIFFTEST_isRVC")
  BoringUtils.addSink(difftest.intrNO, "DIFFTEST_intrNO")

  BoringUtils.addSink(difftest.priviledgeMode, "DIFFTEST_priviledgeMode")
  BoringUtils.addSink(difftest.mstatus, "DIFFTEST_mstatus")
  BoringUtils.addSink(difftest.sstatus, "DIFFTEST_sstatus") 
  BoringUtils.addSink(difftest.mepc, "DIFFTEST_mepc")
  BoringUtils.addSink(difftest.sepc, "DIFFTEST_sepc")
  BoringUtils.addSink(difftest.mcause, "DIFFTEST_mcause")
  BoringUtils.addSink(difftest.scause, "DIFFTEST_scause")
  io.difftest := difftest

  val log_begin, log_end, log_level = WireInit(0.U(64.W))
  log_begin := io.logCtrl.log_begin
  log_end := io.logCtrl.log_end
  log_level := io.logCtrl.log_level

  assert(log_begin <= log_end)
  BoringUtils.addSource((GTimer() >= log_begin) && (GTimer() < log_end), "DISPLAY_ENABLE")
  BoringUtils.addSource(WireInit(GTimer()), "LOG_TIMESTAMP") // use bare GTimer() in addSource will cause type error

  // make BoringUtils not report boring exception
  val dummyWire1 = WireInit(false.B)
  val dummyWire2 = WireInit(0.U(64.W))
  BoringUtils.addSink(dummyWire1, "DISPLAY_ENABLE")
  BoringUtils.addSink(dummyWire2, "LOG_TIMESTAMP")

  io.difftestCtrl <> mmio.io.difftestCtrl
}
