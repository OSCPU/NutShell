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

package system

import nutcore._
import bus.axi4.{AXI4, AXI4Lite, AXI4Parameters}
import bus.simplebus._
import device.{AXI4CLINT, AXI4PLIC}
import top.Settings
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import difftest.common.{DifftestMemReadIO, DifftestMemWriteIO, HasTopMemoryMasterPort}
import nutcore.FuType.PAddrBits
import utils.MaskExpand

trait HasSoCParameter {
  val EnableILA = Settings.get("EnableILA")
  val HasL2cache = Settings.get("HasL2cache")
  val HasPrefetch = Settings.get("HasPrefetch")
}

class ILABundle extends NutCoreBundle {
  val WBUpc = UInt(VAddrBits.W)
  val WBUvalid = UInt(1.W)
  val WBUrfWen = UInt(1.W)
  val WBUrfDest = UInt(5.W)
  val WBUrfData = UInt(XLEN.W)
  val InstrCnt = UInt(64.W)
}

class NutShell(implicit val p: NutCoreConfig) extends Module with HasSoCParameter with HasTopMemoryMasterPort {
  val io = IO(new Bundle{
    val mem = new AXI4
    val mmio = (if (p.FPGAPlatform) { new AXI4 } else { new SimpleBusUC })
    val frontend = Flipped(new AXI4)
    val meip = Input(UInt(Settings.getInt("NrExtIntr").W))
    val ila = if (p.FPGAPlatform && EnableILA) Some(Output(new ILABundle)) else None
  })
  dontTouch(io)

  val nutcore = Module(new NutCore)
  val cohMg = Module(new CoherenceManager)
  val xbar = Module(new SimpleBusCrossbarNto1(2))
  cohMg.io.in <> nutcore.io.imem.mem
  nutcore.io.dmem.coh <> cohMg.io.out.coh
  xbar.io.in(0) <> cohMg.io.out.mem
  xbar.io.in(1) <> nutcore.io.dmem.mem

  val axi2sb = Module(new AXI42SimpleBusConverter())
  axi2sb.io.in <> io.frontend
  nutcore.io.frontend <> axi2sb.io.out

  val memport = xbar.io.out.toMemPort
  memport.resp.bits.data := DontCare
  memport.resp.valid := DontCare
  memport.req.ready := DontCare

  val mem = if (HasL2cache) {
    val l2cacheOut = Wire(new SimpleBusC)
    val l2cacheIn = if (HasPrefetch) {
      val prefetcher = Module(new Prefetcher)
      val l2cacheIn = Wire(new SimpleBusUC)
      prefetcher.io.in <> xbar.io.out.req
      l2cacheIn.req <> prefetcher.io.out
      xbar.io.out.resp <> l2cacheIn.resp
      l2cacheIn
    } else xbar.io.out
    val l2Empty = Wire(Bool())
    val mmio = WireInit(0.U.asTypeOf(new SimpleBusUC))
    l2cacheOut <> Cache(in = l2cacheIn, mmio = mmio :: Nil, flush = "b00".U, empty = l2Empty, enable = true)(
      CacheConfig(name = "l2cache", totalSize = 128, cacheLevel = 2))
    l2cacheOut.coh.resp.ready := true.B
    l2cacheOut.coh.req.valid := false.B
    l2cacheOut.coh.req.bits := DontCare
    l2cacheOut.mem
  } else {
    xbar.io.out
  }

  val memMapRegionBits = Settings.getInt("MemMapRegionBits")
  val memMapBase = Settings.getLong("MemMapBase")
  val memAddrMap = Module(new SimpleBusAddressMapper((memMapRegionBits, memMapBase)))
  memAddrMap.io.in <> mem
  io.mem <> memAddrMap.io.out.toAXI4(true)
  
  nutcore.io.imem.coh.resp.ready := true.B
  nutcore.io.imem.coh.req.valid := false.B
  nutcore.io.imem.coh.req.bits := DontCare

  val addrSpace = List(
    (0x38000000L, 0x00010000L), // CLINT
    (0x3c000000L, 0x04000000L), // PLIC
    (Settings.getLong("MMIOBase"), Settings.getLong("MMIOSize")), // external devices
  )
  val mmioXbar = Module(new SimpleBusCrossbar1toN(addrSpace))
  mmioXbar.io.in <> nutcore.io.mmio

  val extDev = mmioXbar.io.out(2)
  if (p.FPGAPlatform) { io.mmio <> extDev.toAXI4() }
  else { io.mmio <> extDev }

  val clint = Module(new AXI4CLINT(sim = !p.FPGAPlatform))
  clint.io.in <> mmioXbar.io.out(0).toAXI4Lite
  val mtipSync = clint.io.extra.get.mtip
  val msipSync = clint.io.extra.get.msip
  BoringUtils.addSource(mtipSync, "mtip")
  BoringUtils.addSource(msipSync, "msip")

  val plic = Module(new AXI4PLIC(nrIntr = Settings.getInt("NrExtIntr"), nrHart = 1))
  plic.io.in <> mmioXbar.io.out(1).toAXI4Lite
  plic.io.extra.get.intrVec := RegNext(RegNext(RegNext(RegNext(io.meip))))
  val meipSync = plic.io.extra.get.meip(0)
  BoringUtils.addSource(meipSync, "meip")
  

  // ILA
  if (p.FPGAPlatform && !p.FPGADifftest) {
    def BoringUtilsConnect(sink: UInt, id: String) = {
      val temp = WireInit(0.U(64.W))
      BoringUtils.addSink(temp, id)
      sink := temp
    }

    val dummy = WireInit(0.U.asTypeOf(new ILABundle))
    val ila = io.ila.getOrElse(dummy)
    BoringUtilsConnect(ila.WBUpc      ,"ilaWBUpc")
    BoringUtilsConnect(ila.WBUvalid   ,"ilaWBUvalid")
    BoringUtilsConnect(ila.WBUrfWen   ,"ilaWBUrfWen")
    BoringUtilsConnect(ila.WBUrfDest  ,"ilaWBUrfDest")
    BoringUtilsConnect(ila.WBUrfData  ,"ilaWBUrfData")
    BoringUtilsConnect(ila.InstrCnt   ,"ilaInstrCnt")
    ila := 0.U.asTypeOf(ila)
  }

  override def getTopMemoryMasterRead: DifftestMemReadIO = {
    val req_valid = RegInit(false.B)
    val req_bits = Reg(chiselTypeOf(io.mem.ar.bits))
    val req_beat_count = Reg(UInt(8.W))
    assert(!io.mem.r.fire || (req_valid && io.mem.r.bits.id === req_bits.id), "data fire but req mismatch")
    when (io.mem.ar.fire) {
      req_valid := true.B
      req_bits := io.mem.ar.bits
      // wrap here for burst
      val wrap_mask = ~(io.mem.ar.bits.len << io.mem.ar.bits.size).asTypeOf(UInt(PAddrBits.W))
      req_bits.addr := io.mem.ar.bits.addr & wrap_mask.asUInt
      req_beat_count := (io.mem.ar.bits.addr >> io.mem.ar.bits.size).asUInt & io.mem.ar.bits.len
      assert(!req_valid || io.mem.r.fire && io.mem.r.bits.last, "multiple inflight not supported")
    }.elsewhen(io.mem.r.fire) {
      val should_wrap = req_bits.burst === AXI4Parameters.BURST_WRAP && req_beat_count === req_bits.len
      req_beat_count := Mux(should_wrap, 0.U, req_beat_count + 1.U)
      when (io.mem.r.bits.last) {
        req_valid := false.B
      }
    }
    val read = Wire(Output(new DifftestMemReadIO(io.mem.dataBits / 64)))
    read.valid := io.mem.r.fire
    read.index := (req_bits.addr >> log2Ceil(io.mem.dataBits / 8 - 1)) + req_beat_count
    read.data := io.mem.r.bits.data.asTypeOf(read.data)
    read
  }

  override def getTopMemoryMasterWrite: DifftestMemWriteIO = {
    val req_bits_reg = Reg(chiselTypeOf(io.mem.aw.bits))
    val req_bits = Mux(io.mem.aw.fire, io.mem.aw.bits, req_bits_reg)
    when (io.mem.aw.fire) {
      req_bits_reg := io.mem.aw.bits
      when (io.mem.w.fire) {
        req_bits_reg.addr := io.mem.aw.bits.addr + (io.mem.dataBits / 8).U
      }
    }.elsewhen(io.mem.w.fire) {
      req_bits_reg.addr := req_bits.addr + (io.mem.dataBits / 8).U
    }
    val write = Wire(Output(new DifftestMemWriteIO(io.mem.dataBits / 64)))
    write.valid := io.mem.w.fire
    write.index := req_bits.addr >> log2Ceil(io.mem.dataBits / 8 - 1)
    write.data := io.mem.w.bits.data.asTypeOf(write.data)
    write.mask := MaskExpand(io.mem.w.bits.strb).asTypeOf(write.mask)
    write
  }
}
