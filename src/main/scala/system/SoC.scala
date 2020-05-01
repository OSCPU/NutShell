package system

import noop._
import bus.axi4.{AXI4, AXI4Lite}
import bus.simplebus._
import device.{AXI4Timer, AXI4PLIC}

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

trait HasSoCParameter {
  val EnableILA = true
  val HasL2cache = true
  val HasPrefetch = true
}

class ILABundle extends NOOPBundle {
  val WBUpc = UInt(VAddrBits.W)
  val WBUvalid = UInt(1.W)
  val WBUrfWen = UInt(1.W)
  val WBUrfDest = UInt(5.W)
  val WBUrfData = UInt(XLEN.W)
  val InstrCnt = UInt(64.W)
}

class NOOPSoC(implicit val p: NOOPConfig) extends Module with HasSoCParameter {
  val io = IO(new Bundle{
    val mem = new AXI4
    val mmio = (if (p.FPGAPlatform) { new AXI4Lite } else { new SimpleBusUC })
    val frontend = Flipped(new AXI4)
    val meip = Input(Bool())
    val ila = if (p.FPGAPlatform && EnableILA) Some(Output(new ILABundle)) else None
  })

  val noop = Module(new NOOP)
  val cohMg = Module(new CoherenceManager)
  val xbar = Module(new SimpleBusCrossbarNto1(2))
  cohMg.io.in <> noop.io.imem.mem
  noop.io.dmem.coh <> cohMg.io.out.coh
  xbar.io.in(0) <> cohMg.io.out.mem
  xbar.io.in(1) <> noop.io.dmem.mem

  val axi2sb = Module(new AXI42SimpleBusConverter())
  axi2sb.io.in <> io.frontend
  noop.io.frontend <> axi2sb.io.out

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
    l2cacheOut <> Cache(in = l2cacheIn, mmio = 0.U.asTypeOf(new SimpleBusUC) :: Nil, flush = "b00".U, empty = l2Empty, enable = true)(
      CacheConfig(name = "l2cache", totalSize = 128, cacheLevel = 2))
    l2cacheOut.coh.resp.ready := true.B
    l2cacheOut.coh.req.valid := false.B
    l2cacheOut.coh.req.bits := DontCare
    l2cacheOut.mem
  } else {
    xbar.io.out
  }

  val memAddrMap = Module(new SimpleBusAddressMapper((28, 0x10000000L)))
  memAddrMap.io.in <> mem
  io.mem <> memAddrMap.io.out.toAXI4()
  
  noop.io.imem.coh.resp.ready := true.B
  noop.io.imem.coh.req.valid := false.B
  noop.io.imem.coh.req.bits := DontCare

  val addrSpace = List(
    (0x40000000L, 0x01000000L), // external devices
    (0x48000000L, 0x00010000L), // CLINT
    (0x4c000000L, 0x04000000L)  // PLIC
  )
  val mmioXbar = Module(new SimpleBusCrossbar1toN(addrSpace))
  mmioXbar.io.in <> noop.io.mmio

  val extDev = mmioXbar.io.out(0)
  if (p.FPGAPlatform) {
    val mmioAddrMap = Module(new SimpleBusAddressMapper((24, 0xe0000000L)))
    mmioAddrMap.io.in <> extDev
    io.mmio <> mmioAddrMap.io.out.toAXI4Lite()
  }
  else io.mmio <> extDev

  val clint = Module(new AXI4Timer(sim = !p.FPGAPlatform))
  clint.io.in <> mmioXbar.io.out(1).toAXI4Lite()
  val mtipSync = clint.io.extra.get.mtip
  BoringUtils.addSource(mtipSync, "mtip")

  val plic = Module(new AXI4PLIC(nrIntr = 1, nrHart = 1))
  plic.io.in <> mmioXbar.io.out(2).toAXI4Lite()
  plic.io.extra.get.intrVec := RegNext(RegNext(io.meip))
  val meipSync = plic.io.extra.get.meip(0)
  BoringUtils.addSource(meipSync, "meip")

  // ILA
  if (p.FPGAPlatform) {
    def BoringUtilsConnect(sink: UInt, id: String) {
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
  }
}
