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

package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import bus.simplebus._
import bus.axi4._
import utils._
import top.Settings

trait HasNutCoreParameter {
  // General Parameter for NutShell
  val XLEN = if (Settings.get("IsRV32")) 32 else 64
  val HasMExtension = true
  val HasCExtension = Settings.get("EnableRVC")
  val HasNExtension = Settings.get("EnableRVN")
  val HasDiv = true
  val HasIcache = Settings.get("HasIcache")
  val HasDcache = Settings.get("HasDcache")
  val HasITLB = Settings.get("HasITLB")
  val HasDTLB = Settings.get("HasDTLB")
  val AddrBits = 64 // AddrBits is used in some cases
  val VAddrBits = if (Settings.get("IsRV32")) 32 else 39 // VAddrBits is Virtual Memory addr bits
  val PAddrBits = 32 // PAddrBits is Phyical Memory addr bits
  val AddrBytes = AddrBits / 8 // unused
  val DataBits = XLEN
  val DataBytes = DataBits / 8
  val EnableVirtualMemory = if (Settings.get("HasDTLB") && Settings.get("HasITLB")) true else false
  val EnablePerfCnt = true
  val ExceptionTypes = 22
  val InterruptTypes = 12
  // Parameter for Argo's OoO backend
  val EnableMultiIssue = Settings.get("EnableMultiIssue")
  val EnableOutOfOrderExec = Settings.get("EnableOutOfOrderExec")
  val EnableMultiCyclePredictor = false // false unless a customized condition branch predictor is included
  val EnableOutOfOrderMemAccess = false // enable out of order mem access will improve OoO backend's performance
}

trait HasNutCoreConst extends HasNutCoreParameter {
  val CacheReadWidth = 8
  val ICacheUserBundleWidth = VAddrBits*2 + 9
  val DCacheUserBundleWidth = 16
  val IndependentBru = if (Settings.get("EnableOutOfOrderExec")) true else false
}

trait HasNutCoreLog { this: RawModule =>
  implicit val moduleName: String = this.name
}

trait HasDasicsConst {
  val dasicsLibGroups = 8

  def SCFG_GLB = 0x2
  def SCFG_ENA = 0x1
  def SCFG_CLS = 0x0

  def UCFG_ENA = 0x1
  def UCFG_CLS = 0x0

  def LIBCFG_V = 0x3
  def LIBCFG_X = 0x2
  def LIBCFG_R = 0x1
  def LIBCFG_W = 0x0
}

abstract class NutCoreModule extends Module with HasNutCoreParameter with HasNutCoreConst with HasExceptionNO with HasBackendConst with HasNutCoreLog with HasDasicsConst
abstract class NutCoreBundle extends Bundle with HasNutCoreParameter with HasNutCoreConst with HasBackendConst

case class NutCoreConfig (
  FPGAPlatform: Boolean = true,
  EnableDebug: Boolean = Settings.get("EnableDebug"),
  EnhancedLog: Boolean = true 
)
// Enable EnhancedLog will slow down simulation, 
// but make it possible to control debug log using emu parameter

object AddressSpace extends HasNutCoreParameter {
  // (start, size)
  // address out of MMIO will be considered as DRAM
  def mmio = if (Settings.get("SoCTest")) {
    List(
    // internal devices, such as CLINT and PLIC
    (0x00000000L, 0x10000000L),
    // external devices
    (Settings.getLong("MMIOBase"), Settings.getLong("MMIOSize"))
  )} else {
  List(
    // internal devices, such as CLINT and PLIC
    (0x30000000L, 0x10000000L),
    // external devices
    (Settings.getLong("MMIOBase"), Settings.getLong("MMIOSize"))
  )}

  def isMMIO(addr: UInt) = mmio.map(range => {
    // require(isPow2(range._2))
    // val bits = log2Up(range._2)
    // (addr ^ range._1.U)(PAddrBits-1, bits) === 0.U
    addr >= range._1.U && addr < (range._1 + range._2).U
  }).reduce(_ || _)
}

class MMIOBridge(implicit val p: NutCoreConfig) extends NutCoreModule {
  val io = IO(new Bundle {
    val in = Flipped(new SimpleBusUC)
    val out = new SimpleBusUC
  })
  val s_idle :: s_req1 :: s_waitResp1 :: s_req2 :: s_waitResp2 :: s_done :: s_req :: s_normal :: Nil = Enum(8)
  val state = RegInit(0.U(8.W))
  val in_reqLatch = RegEnable(io.in.req.bits, 0.U.asTypeOf(io.in.req.bits), io.in.req.fire())
  val out_respLatch1 = HoldUnless(io.out.resp.bits, io.out.resp.fire() && state === s_waitResp1)
  val out_respLatch2 = HoldUnless(io.out.resp.bits, io.out.resp.fire() && state === s_waitResp2)
  val normal = RegInit(false.B)
  val out_normalLatch = HoldUnless(io.out.resp.bits, io.out.resp.fire() && normal)

  val in_sel = !(io.in.req.bits.size === "b11".U)

  switch (state) {
    is (s_idle) {
      when (io.in.req.fire()) {
        when (io.in.req.bits.size === "b11".U) {
          state := s_req1
          normal := false.B
        }.otherwise{
          assert(false.B)
          normal := true.B
        }
      }
    }
    is (s_req1) {
      when (io.out.req.fire()) {
        // printf("out addr: %x\n", io.out.req.bits.addr)
        state := s_waitResp1
      }
    }
    is (s_waitResp1) {
      when (io.out.resp.fire()) {
        // printf("out resp1: %x\n", io.out.resp.bits.rdata)
        state := s_req2
      }
    }
    is (s_req2) {
      when (io.out.req.fire()) {
        state := s_waitResp2
      }
    }
    is (s_waitResp2) {
      when (io.out.resp.fire()) {
        state := s_done
      }
    }
    is (s_done) {
      when (io.in.resp.fire()) {
        state := s_idle
        // printf("acquire addr: %x\n", in_reqLatch.addr)
        // printf("resp data: %x\n", io.in.resp.bits.rdata)
      }
    }
    is (s_req) {
      when {io.out.req.fire()} {
        state := s_normal
      }
    }
    is (s_normal) {
      when {io.out.resp.fire()} {
        state := s_done
      }
    }
  }

  io.in.req.ready := Mux(in_sel, io.out.req.ready, state === s_idle)
  io.in.resp.valid := Mux(normal, io.out.resp.valid, state === s_done)
  io.in.resp.bits.cmd := Mux(normal, io.out.resp.bits.cmd, out_respLatch2.cmd)
  io.in.resp.bits.rdata := Mux(normal, io.out.resp.bits.rdata, Cat(out_respLatch2.rdata(31,0), out_respLatch1.rdata(31,0)))

  io.out.req.valid := (state === s_req1) || (state === s_req2) || ((state === s_idle) && io.in.req.valid && in_sel)
  //  io.out.req.bits <> in_reqLatch
  io.out.req.bits.addr :=  Mux(in_sel, io.in.req.bits.addr, Mux(state === s_req1, Cat(in_reqLatch.addr(31,3),0.U(3.W)), Cat(in_reqLatch.addr(31,3),0.U(3.W)) + 4.U))
  io.out.req.bits.size := Mux(in_sel, io.in.req.bits.size, "b10".U)
  io.out.req.bits.cmd := io.in.req.bits.cmd
  io.out.req.bits.wdata := Mux(in_sel, io.in.req.bits.wdata, Mux(state === s_req1, Cat(Seq.fill(2)(in_reqLatch.wdata(31,0))), Cat(Seq.fill(2)(in_reqLatch.wdata(63,32)))))
  io.out.req.bits.wmask := Mux(in_sel, io.in.req.bits.wmask, Mux(state === s_req1, in_reqLatch.wmask(3,0), in_reqLatch.wmask(7,4)))
  io.out.resp.ready := Mux(normal, io.in.resp.ready, true.B)
  //  assert(io.in.resp.bits.rdata === out_normalLatch.rdata)
}

class NutCore(implicit val p: NutCoreConfig) extends NutCoreModule {
  val io = IO(new Bundle {
    val imem = new SimpleBusC
    val dmem = new SimpleBusC
    val mmio = new SimpleBusUC
    val frontend = Flipped(new SimpleBusUC())
  })

  // Frontend
  val frontend = (Settings.get("IsRV32"), Settings.get("EnableOutOfOrderExec")) match {
    case (true, _)      => Module(new Frontend_embedded)
    case (false, true)  => Module(new Frontend_ooo)
    case (false, false) => Module(new Frontend_inorder)
  }
  
  // Backend
  if (EnableOutOfOrderExec) {
    val mmioXbar = Module(new SimpleBusCrossbarNto1(if (HasDcache) 2 else 3))
    val backend = Module(new Backend_ooo)
    PipelineVector2Connect(new DecodeIO, frontend.io.out(0), frontend.io.out(1), backend.io.in(0), backend.io.in(1), frontend.io.flushVec(1), 16)
    backend.io.flush := frontend.io.flushVec(2)
    frontend.io.redirect <> backend.io.redirect

    val dmemXbar = Module(new SimpleBusAutoIDCrossbarNto1(4, userBits = if (HasDcache) DCacheUserBundleWidth else 0))

    val itlb = TLB(in = frontend.io.imem, mem = dmemXbar.io.in(2), flush = frontend.io.flushVec(0) | frontend.io.bpFlush, csrMMU = backend.io.memMMU.imem)(TLBConfig(name = "itlb", userBits = ICacheUserBundleWidth, totalEntry = 4))
    frontend.io.ipf := itlb.io.ipf
    io.imem <> Cache(in = itlb.io.out, mmio = mmioXbar.io.in.take(1), flush = Fill(2, frontend.io.flushVec(0) | frontend.io.bpFlush), empty = itlb.io.cacheEmpty)(
      CacheConfig(ro = true, name = "icache", userBits = ICacheUserBundleWidth))
    
    val dtlb = TLB(in = backend.io.dtlb, mem = dmemXbar.io.in(1), flush = frontend.io.flushVec(3), csrMMU = backend.io.memMMU.dmem)(TLBConfig(name = "dtlb", userBits = DCacheUserBundleWidth, totalEntry = 64))
    dtlb.io.out := DontCare //FIXIT
    dtlb.io.out.req.ready := true.B //FIXIT

    if (EnableVirtualMemory) {
      dmemXbar.io.in(3) <> backend.io.dmem
      io.dmem <> Cache(in = dmemXbar.io.out, mmio = mmioXbar.io.in.drop(1), flush = "b00".U, empty = dtlb.io.cacheEmpty, enable = HasDcache)(
        CacheConfig(ro = false, name = "dcache", userBits = DCacheUserBundleWidth, idBits = 4))
    } else {
      dmemXbar.io.in(1) := DontCare
      dmemXbar.io.in(3) := DontCare
      dmemXbar.io.out := DontCare
      io.dmem <> Cache(in = backend.io.dmem, mmio = mmioXbar.io.in.drop(1), flush = "b00".U, empty = dtlb.io.cacheEmpty, enable = HasDcache)(
        CacheConfig(ro = false, name = "dcache", userBits = DCacheUserBundleWidth))
    }

    // Make DMA access through L1 DCache to keep coherence
    val expender = Module(new SimpleBusUCExpender(userBits = DCacheUserBundleWidth, userVal = 0.U))
    expender.io.in <> io.frontend
    dmemXbar.io.in(0) <> expender.io.out

    io.mmio <> mmioXbar.io.out

  } else {
    val backend = Module(new Backend_inorder)

    PipelineVector2Connect(new DecodeIO, frontend.io.out(0), frontend.io.out(1), backend.io.in(0), backend.io.in(1), frontend.io.flushVec(1), 4)

    val mmioXbar = Module(new SimpleBusCrossbarNto1(2))
    val dmemXbar = Module(new SimpleBusCrossbarNto1(4))

    val mmioBridge = Module(new MMIOBridge())
    val immioBus = WireInit(0.U.asTypeOf(new SimpleBusUC))

    val itlb = EmbeddedTLB(in = frontend.io.imem, mem = dmemXbar.io.in(1), flush = frontend.io.flushVec(0) | frontend.io.bpFlush, csrMMU = backend.io.memMMU.imem, enable = HasITLB)(TLBConfig(name = "itlb", userBits = ICacheUserBundleWidth, totalEntry = 4))
    frontend.io.ipf := itlb.io.ipf
    io.imem <> Cache(in = itlb.io.out, mmio = Seq(immioBus), flush = Fill(2, frontend.io.flushVec(0) | frontend.io.bpFlush), empty = itlb.io.cacheEmpty, enable = HasIcache)(CacheConfig(ro = true, name = "icache", userBits = ICacheUserBundleWidth))

    immioBus <> mmioBridge.io.in
    mmioXbar.io.in(0) <> mmioBridge.io.out

    // dtlb
    val dtlb = EmbeddedTLB(in = backend.io.dmem, mem = dmemXbar.io.in(2), flush = false.B, csrMMU = backend.io.memMMU.dmem, enable = HasDTLB)(TLBConfig(name = "dtlb", totalEntry = 64))
    dmemXbar.io.in(0) <> dtlb.io.out
    io.dmem <> Cache(in = dmemXbar.io.out, mmio = mmioXbar.io.in.drop(1), flush = "b00".U, empty = dtlb.io.cacheEmpty, enable = HasDcache)(CacheConfig(ro = false, name = "dcache"))

    // redirect
    frontend.io.redirect <> backend.io.redirect
    backend.io.flush := frontend.io.flushVec(3,2)

    // Make DMA access through L1 DCache to keep coherence
    dmemXbar.io.in(3) <> io.frontend

    io.mmio <> mmioXbar.io.out
  }

  Debug("------------------------ BACKEND ------------------------\n")
}
