package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import bus.simplebus._
import bus.axi4._
import utils._
import top.Settings

trait HasNOOPParameter {
  val XLEN = 64
  val NXLEN = if (Settings.IsRV32) 32 else 64
  val HasMExtension = true
  val HasCExtension = true
  val HasDiv = true
  val HasIcache = Settings.HasIcache
  val HasDcache = Settings.HasDcache
  val HasITLB = Settings.HasITLB
  val HasDTLB = Settings.HasDTLB
  val EnableStoreQueue = false
  val AddrBits = 64 // AddrBits is used in some cases
  val VAddrBits = Settings.VAddrBits // VAddrBits is Virtual Memory addr bits
  val PAddrBits = 32 // PAddrBits is Phyical Memory addr bits
  val AddrBytes = AddrBits / 8 // unused
  val DataBits = XLEN
  val DataBytes = DataBits / 8
  val EnableMultiIssue = Settings.EnableMultiIssue
  val EnableSuperScalarExec = Settings.EnableSuperScalarExec
  val EnableOutOfOrderExec = Settings.EnableOutOfOrderExec
}

trait HasNOOPConst {
  val CacheReadWidth = 8
  val ICacheUserBundleWidth = Settings.VAddrBits*2 + 9
}

abstract class NOOPModule extends Module with HasNOOPParameter with HasNOOPConst with HasExceptionNO
abstract class NOOPBundle extends Bundle with HasNOOPParameter with HasNOOPConst with HasBackendConst

case class NOOPConfig (
  FPGAPlatform: Boolean = true,
  EnableDebug: Boolean = Settings.EnableDebug
)

object AddressSpace {
  // (start, size)
  def mmio = List((0x0000000040000000L, 0x0000000010000000L))
  def dram = (0x0000000080000000L, 0x0000000010000000L)

  //def isMMIO(addr: UInt) = mmio.map(range => ((addr & ~((range._2 - 1).U(32.W))) === range._1.U)).reduce(_ || _)
  def isMMIO(addr: UInt) = addr(31,28) === "h4".U
}

class NOOP(implicit val p: NOOPConfig) extends NOOPModule {
  val io = IO(new Bundle {
    val imem = new SimpleBusC
    val dmem = new SimpleBusC
    val mmio = new SimpleBusUC
    val frontend = Flipped(new SimpleBusUC)
  })

  def pipelineConnect2[T <: Data](left: DecoupledIO[T], right: DecoupledIO[T],
    isFlush: Bool, entries: Int = 4, pipe: Boolean = false) = {
    right <> FlushableQueue(left, isFlush,  entries = entries, pipe = pipe)
  }

  // Frontend

  val ifu  = Module(new IFU)
  val ibf = Module(new IBF)
  val idu  = Module(new IDU)

  pipelineConnect2(ifu.io.out, ibf.io.in, ifu.io.flushVec(0))
  PipelineVector2Connect(new CtrlFlowIO, ibf.io.out(0), ibf.io.out(1), idu.io.in(0), idu.io.in(1), ifu.io.flushVec(1), 8)
  ibf.io.flush := ifu.io.flushVec(1)
  idu.io.flush := ifu.io.flushVec(1)
  
  Debug() {
    printf("------------------------ FRONTEND: %d ------------------------\n", GTimer())
    printf("flush = %b, ifu:(%d,%d), ibf:(%d,%d), idu:(%d,%d)\n",
      ifu.io.flushVec.asUInt, ifu.io.out.valid, ifu.io.out.ready,
      ibf.io.in.valid, ibf.io.in.ready, idu.io.in(0).valid, idu.io.in(0).ready)
    when (ifu.io.out.valid) { printf("IFU: pc = 0x%x, instr = 0x%x\n", ifu.io.out.bits.pc, ifu.io.out.bits.instr)} ; 
    when (ibf.io.in.valid) { printf("IBF: pc = 0x%x, instr = 0x%x\n", ibf.io.in.bits.pc, ibf.io.in.bits.instr)}
    when (idu.io.in(0).valid) { printf("IDU1: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", idu.io.in(0).bits.pc, idu.io.in(0).bits.instr, idu.io.in(0).bits.pnpc) }
    when (idu.io.in(1).valid) { printf("IDU2: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", idu.io.in(1).bits.pc, idu.io.in(1).bits.instr, idu.io.in(1).bits.pnpc) }
  }

  // Backend

  val backend = if (EnableOutOfOrderExec) Module(new Backend) else Module(new Backend_seq)
  PipelineVector2Connect(new DecodeIO, idu.io.out(0), idu.io.out(1), backend.io.in(0), backend.io.in(1), ifu.io.flushVec(1), 16)

  val mmioXbar = Module(new SimpleBusCrossbarNto1(2))
  val dmemXbar = Module(new SimpleBusCrossbarNto1(4))
  
  // itlb
  val itlb = TLB(in = ifu.io.imem, mem = dmemXbar.io.in(1), flush = ifu.io.flushVec(0) | ifu.io.bpFlush, csrMMU = backend.io.memMMU.imem, enable = HasITLB)(TLBConfig(name = "itlb", userBits = ICacheUserBundleWidth, totalEntry = 4))
  ifu.io.ipf := itlb.io.ipf
  io.imem <> Cache(in = itlb.io.out, mmio = mmioXbar.io.in.take(1), flush = Fill(2, ifu.io.flushVec(0) | ifu.io.bpFlush), empty = itlb.io.cacheEmpty, enable = HasIcache)(CacheConfig(ro = true, name = "icache", userBits = ICacheUserBundleWidth))
  
  // dtlb
  val dtlb = TLB(in = backend.io.dmem, mem = dmemXbar.io.in(2), flush = false.B, csrMMU = backend.io.memMMU.dmem, enable = HasDTLB)(TLBConfig(name = "dtlb", totalEntry = 64))
  dmemXbar.io.in(0) <> dtlb.io.out
  io.dmem <> Cache(in = dmemXbar.io.out, mmio = mmioXbar.io.in.drop(1), flush = "b00".U, empty = dtlb.io.cacheEmpty, enable = HasDcache)(CacheConfig(ro = false, name = "dcache"))

  // redirect
  ifu.io.redirect <> backend.io.redirect

  if (EnableOutOfOrderExec) {
    backend.io.flush := ifu.io.flushVec(2)
  } else {
    backend.io.flush := ifu.io.flushVec(3,2)
  }

  // Make DMA access through L1 DCache to keep coherence
  dmemXbar.io.in(3) <> io.frontend

  io.mmio <> mmioXbar.io.out
  
  Debug() {
    printf("------------------------ BACKEND : %d ------------------------\n", GTimer())
  }
}
