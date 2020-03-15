package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import bus.simplebus._
import bus.axi4._
import utils._

trait HasNOOPParameter {
  val XLEN = 64
  val HasMExtension = true
  val HasCExtension = true
  val HasDiv = true
  val HasIcache = true
  val HasDcache = true
  val EnableStoreQueue = false
  val AddrBits = 64 // AddrBits is used in some cases
  val VAddrBits = 39 // VAddrBits is Virtual Memory addr bits
  val PAddrBits = 32 // PAddrBits is Phyical Memory addr bits
  val AddrBytes = AddrBits / 8 // unused
  val DataBits = XLEN
  val DataBytes = DataBits / 8
  val EnableMultiIssue = true
  val EnableSuperScalarExec = true
  val EnableOutOfOrderExec = true
}

trait HasNOOPConst {
  val CacheReadWidth = 8
  val ICacheUserBundleWidth = 39*2 + 9 // TODO: this const depends on VAddrBits
  val DCacheUserBundleWidth = 16
}

abstract class NOOPModule extends Module with HasNOOPParameter with HasNOOPConst with HasExceptionNO
abstract class NOOPBundle extends Bundle with HasNOOPParameter with HasNOOPConst with HasBackendConst

case class NOOPConfig (
  FPGAPlatform: Boolean = true,
  EnableDebug: Boolean = true
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
    val frontend = Flipped(new SimpleBusUC())
  })

  def pipelineConnect2[T <: Data](left: DecoupledIO[T], right: DecoupledIO[T],
    isFlush: Bool, entries: Int = 4, pipe: Boolean = false) = {
    right <> FlushableQueue(left, isFlush,  entries = entries, pipe = pipe)
  }

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

  val mmioXbar = Module(new SimpleBusCrossbarNto1(if (HasDcache) 2 else 3))
  val dmemXbar = Module(new SimpleBusCrossbarNto1(4, userBits = if (HasDcache) DCacheUserBundleWidth else 0))

  if(EnableOutOfOrderExec){
    val backend = Module(new Backend)
    // backend.io := DontCare // TODO: Delete it
    PipelineVector2Connect(new DecodeIO, idu.io.out(0), idu.io.out(1), backend.io.in(0), backend.io.in(1), ifu.io.flushVec(1), 16)
    backend.io.flush := ifu.io.flushVec(2)
    ifu.io.redirect <> backend.io.redirect

    val itlb = TLB(in = ifu.io.imem, mem = dmemXbar.io.in(1), flush = ifu.io.flushVec(0) | ifu.io.bpFlush, csrMMU = backend.io.memMMU.imem)(TLBConfig(name = "itlb", userBits = ICacheUserBundleWidth, totalEntry = 4))
    ifu.io.ipf := itlb.io.ipf
    io.imem <> Cache(in = itlb.io.out, mmio = mmioXbar.io.in.take(1), flush = Fill(2, ifu.io.flushVec(0) | ifu.io.bpFlush), empty = itlb.io.cacheEmpty)(
      CacheConfig(ro = true, name = "icache", userBits = ICacheUserBundleWidth))
    
    val dtlb = TLB(in = backend.io.dmem, mem = dmemXbar.io.in(2), flush = false.B, csrMMU = backend.io.memMMU.dmem)(TLBConfig(name = "dtlb", userBits = DCacheUserBundleWidth, totalEntry = 64))
    dmemXbar.io.in(0) <> dtlb.io.out
    io.dmem <> Cache(in = dmemXbar.io.out, mmio = mmioXbar.io.in.drop(1), flush = "b00".U, empty = dtlb.io.cacheEmpty, enable = HasDcache)(
      CacheConfig(ro = false, name = "dcache", userBits = DCacheUserBundleWidth))

    // Make DMA access through L1 DCache to keep coherence
    val expender = Module(new SimpleBusUCExpender(userBits = DCacheUserBundleWidth, userVal = 0.U))
    expender.io.in <> io.frontend
    dmemXbar.io.in(3) <> expender.io.out

    Debug(){
      printf("------------------------ BACKEND : %d ------------------------\n", GTimer())
    }
  }else{
    val isu  = Module(new ISU)
    val exu  = Module(new EXU)
    val wbu  = Module(new WBU)
    PipelineVector2Connect(new DecodeIO, idu.io.out(0), idu.io.out(1), isu.io.in(0), isu.io.in(1), ifu.io.flushVec(1), 16)
    PipelineConnect(isu.io.out, exu.io.in, exu.io.out.fire(), ifu.io.flushVec(2))
    PipelineConnect(exu.io.out, wbu.io.in, true.B, ifu.io.flushVec(3))
    isu.io.flush := ifu.io.flushVec(2)
    exu.io.flush := ifu.io.flushVec(3)

    Debug() {
      printf("------------------------ TIMER: %d ------------------------\n", GTimer())
      printf("flush = %b, ifu:(%d,%d), ibf:(%d,%d), idu:(%d,%d), isu:(%d,%d), exu:(%d,%d), wbu: (%d,%d)\n",
        ifu.io.flushVec.asUInt, ifu.io.out.valid, ifu.io.out.ready,
        ibf.io.in.valid, ibf.io.in.ready, idu.io.in(0).valid, idu.io.in(0).ready, isu.io.in(0).valid, isu.io.in(0).ready,
        exu.io.in.valid, exu.io.in.ready, wbu.io.in.valid, wbu.io.in.ready)
      when (ifu.io.out.valid) { printf("IFU: pc = 0x%x, instr = 0x%x\n", ifu.io.out.bits.pc, ifu.io.out.bits.instr)} ; 
      when (ibf.io.in.valid) { printf("IBF: pc = 0x%x, instr = 0x%x\n", ibf.io.in.bits.pc, ibf.io.in.bits.instr)}
      when (idu.io.in(0).valid) { printf("IDU1: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", idu.io.in(0).bits.pc, idu.io.in(0).bits.instr, idu.io.in(0).bits.pnpc) }
      when (idu.io.in(1).valid) { printf("IDU2: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", idu.io.in(1).bits.pc, idu.io.in(1).bits.instr, idu.io.in(1).bits.pnpc) }
      when (isu.io.in(0).valid)  { printf("ISU: pc = 0x%x, pnpc = 0x%x\n", isu.io.in(0).bits.cf.pc, isu.io.in(0).bits.cf.pnpc)} ;
      when (exu.io.in.valid)  { printf("EXU: pc = 0x%x, pnpc = 0x%x\n", exu.io.in.bits(0).cf.pc, exu.io.in.bits(0).cf.pnpc)} ;
      when (wbu.io.in.valid)  { printf("WBU: pc = 0x%x rfWen:%d rfDest:%d rfData:%x Futype:%x\n", wbu.io.in.bits(0).decode.cf.pc, wbu.io.in.bits(0).decode.ctrl.rfWen, wbu.io.in.bits(0).decode.ctrl.rfDest, wbu.io.wb(0).rfData, wbu.io.in.bits(0).decode.ctrl.fuType )}
      // when (io.in.valid) { printf("TIMER: %d WBU: pc = 0x%x wen %x wdata %x mmio %x intrNO %x\n", GTimer(), io.in.bits.decode.cf.pc, io.wb.rfWen, io.wb.rfData, io.in.bits.isMMIO, io.in.bits.intrNO) }
      // printf(p"IFUO: redirectIO:${ifu.io.out.bits.redirect}\n") ; printf("IFUO: exceptionVec: %x\n", ifu.io.out.bits.exceptionVec.asUInt)} 
      // printf(p"IDUO: redirectIO:${idu.io.out.bits.cf.redirect} redirectIOC:${idu.io.redirect}\n") ; printf("IDUO: exceptionVec:%x\n", idu.io.out.bits.cf.exceptionVec.asUInt)}
      // printf(p"ISUO: ${isu.io.out.bits.cf.redirect}\n") ; printf("ISUO: exceptionVec:%x\n", isu.io.out.bits.cf.exceptionVec.asUInt)}
      // when (exu.io.out.bits.decode.cf.redirect.valid) { printf("EXUO: redirect valid:%d target:%x\n", exu.io.out.bits.decode.cf.redirect.valid, exu.io.out.bits.decode.cf.redirect.target) }
      // when (wbu.io.in.valid) { printf("WBU: pc = 0x%x rfWen:%d rfDest:%d rfData:%x Futype:%x commits(0):%x commits(1):%x commits(3):%x\n", wbu.io.in.bits.decode.cf.pc, wbu.io.in.bits.decode.ctrl.rfWen, wbu.io.in.bits.decode.ctrl.rfDest, wbu.io.wb.rfData, wbu.io.in.bits.decode.ctrl.fuType, wbu.io.in.bits.commits(0), wbu.io.in.bits.commits(1), wbu.io.in.bits.commits(3)) }
    }

    isu.io.wb <> wbu.io.wb
    ifu.io.redirect <> wbu.io.redirect
    // forward
    isu.io.forward <> exu.io.forward

    val itlb = TLB(in = ifu.io.imem, mem = dmemXbar.io.in(1), flush = ifu.io.flushVec(0) | ifu.io.bpFlush, csrMMU = exu.io.memMMU.imem)(TLBConfig(name = "itlb", userBits = ICacheUserBundleWidth, totalEntry = 4))
    ifu.io.ipf := itlb.io.ipf
    io.imem <> Cache(in = itlb.io.out, mmio = mmioXbar.io.in.take(1), flush = Fill(2, ifu.io.flushVec(0) | ifu.io.bpFlush), empty = itlb.io.cacheEmpty)(
      CacheConfig(ro = true, name = "icache", userBits = ICacheUserBundleWidth))
    
    val dtlb = TLB(in = exu.io.dmem, mem = dmemXbar.io.in(2), flush = false.B, csrMMU = exu.io.memMMU.dmem)(TLBConfig(name = "dtlb", totalEntry = 64))
    dmemXbar.io.in(0) <> dtlb.io.out
    io.dmem <> Cache(in = dmemXbar.io.out, mmio = mmioXbar.io.in.drop(1), flush = "b00".U, empty = dtlb.io.cacheEmpty, enable = HasDcache)(CacheConfig(ro = false, name = "dcache"))

    // Make DMA access through L1 DCache to keep coherence
    dmemXbar.io.in(3) <> io.frontend
  }  

  io.mmio <> mmioXbar.io.out
}
