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

package nutcore.mem.tlb

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import nutcore._

import bus.simplebus._
import bus.axi4._
import utils._
import top.Settings

trait Sv39Const extends HasNutCoreParameter{
  val Level = 3
  val offLen  = 12
  val ppn0Len = 9
  val ppn1Len = 9
  val ppn2Len = PAddrBits - offLen - ppn0Len - ppn1Len // 2
  val ppnLen = ppn2Len + ppn1Len + ppn0Len
  val vpn2Len = 9
  val vpn1Len = 9
  val vpn0Len = 9
  val vpnLen = vpn2Len + vpn1Len + vpn0Len
  
  //val paddrLen = PAddrBits
  //val vaddrLen = VAddrBits
  val satpLen = XLEN
  val satpModeLen = 4
  val asidLen = 16
  val flagLen = 8

  val ptEntryLen = XLEN
  val satpResLen = XLEN - ppnLen - satpModeLen - asidLen
  //val vaResLen = 25 // unused
  //val paResLen = 25 // unused 
  val pteResLen = XLEN - ppnLen - 2 - flagLen

  def vaBundle = new Bundle {
    val vpn2 = UInt(vpn2Len.W)
    val vpn1 = UInt(vpn1Len.W)
    val vpn0 = UInt(vpn0Len.W)
    val off  = UInt( offLen.W)
  }

  def vaBundle2 = new Bundle {
    val vpn  = UInt(vpnLen.W)
    val off  = UInt(offLen.W)
  }

  def vaBundle3 = new Bundle {
    val vpn = UInt(vpnLen.W)
    val off = UInt(offLen.W)
  }

  def vpnBundle = new Bundle {
    val vpn2 = UInt(vpn2Len.W)
    val vpn1 = UInt(vpn1Len.W)
    val vpn0 = UInt(vpn0Len.W)
  }

  def paBundle = new Bundle {
    val ppn2 = UInt(ppn2Len.W)
    val ppn1 = UInt(ppn1Len.W)
    val ppn0 = UInt(ppn0Len.W)
    val off  = UInt( offLen.W)
  }

  def paBundle2 = new Bundle {
    val ppn  = UInt(ppnLen.W)
    val off  = UInt(offLen.W)
  }

  def paddrApply(ppn: UInt, vpnn: UInt):UInt = {
    Cat(Cat(ppn, vpnn), 0.U(3.W))
  }
  
  def pteBundle = new Bundle {
    val reserved  = UInt(pteResLen.W)
    val ppn  = UInt(ppnLen.W)
    val rsw  = UInt(2.W)
    val flag = new Bundle {
      val d    = UInt(1.W)
      val a    = UInt(1.W)
      val g    = UInt(1.W)
      val u    = UInt(1.W)
      val x    = UInt(1.W)
      val w    = UInt(1.W)
      val r    = UInt(1.W)
      val v    = UInt(1.W)
    }
  }

  def satpBundle = new Bundle {
    val mode = UInt(satpModeLen.W)
    val asid = UInt(asidLen.W)
    val res = UInt(satpResLen.W)
    val ppn  = UInt(ppnLen.W)
  }

  def flagBundle = new Bundle {
    val d    = Bool()//UInt(1.W)
    val a    = Bool()//UInt(1.W)
    val g    = Bool()//UInt(1.W)
    val u    = Bool()//UInt(1.W)
    val x    = Bool()//UInt(1.W)
    val w    = Bool()//UInt(1.W)
    val r    = Bool()//UInt(1.W)
    val v    = Bool()//UInt(1.W)
  }

  def maskPaddr(ppn:UInt, vaddr:UInt, mask:UInt) = {
    MaskData(vaddr, Cat(ppn, 0.U(offLen.W)), Cat(Fill(ppn2Len, 1.U(1.W)), mask, 0.U(offLen.W)))
  }

  def MaskEQ(mask: UInt, pattern: UInt, vpn: UInt) = {
    (Cat("h1ff".U(vpn2Len.W), mask) & pattern) === (Cat("h1ff".U(vpn2Len.W), mask) & vpn)
  }

}

case class TLBConfig (
  name: String = "tlb",
  userBits: Int = 0,

  totalEntry: Int = 4,
  ways: Int = 4
)

trait HasTlbConst extends Sv39Const{
  implicit val tlbConfig: TLBConfig

  val AddrBits: Int
  val PAddrBits: Int
  val VAddrBits: Int
  val XLEN: Int

  val tlbname = tlbConfig.name
  val userBits = tlbConfig.userBits

  val maskLen = vpn0Len + vpn1Len  // 18
  val metaLen = vpnLen + asidLen + maskLen + flagLen // 27 + 16 + 18 + 8 = 69, is asid necessary 
  val dataLen = ppnLen + PAddrBits // 
  val tlbLen = metaLen + dataLen
  val Ways = tlbConfig.ways
  val TotalEntry = tlbConfig.totalEntry
  val Sets = TotalEntry / Ways
  val IndexBits = log2Up(Sets)
  val TagBits = vpnLen - IndexBits

  val debug = false //&& tlbname == "dtlb"

  def vaddrTlbBundle = new Bundle {
    val tag = UInt(TagBits.W)
    val index = UInt(IndexBits.W)
    val off = UInt(offLen.W)
  }

  def metaBundle = new Bundle {
    val vpn = UInt(vpnLen.W)
    val asid = UInt(asidLen.W)
    val mask = UInt(maskLen.W) // to support super page
    val flag = UInt(flagLen.W)
  }

  def dataBundle = new Bundle {
    val ppn = UInt(ppnLen.W)
    val pteaddr = UInt(PAddrBits.W) // pte addr, used to write back pte when flag changes (flag.d, flag.v)
  }

  def tlbBundle = new Bundle {
    val vpn = UInt(vpnLen.W)
    val asid = UInt(asidLen.W)
    val mask = UInt(maskLen.W)
    val flag = UInt(flagLen.W)
    val ppn = UInt(ppnLen.W)
    val pteaddr = UInt(PAddrBits.W)
  }

  def tlbBundle2 = new Bundle {
    val meta = UInt(metaLen.W)
    val data = UInt(dataLen.W)
  }

  def getIndex(vaddr: UInt) : UInt = {
    vaddr.asTypeOf(vaddrTlbBundle).index
  }
}

abstract class TlbBundle(implicit tlbConfig: TLBConfig) extends NutCoreBundle with HasNutCoreParameter with HasTlbConst with Sv39Const
abstract class TlbModule(implicit tlbConfig: TLBConfig) extends NutCoreModule with HasNutCoreParameter with HasTlbConst with Sv39Const with HasCSRConst

class TLBMDWriteBundle (val IndexBits: Int, val Ways: Int, val tlbLen: Int) extends Bundle with HasNutCoreParameter with Sv39Const {
  val wen = Output(Bool())
  val windex = Output(UInt(IndexBits.W))
  val waymask = Output(UInt(Ways.W))
  val wdata = Output(UInt(tlbLen.W))
  
  def apply(wen: UInt, windex: UInt, waymask: UInt, vpn: UInt, asid: UInt, mask: UInt, flag: UInt, ppn: UInt, pteaddr: UInt) {
    this.wen := wen
    this.windex := windex
    this.waymask := waymask
    this.wdata := Cat(vpn, asid, mask, flag, ppn, pteaddr)
  }
}

class TLBMD(implicit val tlbConfig: TLBConfig) extends TlbModule {
  class TLBMDIO extends Bundle {
    val tlbmd = Output(Vec(Ways, UInt(tlbLen.W)))
    val write = Flipped(new TLBMDWriteBundle(IndexBits = IndexBits, Ways = Ways, tlbLen = tlbLen))
    val rindex = Input(UInt(IndexBits.W))
    val ready = Output(Bool())
  }
  val io = IO(new TLBMDIO)

  //val tlbmd = Reg(Vec(Ways, UInt(tlbLen.W)))
  val tlbmd = Mem(Sets, Vec(Ways, UInt(tlbLen.W)))
  io.tlbmd := tlbmd(io.rindex)

  //val reset = WireInit(false.B)
  val resetState = RegInit(true.B)//RegEnable(true.B, init = true.B, reset)
  val (resetSet, resetFinish) = Counter(resetState, Sets)
  when (resetFinish) { resetState := false.B }

  val writeWen = io.write.wen//WireInit(false.B)
  val writeSetIdx = io.write.windex
  val writeWayMask = io.write.waymask
  val writeData = io.write.wdata

  val wen = Mux(resetState, true.B, writeWen)
  val setIdx = Mux(resetState, resetSet, writeSetIdx)
  val waymask = Mux(resetState, Fill(Ways, "b1".U), writeWayMask)
  val dataword = Mux(resetState, 0.U, writeData)
  val wdata = VecInit(Seq.fill(Ways)(dataword))

  when (wen) { tlbmd.write(setIdx, wdata, waymask.asBools) }

  io.ready := !resetState
  def rready() = !resetState
  def wready() = !resetState
}

class TLB(implicit val tlbConfig: TLBConfig) extends TlbModule{
  class TLBIO extends Bundle {
    val in = Flipped(new SimpleBusUC(userBits = userBits, addrBits = VAddrBits))
    val out = new SimpleBusUC(userBits = userBits)

    val mem = new SimpleBusUC(userBits = userBits)
    val flush = Input(Bool())
    val csrMMU = new MMUIO
    val cacheEmpty = Input(Bool())
    val ipf = Output(Bool())
  }
  val io = IO(new TLBIO)

  val satp = WireInit(0.U(XLEN.W))
  BoringUtils.addSink(satp, "CSRSATP")

  // tlb exec
  val tlbExec = Module(new TLBExec)
  val mdTLB = Module(new TLBMD)
  val mdUpdate = Wire(Bool())
  
  tlbExec.io.flush := io.flush
  tlbExec.io.satp := satp
  tlbExec.io.mem <> io.mem
  tlbExec.io.pf <> io.csrMMU
  tlbExec.io.md <> RegEnable(mdTLB.io.tlbmd, mdUpdate)
  tlbExec.io.mdReady := mdTLB.io.ready
  mdTLB.io.rindex := getIndex(io.in.req.bits.addr)
  mdTLB.io.write <> tlbExec.io.mdWrite
  
  io.ipf := false.B
  
  // meta reset
  val flushTLB = WireInit(false.B)
  BoringUtils.addSink(flushTLB, "MOUFlushTLB")
  mdTLB.reset := reset.asBool || flushTLB

  // VM enable && io
  val vmEnable = satp.asTypeOf(satpBundle).mode === 8.U && (io.csrMMU.priviledgeMode < ModeM)

  def PipelineConnectTLB[T <: Data](left: DecoupledIO[T], right: DecoupledIO[T], update: Bool, rightOutFire: Bool, isFlush: Bool, vmEnable: Bool) = {
    val valid = RegInit(false.B)
    when (rightOutFire) { valid := false.B }
    when (left.valid && right.ready && vmEnable) { valid := true.B }
    when (isFlush) { valid := false.B }

    left.ready := right.ready
    right.bits <> RegEnable(left.bits, left.valid && right.ready)
    right.valid := valid //&& !isFlush

    update := left.valid && right.ready
  }

  PipelineConnectTLB(io.in.req, tlbExec.io.in, mdUpdate, tlbExec.io.isFinish, io.flush, vmEnable)
  when(!vmEnable) {
    tlbExec.io.out.ready := true.B // let existed request go out
    io.out.req.valid := io.in.req.valid
    io.in.req.ready := io.out.req.ready
    io.out.req.bits.addr := io.in.req.bits.addr(PAddrBits-1, 0)
    io.out.req.bits.size := io.in.req.bits.size
    io.out.req.bits.cmd := io.in.req.bits.cmd
    io.out.req.bits.wmask := io.in.req.bits.wmask
    io.out.req.bits.wdata := io.in.req.bits.wdata
    io.out.req.bits.user.map(_ := io.in.req.bits.user.getOrElse(0.U))
  }.otherwise {
    io.out.req <> tlbExec.io.out
  }

  // lsu need dtlb signals
  if(tlbname == "dtlb") {
    // io.in.resp <> TLBExec.io.in.resp
    io.out.resp.ready := true.B
    io.in.resp.valid := tlbExec.io.out.valid
    io.in.resp.bits.rdata := tlbExec.io.out.bits.addr
    io.in.resp.bits.cmd := DontCare
    io.in.resp.bits.user.map(_ := tlbExec.io.out.bits.user.getOrElse(0.U))
    val alreadyOutFinish = RegEnable(true.B, init=false.B, tlbExec.io.out.valid && !tlbExec.io.out.ready)
    // when(alreadyOutFinish && tlbExec.io.out.fire()) { alreadyOutFinish := false.B}
    when(alreadyOutFinish && tlbExec.io.out.valid) { alreadyOutFinish := false.B}//???
    val tlbFinish = (tlbExec.io.out.valid && !alreadyOutFinish) || tlbExec.io.pf.isPF()
    BoringUtils.addSource(tlbFinish, "DTLBFINISH")
    BoringUtils.addSource(io.csrMMU.isPF(), "DTLBPF")
    BoringUtils.addSource(vmEnable, "DTLBENABLE")
  }

  // instruction page fault
  if (tlbname == "itlb") {
    io.out.resp <> io.in.resp
    when (tlbExec.io.ipf && vmEnable) {
      tlbExec.io.out.ready := io.cacheEmpty && io.in.resp.ready
      io.out.req.valid := false.B
    }

    when (tlbExec.io.ipf && vmEnable && io.cacheEmpty) {
      io.in.resp.valid := true.B
      io.in.resp.bits.rdata := 0.U
      io.in.resp.bits.cmd := SimpleBusCmd.readLast
      io.in.resp.bits.user.map(_ := tlbExec.io.in.bits.user.getOrElse(0.U))
      io.ipf := tlbExec.io.ipf
    }
  }
  Debug("InReq(%d, %d) InResp(%d, %d) OutReq(%d, %d) OutResp(%d, %d) vmEnable:%d mode:%d\n", io.in.req.valid, io.in.req.ready, io.in.resp.valid, io.in.resp.ready, io.out.req.valid, io.out.req.ready, io.out.resp.valid, io.out.resp.ready, vmEnable, io.csrMMU.priviledgeMode)
  Debug("InReq: addr:%x cmd:%d wdata:%x OutReq: addr:%x cmd:%x wdata:%x\n", io.in.req.bits.addr, io.in.req.bits.cmd, io.in.req.bits.wdata, io.out.req.bits.addr, io.out.req.bits.cmd, io.out.req.bits.wdata)
  Debug("OutResp: rdata:%x cmd:%x Inresp: rdata:%x cmd:%x\n", io.out.resp.bits.rdata, io.out.resp.bits.cmd, io.in.resp.bits.rdata, io.in.resp.bits.cmd)
  Debug("satp:%x flush:%d cacheEmpty:%d instrPF:%d loadPF:%d storePF:%d \n", satp, io.flush, io.cacheEmpty, io.ipf, io.csrMMU.loadPF, io.csrMMU.storePF)
}


class TLBEmpty(implicit val tlbConfig: TLBConfig) extends TlbModule {
  class TLBEmptyIO extends Bundle {
    val in = Flipped(Decoupled(new SimpleBusReqBundle(userBits = userBits)))
    val out = Decoupled(new SimpleBusReqBundle(userBits = userBits))
  }
  val io = IO(new TLBEmptyIO)

  io.out <> io.in
}

object TLB {
  def apply(in: SimpleBusUC, mem: SimpleBusUC, flush: Bool, csrMMU: MMUIO)(implicit tlbConfig: TLBConfig) = {
    val tlb = Module(new TLB)
    tlb.io.in <> in
    tlb.io.mem <> mem
    tlb.io.flush := flush
    tlb.io.csrMMU <> csrMMU
    tlb
  }
}