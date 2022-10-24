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
import chisel3.experimental.IO
import utils._
import top.Settings

trait HasTLBIO extends HasNutCoreParameter with HasTlbConst with HasCSRConst {
  class TLBIO extends Bundle {
    val in = Flipped(new SimpleBusUC(userBits = userBits, addrBits = VAddrBits))
    val out = new SimpleBusUC(userBits = userBits)

    val mem = new SimpleBusUC()
    val flush = Input(Bool())
    val csrMMU = new MMUIO
    val cacheEmpty = Input(Bool())
    val ipf = Output(Bool())
  }
  val io = IO(new TLBIO)
}

// Duplicate with TLBMD. Consider eliminate one.
class EmbeddedTLBMD(implicit val tlbConfig: TLBConfig) extends TlbModule {
  val io = IO(new Bundle {
    val tlbmd = Output(Vec(Ways, UInt(tlbLen.W)))
    val write = Flipped(new TLBMDWriteBundle(IndexBits = IndexBits, Ways = Ways, tlbLen = tlbLen))
    val rindex = Input(UInt(IndexBits.W))
    val ready = Output(Bool())
  })

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

class EmbeddedTLB(implicit val tlbConfig: TLBConfig) extends TlbModule with HasTLBIO {

  val satp = WireInit(0.U(XLEN.W))
  BoringUtils.addSink(satp, "CSRSATP")

  // tlb exec
  val tlbExec = Module(new EmbeddedTLBExec)
  val tlbEmpty = Module(new EmbeddedTLBEmpty)
  val mdTLB = Module(new EmbeddedTLBMD)
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

  tlbEmpty.io.in <> DontCare
  tlbEmpty.io.out.ready := DontCare
  PipelineConnectTLB(io.in.req, tlbExec.io.in, mdUpdate, tlbExec.io.isFinish, io.flush, vmEnable)
  if(tlbname == "dtlb") {
    PipelineConnect(tlbExec.io.out, tlbEmpty.io.in, tlbEmpty.io.out.fire(), io.flush)
  }
  when(!vmEnable) {
    tlbExec.io.out.ready := true.B // let existed request go out
    if( tlbname == "dtlb") { tlbEmpty.io.out.ready := true.B }
    io.out.req.valid := io.in.req.valid
    io.in.req.ready := io.out.req.ready
    io.out.req.bits.addr := io.in.req.bits.addr(PAddrBits-1, 0)
    io.out.req.bits.size := io.in.req.bits.size
    io.out.req.bits.cmd := io.in.req.bits.cmd
    io.out.req.bits.wmask := io.in.req.bits.wmask
    io.out.req.bits.wdata := io.in.req.bits.wdata
    io.out.req.bits.user.map(_ := io.in.req.bits.user.getOrElse(0.U))
  }.otherwise {
    if (tlbname == "dtlb") { io.out.req <> tlbEmpty.io.out}
    else { io.out.req <> tlbExec.io.out }
  }
  io.out.resp <> io.in.resp

  // lsu need dtlb signals
  if(tlbname == "dtlb") {
    val alreadyOutFinish = RegEnable(true.B, init=false.B, tlbExec.io.out.valid && !tlbExec.io.out.ready)
    when(alreadyOutFinish && tlbExec.io.out.fire()) { alreadyOutFinish := false.B}
    val tlbFinish = (tlbExec.io.out.valid && !alreadyOutFinish) || tlbExec.io.pf.isPF()
    BoringUtils.addSource(tlbFinish, "DTLBFINISH")
    BoringUtils.addSource(io.csrMMU.isPF(), "DTLBPF")
    BoringUtils.addSource(vmEnable, "DTLBENABLE")
  }

  // instruction page fault
  if (tlbname == "itlb") {
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

class EmbeddedTLBEmpty(implicit val tlbConfig: TLBConfig) extends TlbModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new SimpleBusReqBundle(userBits = userBits)))
    val out = Decoupled(new SimpleBusReqBundle(userBits = userBits))
  })

  io.out <> io.in
}

class EmbeddedTLB_fake(implicit val tlbConfig: TLBConfig) extends TlbModule with HasTLBIO {
  io.mem <> DontCare
  io.out <> io.in
  io.csrMMU.loadPF := false.B
  io.csrMMU.storePF := false.B
  io.csrMMU.addr := io.in.req.bits.addr
  io.ipf := false.B
}


object EmbeddedTLB {
  def apply(in: SimpleBusUC, mem: SimpleBusUC, flush: Bool, csrMMU: MMUIO, enable: Boolean = true)(implicit tlbConfig: TLBConfig) = {
    val tlb = if (enable) {
      Module(new EmbeddedTLB)
    } else {
      Module(new EmbeddedTLB_fake)
    }
    tlb.io.in <> in
    tlb.io.mem <> mem
    tlb.io.flush := flush
    tlb.io.csrMMU <> csrMMU
    tlb
  }
}