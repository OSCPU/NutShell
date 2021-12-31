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
import utils._
import bus.simplebus._
import chisel3.experimental.IO

class FrontendIO(implicit val p: NutCoreConfig) extends Bundle with HasNutCoreConst {
  val imem = new SimpleBusUC(userBits = ICacheUserBundleWidth, addrBits = VAddrBits)
  val out = Vec(2, Decoupled(new DecodeIO))
  val flushVec = Output(UInt(4.W))
  val redirect = Flipped(new RedirectIO)
  val bpFlush = Output(Bool())
  val ipf = Input(Bool())
}


trait HasFrontendIO {
  implicit val p: NutCoreConfig
  val io = IO(new FrontendIO)
}

class Frontend_ooo(implicit val p: NutCoreConfig) extends NutCoreModule with HasFrontendIO {
  def pipelineConnect2[T <: Data](left: DecoupledIO[T], right: DecoupledIO[T],
    isFlush: Bool, entries: Int = 4, pipe: Boolean = false) = {
    // NOTE: depend on https://github.com/chipsalliance/chisel3/pull/2245
    // right <> Queue(left,  entries = entries, pipe = pipe, flush = Some(isFlush))
    right <> FlushableQueue(left, isFlush, entries = entries, pipe = pipe)
  }

  val ifu  = Module(new IFU_ooo)
  val ibf = Module(new IBF)
  val idu  = Module(new IDU)

  pipelineConnect2(ifu.io.out, ibf.io.in, ifu.io.flushVec(0))
  PipelineVector2Connect(new CtrlFlowIO, ibf.io.out(0), ibf.io.out(1), idu.io.in(0), idu.io.in(1), ifu.io.flushVec(1), if (EnableOutOfOrderExec) 8 else 4)
  ibf.io.flush := ifu.io.flushVec(1)

  io.out <> idu.io.out
  io.redirect <> ifu.io.redirect
  io.flushVec <> ifu.io.flushVec
  io.bpFlush <> ifu.io.bpFlush
  io.ipf <> ifu.io.ipf
  io.imem <> ifu.io.imem

  Debug("------------------------ FRONTEND:------------------------\n")
  Debug("flush = %b, ifu:(%d,%d), ibf:(%d,%d), idu:(%d,%d)\n",
    ifu.io.flushVec.asUInt, ifu.io.out.valid, ifu.io.out.ready,
    ibf.io.in.valid, ibf.io.in.ready, idu.io.in(0).valid, idu.io.in(0).ready)
  Debug(ifu.io.out.valid, "IFU: pc = 0x%x, instr = 0x%x\n", ifu.io.out.bits.pc, ifu.io.out.bits.instr)
  Debug(ibf.io.in.valid, "IBF: pc = 0x%x, instr = 0x%x\n", ibf.io.in.bits.pc, ibf.io.in.bits.instr)
  Debug(idu.io.in(0).valid, "IDU1: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", idu.io.in(0).bits.pc, idu.io.in(0).bits.instr, idu.io.in(0).bits.pnpc)
  Debug(idu.io.in(1).valid, "IDU2: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", idu.io.in(1).bits.pc, idu.io.in(1).bits.instr, idu.io.in(1).bits.pnpc)
}

class Frontend_embedded(implicit val p: NutCoreConfig) extends NutCoreModule with HasFrontendIO {
  val ifu  = Module(new IFU_embedded)
  val idu  = Module(new IDU)

  PipelineConnect(ifu.io.out, idu.io.in(0), idu.io.out(0).fire, ifu.io.flushVec(0))
  idu.io.in(1) := DontCare

  io.out <> idu.io.out
  io.redirect <> ifu.io.redirect
  io.flushVec <> ifu.io.flushVec
  io.bpFlush <> ifu.io.bpFlush
  io.ipf <> ifu.io.ipf
  io.imem <> ifu.io.imem

    Debug("------------------------ FRONTEND:------------------------\n")
    Debug("flush = %b, ifu:(%d,%d), idu:(%d,%d)\n",
      ifu.io.flushVec.asUInt, ifu.io.out.valid, ifu.io.out.ready, idu.io.in(0).valid, idu.io.in(0).ready)
    Debug(ifu.io.out.valid, "IFU: pc = 0x%x, instr = 0x%x\n", ifu.io.out.bits.pc, ifu.io.out.bits.instr)
    Debug(idu.io.in(0).valid, "IDU1: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", idu.io.in(0).bits.pc, idu.io.in(0).bits.instr, idu.io.in(0).bits.pnpc)
}

class Frontend_inorder(implicit val p: NutCoreConfig) extends NutCoreModule with HasFrontendIO {
  val ifu  = Module(new IFU_inorder)
  val ibf = Module(new NaiveRVCAlignBuffer)
  val idu  = Module(new IDU)

  def PipelineConnect2[T <: Data](left: DecoupledIO[T], right: DecoupledIO[T],
    isFlush: Bool, entries: Int = 4, pipe: Boolean = false) = {
    // NOTE: depend on https://github.com/chipsalliance/chisel3/pull/2245
    // right <> Queue(left,  entries = entries, pipe = pipe, flush = Some(isFlush))
    right <> FlushableQueue(left, isFlush, entries = entries, pipe = pipe)
  }

  PipelineConnect2(ifu.io.out, ibf.io.in, ifu.io.flushVec(0))
  PipelineConnect(ibf.io.out, idu.io.in(0), idu.io.out(0).fire, ifu.io.flushVec(1))
  idu.io.in(1) := DontCare

  ibf.io.flush := ifu.io.flushVec(1)
  io.out <> idu.io.out
  io.redirect <> ifu.io.redirect
  io.flushVec <> ifu.io.flushVec
  io.bpFlush <> ifu.io.bpFlush
  io.ipf <> ifu.io.ipf
  io.imem <> ifu.io.imem

  Debug("------------------------ FRONTEND:------------------------\n")
  Debug("flush = %b, ifu:(%d,%d), idu:(%d,%d)\n",
    ifu.io.flushVec.asUInt, ifu.io.out.valid, ifu.io.out.ready, idu.io.in(0).valid, idu.io.in(0).ready)
  Debug(ifu.io.out.valid, "IFU: pc = 0x%x, instr = 0x%x\n", ifu.io.out.bits.pc, ifu.io.out.bits.instr)
  Debug(idu.io.in(0).valid, "IDU1: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", idu.io.in(0).bits.pc, idu.io.in(0).bits.instr, idu.io.in(0).bits.pnpc)
}