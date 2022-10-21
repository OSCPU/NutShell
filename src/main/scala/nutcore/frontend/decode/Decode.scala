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

package nutcore.frontend.decode

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import nutcore._

import utils._
import difftest._

class Decode(implicit val p: NutCoreConfig) extends NutCoreModule with HasInstrType {
  val io = IO(new Bundle {
    val in = Vec(2, Flipped(Decoupled(new CtrlFlowIO)))
    val out = Vec(2, Decoupled(new DecodeIO))
  })
  val decoder1  = Module(new DecodeUnit)
  val decoder2  = Module(new DecodeUnit)
  io.in(0) <> decoder1.io.in
  io.in(1) <> decoder2.io.in
  io.out(0) <> decoder1.io.out
  io.out(1) <> decoder2.io.out
  if(!EnableMultiIssue){
    io.in(1).ready := false.B
    decoder2.io.in.valid := false.B
  }

  val checkpoint_id = RegInit(0.U(64.W))

  // debug runahead
  val runahead = Module(new DifftestRunaheadEvent)
  runahead.io.clock         := clock
  runahead.io.coreid        := 0.U
  runahead.io.valid         := io.out(0).fire()
  runahead.io.branch        := decoder1.io.isBranch
  runahead.io.pc            := io.out(0).bits.cf.pc
  runahead.io.checkpoint_id := checkpoint_id
  when(runahead.io.valid && runahead.io.branch) {
    checkpoint_id := checkpoint_id + 1.U // allocate a new checkpoint_id
  }
  io.out(0).bits.cf.isBranch := decoder1.io.isBranch
  io.out(0).bits.cf.runahead_checkpoint_id := checkpoint_id
  // when(runahead.io.valid) {
  //   printf("fire pc %x branch %x inst %x\n", runahead.io.pc, runahead.io.branch, io.out(0).bits.cf.instr)
  // }

  if (!p.FPGAPlatform) {
    BoringUtils.addSource(decoder1.io.isWFI | decoder2.io.isWFI, "isWFI")
  }
}
