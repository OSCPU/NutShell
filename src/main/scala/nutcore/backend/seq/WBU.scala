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
import difftest._

class WBU(implicit val p: NutCoreConfig) extends NutCoreModule{
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new CommitIO))
    val wb = new WriteBackIO
    val redirect = new RedirectIO
  })

  io.wb.rfWen := io.in.bits.decode.ctrl.rfWen && io.in.valid
  io.wb.rfDest := io.in.bits.decode.ctrl.rfDest
  io.wb.rfData := io.in.bits.commits(io.in.bits.decode.ctrl.fuType)

  io.in.ready := true.B

  io.redirect := io.in.bits.decode.cf.redirect
  io.redirect.valid := io.in.bits.decode.cf.redirect.valid && io.in.valid
  
  val runahead_redirect = Module(new DifftestRunaheadRedirectEvent)
  runahead_redirect.io.clock := clock
  runahead_redirect.io.coreid := 0.U
  runahead_redirect.io.valid := io.redirect.valid
  runahead_redirect.io.pc := io.in.bits.decode.cf.pc // for debug only
  runahead_redirect.io.target_pc := io.in.bits.decode.cf.redirect.target // for debug only
  runahead_redirect.io.checkpoint_id := io.in.bits.decode.cf.runahead_checkpoint_id // make sure it is right

  // when(runahead_redirect.io.valid) {
  //   printf("DUT pc %x redirect to %x cpid %x\n", runahead_redirect.io.pc, runahead_redirect.io.target_pc, runahead_redirect.io.checkpoint_id)
  // }

  Debug(io.in.valid, "[COMMIT] pc = 0x%x inst %x wen %x wdst %x wdata %x mmio %x intrNO %x\n", io.in.bits.decode.cf.pc, io.in.bits.decode.cf.instr, io.wb.rfWen, io.wb.rfDest, io.wb.rfData, io.in.bits.isMMIO, io.in.bits.intrNO)

  val falseWire = WireInit(false.B) // make BoringUtils.addSource happy
  BoringUtils.addSource(io.in.valid, "perfCntCondMinstret")
  BoringUtils.addSource(falseWire, "perfCntCondMultiCommit")
  
  if (!p.FPGAPlatform) {
    val difftest_commit = Module(new DifftestInstrCommit)
    difftest_commit.io.clock    := clock
    difftest_commit.io.coreid   := 0.U
    difftest_commit.io.index    := 0.U

    difftest_commit.io.valid    := RegNext(io.in.valid)
    difftest_commit.io.pc       := RegNext(SignExt(io.in.bits.decode.cf.pc, AddrBits))
    difftest_commit.io.instr    := RegNext(io.in.bits.decode.cf.instr)
    difftest_commit.io.skip     := RegNext(io.in.bits.isMMIO)
    difftest_commit.io.isRVC    := RegNext(io.in.bits.decode.cf.instr(1,0)=/="b11".U)
    difftest_commit.io.rfwen    := RegNext(io.wb.rfWen && io.wb.rfDest =/= 0.U) // && valid(ringBufferTail)(i) && commited(ringBufferTail)(i)
    difftest_commit.io.fpwen    := false.B
    // difftest.io.wdata    := RegNext(io.wb.rfData)
    difftest_commit.io.wdest    := RegNext(io.wb.rfDest)
    difftest_commit.io.wpdest   := RegNext(io.wb.rfDest)

    val difftest_wb = Module(new DifftestIntWriteback)
    difftest_wb.io.clock := clock
    difftest_wb.io.coreid := 0.U
    difftest_wb.io.valid := RegNext(io.wb.rfWen && io.wb.rfDest =/= 0.U)
    difftest_wb.io.dest := RegNext(io.wb.rfDest)
    difftest_wb.io.data := RegNext(io.wb.rfData)

    val runahead_commit = Module(new DifftestRunaheadCommitEvent)
    runahead_commit.io.clock := clock
    runahead_commit.io.coreid := 0.U
    runahead_commit.io.valid := RegNext(io.in.valid && io.in.bits.decode.cf.isBranch)
    runahead_commit.io.pc    := RegNext(SignExt(io.in.bits.decode.cf.pc, AddrBits))
    // when(runahead_commit.io.valid) {
    //   printf("DUT commit branch %x\n", runahead_commit.io.pc)
    // }
  } else {
    BoringUtils.addSource(io.in.valid, "ilaWBUvalid")
    BoringUtils.addSource(io.in.bits.decode.cf.pc, "ilaWBUpc")
    BoringUtils.addSource(io.wb.rfWen, "ilaWBUrfWen")
    BoringUtils.addSource(io.wb.rfDest, "ilaWBUrfDest")
    BoringUtils.addSource(io.wb.rfData, "ilaWBUrfData")
  }
}
