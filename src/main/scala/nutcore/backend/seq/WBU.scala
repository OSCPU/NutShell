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

  Debug(io.in.valid, "[COMMIT] pc = 0x%x inst %x wen %x wdst %x wdata %x mmio %x intrNO %x\n", io.in.bits.decode.cf.pc, io.in.bits.decode.cf.instr, io.wb.rfWen, io.wb.rfDest, io.wb.rfData, io.in.bits.isMMIO, io.in.bits.intrNO)

  val falseWire = WireInit(false.B) // make BoringUtils.addSource happy
  BoringUtils.addSource(io.in.valid, "perfCntCondMinstret")
  BoringUtils.addSource(falseWire, "perfCntCondMultiCommit")
  
  if (!p.FPGAPlatform) {
    BoringUtils.addSource(RegNext(io.in.valid), "difftestCommit")
    BoringUtils.addSource(falseWire, "difftestMultiCommit")
    BoringUtils.addSource(RegNext(SignExt(io.in.bits.decode.cf.pc, AddrBits)), "difftestThisPC")
    BoringUtils.addSource(RegNext(io.in.bits.decode.cf.instr), "difftestThisINST")
    BoringUtils.addSource(RegNext(io.in.bits.isMMIO), "difftestIsMMIO")
    BoringUtils.addSource(RegNext(io.in.bits.decode.cf.instr(1,0)=/="b11".U), "difftestIsRVC")
    BoringUtils.addSource(falseWire, "difftestIsRVC2")
    BoringUtils.addSource(RegNext(io.in.bits.intrNO), "difftestIntrNO")
  } else {
    BoringUtils.addSource(io.in.valid, "ilaWBUvalid")
    BoringUtils.addSource(io.in.bits.decode.cf.pc, "ilaWBUpc")
    BoringUtils.addSource(io.wb.rfWen, "ilaWBUrfWen")
    BoringUtils.addSource(io.wb.rfDest, "ilaWBUrfDest")
    BoringUtils.addSource(io.wb.rfData, "ilaWBUrfData")
  }
}
