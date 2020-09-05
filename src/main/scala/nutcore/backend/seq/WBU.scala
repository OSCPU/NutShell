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

  Debug(){
    when (io.in.valid) { printf("[COMMIT] TIMER: %d WBU: pc = 0x%x inst %x wen %x wdst %x wdata %x mmio %x intrNO %x\n", GTimer(), io.in.bits.decode.cf.pc, io.in.bits.decode.cf.instr, io.wb.rfWen, io.wb.rfDest, io.wb.rfData, io.in.bits.isMMIO, io.in.bits.intrNO) }
  }

  val falseWire = WireInit(false.B) // make BoringUtils.addSource happy
  BoringUtils.addSource(io.in.valid, "perfCntCondMinstret")
  BoringUtils.addSource(falseWire, "perfCntCondMultiCommit")
  
  if (!p.FPGAPlatform) {
    // generate needSkip vector
    // fix mip.mtip
    val isCSR = (io.in.bits.decode.cf.instr & "h7f".U) === "h73".U
    val isCSRMip = (io.in.bits.decode.cf.instr(31,20) === "h344".U) && isCSR
    val needSkip = io.in.bits.isMMIO || isCSRMip

    // define debug vector
    val skipVec = WireInit(VecInit(0.U(DifftestWidth.W).asBools))
    val wenVec = WireInit(VecInit(0.U(DifftestWidth.W).asBools))
    val wdataVec = Wire(Vec(DifftestWidth, UInt(XLEN.W)))
    val wdstVec = Wire(Vec(DifftestWidth, UInt(32.W)))
    val wpcVec = Wire(Vec(DifftestWidth, UInt(VAddrBits.W)))
    val isRVCVec = WireInit(VecInit(0.U(DifftestWidth.W).asBools))
    wdataVec := DontCare
    wdstVec := DontCare
    wpcVec := DontCare

    // assign debug vector
    skipVec(0) := needSkip
    wenVec(0) := io.wb.rfWen
    wdataVec(0) := io.wb.rfData
    wdstVec(0) := io.wb.rfDest
    wpcVec(0) := SignExt(io.in.bits.decode.cf.pc, XLEN)
    isRVCVec(0) := io.in.bits.decode.cf.instr(1,0)=/="b11".U

    // send debug signal to sim top
    BoringUtils.addSource(RegNext(io.in.valid).asUInt, "DIFFTEST_commit")
    BoringUtils.addSource(RegNext(SignExt(io.in.bits.decode.cf.pc, XLEN)), "DIFFTEST_thisPC")
    BoringUtils.addSource(RegNext(io.in.bits.decode.cf.instr), "DIFFTEST_thisINST")
    BoringUtils.addSource(RegNext(skipVec.asUInt), "DIFFTEST_skip")
    BoringUtils.addSource(RegNext(wenVec.asUInt), "DIFFTEST_wen")
    BoringUtils.addSource(RegNext(wdataVec), "DIFFTEST_wdata")
    BoringUtils.addSource(RegNext(wdstVec), "DIFFTEST_wdst")
    BoringUtils.addSource(RegNext(wpcVec), "DIFFTEST_wpc")
    BoringUtils.addSource(RegNext(isRVCVec.asUInt), "DIFFTEST_isRVC")
    BoringUtils.addSource(RegNext(io.in.bits.intrNO), "DIFFTEST_intrNO")
  } else {
    BoringUtils.addSource(io.in.valid, "ilaWBUvalid")
    BoringUtils.addSource(io.in.bits.decode.cf.pc, "ilaWBUpc")
    BoringUtils.addSource(io.wb.rfWen, "ilaWBUrfWen")
    BoringUtils.addSource(io.wb.rfDest, "ilaWBUrfDest")
    BoringUtils.addSource(io.wb.rfData, "ilaWBUrfData")
  }
}
