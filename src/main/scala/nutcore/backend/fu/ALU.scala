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
import top.Settings

object ALUOpType {
  def add  = "b1000000".U
  def sll  = "b0000001".U
  def slt  = "b0000010".U
  def sltu = "b0000011".U
  def xor  = "b0000100".U
  def srl  = "b0000101".U
  def or   = "b0000110".U
  def and  = "b0000111".U
  def sub  = "b0001000".U
  def sra  = "b0001101".U

  def addw = "b1100000".U
  def subw = "b0101000".U
  def sllw = "b0100001".U
  def srlw = "b0100101".U
  def sraw = "b0101101".U

  def isWordOp(func: UInt) = func(5)

  def jal  = "b1011000".U
  def jalr = "b1011010".U
  def beq  = "b0010000".U
  def bne  = "b0010001".U
  def blt  = "b0010100".U
  def bge  = "b0010101".U
  def bltu = "b0010110".U
  def bgeu = "b0010111".U

  // for RAS
  def call = "b1011100".U
  def ret  = "b1011110".U

  def isAdd(func: UInt) = func(6)
  def pcPlus2(func: UInt) = func(5)
  def isBru(func: UInt) = func(4)
  def isBranch(func: UInt) = !func(3)
  def isJump(func: UInt) = isBru(func) && !isBranch(func)
  def getBranchType(func: UInt) = func(2, 1)
  def isBranchInvert(func: UInt) = func(0)
}

class ALUIO extends FunctionUnitIO {
  val cfIn = Flipped(new CtrlFlowIO)
  val redirect = new RedirectIO
  val offset = Input(UInt(XLEN.W))
}

class ALU(hasBru: Boolean = false) extends NutCoreModule {
  val io = IO(new ALUIO)

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  val isAdderSub = !ALUOpType.isAdd(func)
  val adderRes = (src1 +& (src2 ^ Fill(XLEN, isAdderSub))) + isAdderSub
  val xorRes = src1 ^ src2
  val sltu = !adderRes(XLEN)
  val slt = xorRes(XLEN-1) ^ sltu

  val shsrc1 = LookupTreeDefault(func, src1(XLEN-1,0), List(
    ALUOpType.srlw -> ZeroExt(src1(31,0), XLEN),
    ALUOpType.sraw -> SignExt(src1(31,0), XLEN)
  ))
  val shamt = Mux(ALUOpType.isWordOp(func), src2(4, 0), if (XLEN == 64) src2(5, 0) else src2(4, 0))
  val res = LookupTreeDefault(func(3, 0), adderRes, List(
    ALUOpType.sll  -> ((shsrc1  << shamt)(XLEN-1, 0)),
    ALUOpType.slt  -> ZeroExt(slt, XLEN),
    ALUOpType.sltu -> ZeroExt(sltu, XLEN),
    ALUOpType.xor  -> xorRes,
    ALUOpType.srl  -> (shsrc1  >> shamt),
    ALUOpType.or   -> (src1  |  src2),
    ALUOpType.and  -> (src1  &  src2),
    ALUOpType.sra  -> ((shsrc1.asSInt >> shamt).asUInt)
  ))
  val aluRes = Mux(ALUOpType.isWordOp(func), SignExt(res(31,0), 64), res)

  val branchOpTable = List(
    ALUOpType.getBranchType(ALUOpType.beq)  -> !xorRes.orR,
    ALUOpType.getBranchType(ALUOpType.blt)  -> slt,
    ALUOpType.getBranchType(ALUOpType.bltu) -> sltu
  )

  val isBranch = ALUOpType.isBranch(func)
  val isBru = ALUOpType.isBru(func)
  val taken = LookupTree(ALUOpType.getBranchType(func), branchOpTable) ^ ALUOpType.isBranchInvert(func)
  val target = Mux(isBranch, io.cfIn.pc + io.offset, adderRes)(VAddrBits-1,0)
  val predictWrong = Mux(!taken && isBranch, io.cfIn.brIdx(0), !io.cfIn.brIdx(0) || (io.redirect.target =/= io.cfIn.pnpc))
  val isRVC = (io.cfIn.instr(1,0) =/= "b11".U)
  assert(io.cfIn.instr(1,0) === "b11".U || isRVC || !valid)
  Debug(valid && (io.cfIn.instr(1,0) === "b11".U) =/= !isRVC, "[ERROR] pc %x inst %x rvc %x\n",io.cfIn.pc, io.cfIn.instr, isRVC)
  io.redirect.target := Mux(!taken && isBranch, Mux(isRVC, io.cfIn.pc + 2.U, io.cfIn.pc + 4.U), target)
  // with branch predictor, this is actually to fix the wrong prediction
  io.redirect.valid := valid && isBru && predictWrong
  val redirectRtype = if (EnableOutOfOrderExec) 1.U else 0.U
  io.redirect.rtype := redirectRtype
  // mark redirect type as speculative exec fix
  // may be can be moved to ISU to calculate pc + 4
  // this is actually for jal and jalr to write pc + 4/2 to rd
  io.out.bits := Mux(isBru, Mux(!isRVC, SignExt(io.cfIn.pc, AddrBits) + 4.U, SignExt(io.cfIn.pc, AddrBits) + 2.U), aluRes)

  Debug(valid && isBru, "tgt %x, valid:%d, npc: %x, pdwrong: %x\n", io.redirect.target, io.redirect.valid, io.cfIn.pnpc, predictWrong)
  Debug(valid && isBru, "taken:%d addrRes:%x src1:%x src2:%x func:%x\n", taken, adderRes, src1, src2, func)
  Debug(valid && isBru, "[BPW] pc %x tgt %x, npc: %x, pdwrong: %x type: %x%x%x%x\n", io.cfIn.pc, io.redirect.target, io.cfIn.pnpc, predictWrong, isBranch, (func === ALUOpType.jal || func === ALUOpType.call), func === ALUOpType.jalr, func === ALUOpType.ret)
  Debug("valid:%d isBru:%d isBranch:%d \n", valid, isBru, isBranch)
  // Debug("pc %x instr %x tgt %x, npc: %x, pdwrong: %x type: %x%x%x%x\n", io.cfIn.pc, io.cfIn.instr, io.redirect.target, io.cfIn.pnpc, predictWrong, isBranch, (func === ALUOpType.jal || func === ALUOpType.call), func === ALUOpType.jalr, func === ALUOpType.ret)
  // Debug("func:%b ", func)
  // Debug("tgt %x, npc: %x, pdwrong: %x\n", io.redirect.target, io.cfIn.pnpc, predictWrong)
  // Debug("taken:%d addrRes:%x src1:%x src2:%x func:%x\n", taken, adderRes, src1, src2, func)

  Debug(valid && isBru, " bpuUpdateReq: valid:%d pc:%x isMissPredict:%d actualTarget:%x actualTaken:%x fuOpType:%x btbType:%x isRVC:%d \n", valid && isBru, io.cfIn.pc, predictWrong, target, taken, func, LookupTree(func, RV32I_BRUInstr.bruFuncTobtbTypeTable), isRVC)

  io.in.ready := io.out.ready
  io.out.valid := valid

  val bpuUpdateReq = WireInit(0.U.asTypeOf(new BPUUpdateReq))
  bpuUpdateReq.valid := valid && isBru
  bpuUpdateReq.pc := io.cfIn.pc
  bpuUpdateReq.isMissPredict := predictWrong
  bpuUpdateReq.actualTarget := target
  bpuUpdateReq.actualTaken := taken
  bpuUpdateReq.fuOpType := func
  bpuUpdateReq.btbType := LookupTree(func, RV32I_BRUInstr.bruFuncTobtbTypeTable)
  bpuUpdateReq.isRVC := isRVC

  if(hasBru){
    BoringUtils.addSource(RegNext(bpuUpdateReq), "bpuUpdateReq")

    val right = valid && isBru && !predictWrong
    val wrong = valid && isBru && predictWrong
    BoringUtils.addSource(WireInit(right && isBranch), "MbpBRight")
    BoringUtils.addSource(WireInit(wrong && isBranch), "MbpBWrong")
    BoringUtils.addSource(WireInit(wrong && isBranch && io.cfIn.pc(2,0)==="h0".U && isRVC), "Custom1")
    BoringUtils.addSource(WireInit(wrong && isBranch && io.cfIn.pc(2,0)==="h0".U && !isRVC), "Custom2")
    BoringUtils.addSource(WireInit(wrong && isBranch && io.cfIn.pc(2,0)==="h2".U && isRVC), "Custom3")
    BoringUtils.addSource(WireInit(wrong && isBranch && io.cfIn.pc(2,0)==="h2".U && !isRVC), "Custom4")
    BoringUtils.addSource(WireInit(wrong && isBranch && io.cfIn.pc(2,0)==="h4".U && isRVC), "Custom5")
    BoringUtils.addSource(WireInit(wrong && isBranch && io.cfIn.pc(2,0)==="h4".U && !isRVC), "Custom6")
    BoringUtils.addSource(WireInit(wrong && isBranch && io.cfIn.pc(2,0)==="h6".U && isRVC), "Custom7")
    BoringUtils.addSource(WireInit(wrong && isBranch && io.cfIn.pc(2,0)==="h6".U && !isRVC), "Custom8")
    BoringUtils.addSource(WireInit(right && (func === ALUOpType.jal || func === ALUOpType.call)), "MbpJRight")
    BoringUtils.addSource(WireInit(wrong && (func === ALUOpType.jal || func === ALUOpType.call)), "MbpJWrong")
    BoringUtils.addSource(WireInit(right && func === ALUOpType.jalr), "MbpIRight")
    BoringUtils.addSource(WireInit(wrong && func === ALUOpType.jalr), "MbpIWrong")
    BoringUtils.addSource(WireInit(right && func === ALUOpType.ret), "MbpRRight")
    BoringUtils.addSource(WireInit(wrong && func === ALUOpType.ret), "MbpRWrong")
  }
}
