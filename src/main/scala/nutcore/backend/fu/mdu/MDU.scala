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

object MDUOpType {
  def mul    = "b0000".U
  def mulh   = "b0001".U
  def mulhsu = "b0010".U
  def mulhu  = "b0011".U
  def div    = "b0100".U
  def divu   = "b0101".U
  def rem    = "b0110".U
  def remu   = "b0111".U

  def mulw   = "b1000".U
  def divw   = "b1100".U
  def divuw  = "b1101".U
  def remw   = "b1110".U
  def remuw  = "b1111".U

  def isDiv(op: UInt) = op(2)
  def isDivSign(op: UInt) = isDiv(op) && !op(0)
  def isW(op: UInt) = op(3)
}

class MulDivIO(val len: Int) extends Bundle {
  val in = Flipped(DecoupledIO(Vec(2, Output(UInt(len.W)))))
  val sign = Input(Bool())
  val out = DecoupledIO(Output(UInt((len * 2).W)))
}

abstract class Multiplier(len: Int) extends NutCoreModule {
  val io = IO(new MulDivIO(len))
}

abstract class Divider(len: Int) extends NutCoreModule {
  val io = IO(new MulDivIO(len))
}

class MDUIO extends FunctionUnitIO {
}

class MDU extends NutCoreModule {
  val io = IO(new MDUIO)
  val useRadix4Div = true
  val useArrayMult = true

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  val isDiv = MDUOpType.isDiv(func)
  val isDivSign = MDUOpType.isDivSign(func)
  val isW = MDUOpType.isW(func)

  val mul = Module(if (useArrayMult) new ArrayMultiplierWrapper(2)(XLEN+1) else new PipedMultiplier(XLEN + 1))
  val div = Module(if (useRadix4Div) new Radix4Divider(64) else new Radix2Divider(64))
  List(mul.io, div.io).map { case x =>
    x.sign := isDivSign
    x.out.ready := io.out.ready
  }

  val signext = SignExt(_: UInt, XLEN+1)
  val zeroext = ZeroExt(_: UInt, XLEN+1)
  val mulInputFuncTable = List(
    MDUOpType.mul    -> (zeroext, zeroext),
    MDUOpType.mulh   -> (signext, signext),
    MDUOpType.mulhsu -> (signext, zeroext),
    MDUOpType.mulhu  -> (zeroext, zeroext)
  )
  mul.io.in.bits(0) := LookupTree(func(1,0), mulInputFuncTable.map(p => (p._1(1,0), p._2._1(src1))))
  mul.io.in.bits(1) := LookupTree(func(1,0), mulInputFuncTable.map(p => (p._1(1,0), p._2._2(src2))))

  val divInputFunc = (x: UInt) => Mux(isW, Mux(isDivSign, SignExt(x(31,0), XLEN), ZeroExt(x(31,0), XLEN)), x)
  div.io.in.bits(0) := divInputFunc(src1)
  div.io.in.bits(1) := divInputFunc(src2)

  mul.io.in.valid := io.in.valid && !isDiv
  div.io.in.valid := io.in.valid && isDiv

  val mulRes = Mux(func(1,0) === MDUOpType.mul(1,0), mul.io.out.bits(XLEN-1,0), mul.io.out.bits(2*XLEN-1,XLEN))
  val divRes = Mux(func(1) /* rem */, div.io.out.bits(2*XLEN-1,XLEN), div.io.out.bits(XLEN-1,0))
  val res = Mux(isDiv, divRes, mulRes)
  io.out.bits := Mux(isW, SignExt(res(31,0),XLEN), res)

  val isDivReg = Mux(io.in.fire(), isDiv, RegNext(isDiv, false.B))
  io.in.ready := Mux(isDiv, div.io.in.ready, mul.io.in.ready)
  io.out.valid := Mux(isDivReg, div.io.out.valid, mul.io.out.valid)

  BoringUtils.addSource(mul.io.out.fire(), "perfCntCondMmulInstr")
}
