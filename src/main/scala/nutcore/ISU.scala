/**************************************************************************************
* Copyright (c) 2020 Institute of Computing Technology, CAS
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

// Sequential Inst Issue Unit 
class ISU(implicit val p: NutCoreConfig) extends NutCoreModule with HasRegFileParameter {
  val io = IO(new Bundle {
    val in = Vec(2, Flipped(Decoupled(new DecodeIO)))
    val out = Decoupled(Vec(2, new DecodeIO))
    val wb = Vec(2, Flipped(new WriteBackIO))
    val forward = Vec(2, Flipped(new ForwardIO))
    val flush = Input(Bool())
  })

  io.out.bits := DontCare
  val rfSrc1 = io.in(0).bits.ctrl.rfSrc1
  val rfSrc2 = io.in(0).bits.ctrl.rfSrc2
  val rfSrc3 = io.in(1).bits.ctrl.rfSrc1
  val rfSrc4 = io.in(1).bits.ctrl.rfSrc2
  val rfDest1 = io.in(0).bits.ctrl.rfDest
  val rfDest2 = io.in(1).bits.ctrl.rfDest

  def isDepend(rfSrc: UInt, rfDest: UInt, wen: Bool): Bool = (rfSrc =/= 0.U) && (rfSrc === rfDest) && wen
  def isDepend2(rfSrc: UInt, rfDest1: UInt, wen1: Bool, rfDest2: UInt, wen2: Bool): Bool = (rfSrc =/= 0.U) && ((rfSrc === rfDest1) && wen1 || (rfSrc === rfDest2) && wen2)

  val enablePipeline2 = EnableSuperScalarExec.B

  val forwardRfWen = List(
    io.forward(0).wb.rfWen && io.forward(0).valid,
    io.forward(1).wb.rfWen && io.forward(1).valid
  )
  val dontForward1 = (io.forward(0).fuType =/= FuType.alu) && (io.forward(0).fuType =/= FuType.lsu)
  val dontForward2 = (io.forward(0).fuType =/= FuType.alu) && (io.forward(0).fuType =/= FuType.lsu)
  val src3DependIS = isDepend(rfSrc3, rfDest1, io.in(0).bits.ctrl.rfWen)
  val src4DependIS = isDepend(rfSrc4, rfDest1, io.in(0).bits.ctrl.rfWen)
  val src1DependEX = isDepend2(rfSrc1, io.forward(0).wb.rfDest, forwardRfWen(0), io.forward(1).wb.rfDest, forwardRfWen(1))
  val src2DependEX = isDepend2(rfSrc2, io.forward(0).wb.rfDest, forwardRfWen(0), io.forward(1).wb.rfDest, forwardRfWen(1))
  val src3DependEX = isDepend2(rfSrc3, io.forward(0).wb.rfDest, forwardRfWen(0), io.forward(1).wb.rfDest, forwardRfWen(1)) && enablePipeline2
  val src4DependEX = isDepend2(rfSrc4, io.forward(0).wb.rfDest, forwardRfWen(0), io.forward(1).wb.rfDest, forwardRfWen(1)) && enablePipeline2
  val src1DependWB = isDepend2(rfSrc1, io.wb(0).rfDest, io.wb(0).rfWen, io.wb(1).rfDest, io.wb(1).rfWen)
  val src2DependWB = isDepend2(rfSrc2, io.wb(0).rfDest, io.wb(0).rfWen, io.wb(1).rfDest, io.wb(1).rfWen)
  val src3DependWB = isDepend2(rfSrc3, io.wb(0).rfDest, io.wb(0).rfWen, io.wb(1).rfDest, io.wb(1).rfWen) && enablePipeline2
  val src4DependWB = isDepend2(rfSrc4, io.wb(0).rfDest, io.wb(0).rfWen, io.wb(1).rfDest, io.wb(1).rfWen) && enablePipeline2

  val src1ForwardNextCycle = src1DependEX && !dontForward1
  val src2ForwardNextCycle = src2DependEX && !dontForward1
  val src3ForwardNextCycle = src3DependEX && !dontForward2
  val src4ForwardNextCycle = src4DependEX && !dontForward2
  val src1Forward = src1DependWB && Mux(dontForward1, !src1DependEX, true.B)
  val src2Forward = src2DependWB && Mux(dontForward1, !src2DependEX, true.B)
  val src3Forward = src3DependWB && Mux(dontForward2, !src3DependEX, true.B)
  val src4Forward = src4DependWB && Mux(dontForward2, !src4DependEX, true.B)

  Debug()
  {
    printf("[ISU] src1DependEX: %x %x %x %x %x\n", rfSrc1, io.forward(0).wb.rfDest, forwardRfWen(0), io.forward(1).wb.rfDest, forwardRfWen(1))
    printf("[ISU] src1DependWB: %x %x %x %x %x\n", rfSrc1, io.wb(0).rfDest, io.wb(0).rfWen, io.wb(1).rfDest, io.wb(1).rfWen)
    printf("[ISU] ForwardControl: time %x DIS %x %x DEX %x %x %x %x DWB %x %x %x %x FNC %x %x %x %x F %x %x %x %x\n", GTimer(),
      src3DependIS,
      src4DependIS,
      src1DependEX,
      src2DependEX,
      src3DependEX,
      src4DependEX,
      src1DependWB,
      src2DependWB,
      src3DependWB,
      src4DependWB,
      src1ForwardNextCycle,
      src2ForwardNextCycle,
      src3ForwardNextCycle,
      src4ForwardNextCycle,
      src1Forward,
      src2Forward,
      src3Forward,
      src4Forward
    )
  }

  val out1_1ForwardDataEX = Mux(isDepend(rfSrc1, io.forward(1).wb.rfDest, forwardRfWen(1)), io.forward(1).wb.rfData, io.forward(0).wb.rfData)
  val out1_2ForwardDataEX = Mux(isDepend(rfSrc2, io.forward(1).wb.rfDest, forwardRfWen(1)), io.forward(1).wb.rfData, io.forward(0).wb.rfData)
  val out2_1ForwardDataEX = Mux(isDepend(rfSrc3, io.forward(1).wb.rfDest, forwardRfWen(1)), io.forward(1).wb.rfData, io.forward(0).wb.rfData)
  val out2_2ForwardDataEX = Mux(isDepend(rfSrc4, io.forward(1).wb.rfDest, forwardRfWen(1)), io.forward(1).wb.rfData, io.forward(0).wb.rfData)
  val out1_1ForwardDataWB = Mux(isDepend(rfSrc1, io.wb(1).rfDest, io.wb(1).rfWen), io.wb(1).rfData, io.wb(0).rfData)
  val out1_2ForwardDataWB = Mux(isDepend(rfSrc2, io.wb(1).rfDest, io.wb(1).rfWen), io.wb(1).rfData, io.wb(0).rfData)
  val out2_1ForwardDataWB = Mux(isDepend(rfSrc3, io.wb(1).rfDest, io.wb(1).rfWen), io.wb(1).rfData, io.wb(0).rfData)
  val out2_2ForwardDataWB = Mux(isDepend(rfSrc4, io.wb(1).rfDest, io.wb(1).rfWen), io.wb(1).rfData, io.wb(0).rfData)

  val sb = new ScoreBoard
  val src1Ready = !sb.isBusy(rfSrc1) || src1ForwardNextCycle || src1Forward
  val src2Ready = !sb.isBusy(rfSrc2) || src2ForwardNextCycle || src2Forward
  val src3Ready = (!sb.isBusy(rfSrc3) || src3ForwardNextCycle || src3Forward) && !src3DependIS && enablePipeline2
  val src4Ready = (!sb.isBusy(rfSrc4) || src4ForwardNextCycle || src4Forward) && !src4DependIS && enablePipeline2
  io.out.valid := io.in(0).valid && src1Ready && src2Ready

  def isBru(func: UInt) = func(4)
  val inst2IsALUInst = io.in(1).bits.ctrl.fuType === FuType.alu && !isBru(io.in(1).bits.ctrl.fuOpType) && io.in(1).valid
  if(EnableSuperScalarExec){
    // in simple sequential multi issue mode, only alu inst can go through the 2nd pipeline
    io.out.bits(1).pipeline2 := io.in(1).valid && src3Ready && src4Ready && inst2IsALUInst
    if(EnableOutOfOrderExec){
      io.out.bits(1).pipeline2 := io.in(1).valid
    }
  }else{
    io.out.bits(1).pipeline2 := false.B
  }


  val rf = new RegFile
  Debug()
  {
    printf("[ISU] ForwardDat: %x %x %x %x %x %x\n", out1_1ForwardDataEX, out1_1ForwardDataWB, rf.read(rfSrc1), out1_2ForwardDataEX, out1_2ForwardDataWB, rf.read(rfSrc2))
  }
  // out1
  io.out.bits(0).data.src1 := Mux1H(List(
    (io.in(0).bits.ctrl.src1Type === SrcType.pc) -> SignExt(io.in(0).bits.cf.pc, AddrBits),
    src1ForwardNextCycle -> out1_1ForwardDataEX, //io.forward.wb.rfData,
    (src1Forward && !src1ForwardNextCycle) -> out1_1ForwardDataWB, //io.wb.rfData,
    ((io.in(0).bits.ctrl.src1Type =/= SrcType.pc) && !src1ForwardNextCycle && !src1Forward) -> rf.read(rfSrc1)
  ))
  io.out.bits(0).data.src2 := Mux1H(List(
    (io.in(0).bits.ctrl.src2Type =/= SrcType.reg) -> io.in(0).bits.data.imm,
    src2ForwardNextCycle -> out1_2ForwardDataEX, //io.forward.wb.rfData,
    (src2Forward && !src2ForwardNextCycle) -> out1_2ForwardDataWB, //io.wb.rfData,
    ((io.in(0).bits.ctrl.src2Type === SrcType.reg) && !src2ForwardNextCycle && !src2Forward) -> rf.read(rfSrc2)
  ))
  io.out.bits(0).data.imm  := io.in(0).bits.data.imm

  io.out.bits(0).cf <> io.in(0).bits.cf
  io.out.bits(0).ctrl := io.in(0).bits.ctrl
  io.out.bits(0).ctrl.isSrc1Forward := src1ForwardNextCycle
  io.out.bits(0).ctrl.isSrc2Forward := src2ForwardNextCycle

  // out2
  if(EnableSuperScalarExec){
    io.out.bits(1).data.src1 := Mux1H(List(
      (io.in(1).bits.ctrl.src1Type === SrcType.pc) -> SignExt(io.in(1).bits.cf.pc, AddrBits),
      src3ForwardNextCycle -> out2_1ForwardDataEX,
      (src3Forward && !src3ForwardNextCycle) -> out2_1ForwardDataWB,
      ((io.in(1).bits.ctrl.src1Type =/= SrcType.pc) && !src3ForwardNextCycle && !src3Forward) -> rf.read(rfSrc3)
    ))
    io.out.bits(1).data.src2 := Mux1H(List(
      (io.in(1).bits.ctrl.src2Type =/= SrcType.reg) -> io.in(1).bits.data.imm,
      src4ForwardNextCycle -> out2_2ForwardDataEX,
      (src4Forward && !src4ForwardNextCycle) -> out2_2ForwardDataWB,
      ((io.in(1).bits.ctrl.src2Type === SrcType.reg) && !src4ForwardNextCycle && !src4Forward) -> rf.read(rfSrc4)
    ))
  }else{
    io.out.bits(1).data.src1 := DontCare
    io.out.bits(1).data.src2 := DontCare
  }

  io.out.bits(1).data.imm  := io.in(1).bits.data.imm
  io.out.bits(1).cf <> io.in(1).bits.cf
  io.out.bits(1).ctrl := io.in(1).bits.ctrl
  io.out.bits(1).ctrl.isSrc1Forward := src3ForwardNextCycle
  io.out.bits(1).ctrl.isSrc2Forward := src4ForwardNextCycle

  // retire: write rf
  when (io.wb(0).rfWen) { rf.write(io.wb(0).rfDest, io.wb(0).rfData) }
  when (io.wb(1).rfWen) { rf.write(io.wb(1).rfDest, io.wb(1).rfData) }

  val wbClearMask = 
    Mux(io.wb(0).rfWen && !isDepend2(io.wb(0).rfDest, io.forward(0).wb.rfDest, forwardRfWen(0), io.forward(1).wb.rfDest, forwardRfWen(1)), sb.mask(io.wb(0).rfDest), 0.U(NRReg.W)) |
    Mux(io.wb(1).rfWen && !isDepend2(io.wb(1).rfDest, io.forward(0).wb.rfDest, forwardRfWen(0), io.forward(1).wb.rfDest, forwardRfWen(1)), sb.mask(io.wb(1).rfDest), 0.U(NRReg.W))
  // val isuFireSetMask = Mux(io.out.fire(), sb.mask(rfDest), 0.U)
  val isuFireSetMask = 
    Mux(io.out.fire(), sb.mask(rfDest1), 0.U) |
    Mux(io.out.fire() && io.out.bits(1).pipeline2, sb.mask(rfDest2), 0.U)
  when (io.flush) { sb.update(0.U, Fill(NRReg, 1.U(1.W))) }
  .otherwise { sb.update(isuFireSetMask, wbClearMask) }

  io.in(0).ready := !io.in(0).valid || io.out.fire()
  if(EnableSuperScalarExec){
    io.in(1).ready := io.out.fire() && io.out.bits(1).pipeline2
  }else{
    io.in(1).ready := false.B
  }

  Debug(){
    when(io.out.fire()){printf("[ISU] issue1: pc %x npc %x instr %x src1 %x src2 %x imm %x\n", io.out.bits(0).cf.pc, io.out.bits(0).cf.pnpc, io.out.bits(0).cf.instr, io.out.bits(0).data.src1, io.out.bits(0).data.src2, io.out.bits(0).data.imm)}
    when(io.out.fire() && io.out.bits(1).pipeline2){printf("[ISU] issue2: pc %x npc %x instr %x src1 %x src2 %x imm %x\n", io.out.bits(1).cf.pc, io.out.bits(1).cf.pnpc, io.out.bits(1).cf.instr, io.out.bits(1).data.src1, io.out.bits(1).data.src2, io.out.bits(1).data.imm)}
  }

  // read after write
  BoringUtils.addSource(io.in(0).valid && !io.out.valid, "perfCntCondMrawStall")
  BoringUtils.addSource(io.out.valid && !io.out.fire(), "perfCntCondMexuBusy")
  BoringUtils.addSource(io.out.fire(), "perfCntCondISUIssue")
  // io.in(1).valid && src3Ready && src4Ready && io.in(1).bits.ctrl.fuType === FuType.alu && !isBru(io.in(1).bits.ctrl.fuOpType)
  BoringUtils.addSource(io.out.fire() && !io.out.bits(1).pipeline2, "perfCntCondISU1Issue")
  BoringUtils.addSource(io.out.fire() && io.out.bits(1).pipeline2, "perfCntCondISU2Issue")
  BoringUtils.addSource(io.out.fire() && (!src3Ready || !src4Ready) && inst2IsALUInst, "perfCntCondSrc2NotReady")
  BoringUtils.addSource(io.out.fire() && (src3DependIS || src4DependIS) && inst2IsALUInst, "perfCntCondDst2Conflict")
  BoringUtils.addSource(io.out.fire() && !inst2IsALUInst, "perfCntCondInst2NotALU")
  BoringUtils.addSource(io.out.fire() && !io.in(1).valid, "perfCntCondInst2NotReady")

  if (!p.FPGAPlatform) {
    BoringUtils.addSource(VecInit((0 to NRReg-1).map(i => rf.read(i.U))), "difftestRegs")
  }
}
