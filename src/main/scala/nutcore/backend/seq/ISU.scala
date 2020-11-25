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

// Sequential Inst Issue Unit 
class ISU(implicit val p: NutCoreConfig) extends NutCoreModule with HasRegFileParameter {
  val io = IO(new Bundle {
    val in = Vec(2, Flipped(Decoupled(new DecodeIO))) // make in-order backend compatible with high performance frontend 
    val out = Decoupled(new DecodeIO)
    val wb = Flipped(new WriteBackIO)
    val forward = Flipped(new ForwardIO)
    val flush = Input(Bool())
  })

  io.out.bits := DontCare
  val rfSrc1 = io.in(0).bits.ctrl.rfSrc1
  val rfSrc2 = io.in(0).bits.ctrl.rfSrc2
  val rfDest1 = io.in(0).bits.ctrl.rfDest

  def isDepend(rfSrc: UInt, rfDest: UInt, wen: Bool): Bool = (rfSrc =/= 0.U) && (rfSrc === rfDest) && wen

  val forwardRfWen = io.forward.wb.rfWen && io.forward.valid
  val dontForward1 = (io.forward.fuType =/= FuType.alu) && (io.forward.fuType =/= FuType.lsu)
  val src1DependEX = isDepend(rfSrc1, io.forward.wb.rfDest, forwardRfWen)
  val src2DependEX = isDepend(rfSrc2, io.forward.wb.rfDest, forwardRfWen)
  val src1DependWB = isDepend(rfSrc1, io.wb.rfDest, io.wb.rfWen)
  val src2DependWB = isDepend(rfSrc2, io.wb.rfDest, io.wb.rfWen)

  val src1ForwardNextCycle = src1DependEX && !dontForward1
  val src2ForwardNextCycle = src2DependEX && !dontForward1
  val src1Forward = src1DependWB && Mux(dontForward1, !src1DependEX, true.B)
  val src2Forward = src2DependWB && Mux(dontForward1, !src2DependEX, true.B)

  val sb = new ScoreBoard
  val src1Ready = !sb.isBusy(rfSrc1) || src1ForwardNextCycle || src1Forward
  val src2Ready = !sb.isBusy(rfSrc2) || src2ForwardNextCycle || src2Forward

  val rf = new RegFile

  // fp

  val difftestFpr = WireInit(VecInit(Seq.fill(32)(0.U(XLEN.W))))
  val (fpCanIssue, fpDataVec) =  if(HasFPU){

    val fsb = new ScoreBoard(hasZero = false)
    val fpr = new RegFile(hasZero = false)

    difftestFpr.zipWithIndex.foreach{
      case (d, i) =>
        d := fpr.read(i.U)
    }

    val wb = io.wb
    val forward = io.forward
    val in = io.in(0)

    val forwardFpWen = forward.valid && forward.wb.fpWen
    when(wb.fpWen){
      fpr.write(wb.rfDest, wb.rfData)
    }

    val fsbClearMask = Mux(
      wb.fpWen && !isDepend(wb.rfDest, forward.wb.rfDest, forwardFpWen),
      fsb.mask(wb.rfDest),
      0.U(NRReg.W)
    )
    val fsbSetMask = Mux(io.out.fire() && in.bits.ctrl.fpWen, fsb.mask(rfDest1), 0.U)

    when (io.flush) {
      fsb.update(0.U, Fill(NRReg, 1.U(1.W)))
    }.otherwise {
      fsb.update(fsbSetMask, fsbClearMask)
    }

    val (fpSrc1,fpSrc2,fpSrc3) = (rfSrc1, rfSrc2, in.bits.ctrl.rfSrc3)
    val srcTuple = Seq(
      fpSrc1, fpSrc2, fpSrc3
    ).zip(Seq(
      in.bits.ctrl.src1Type,
      in.bits.ctrl.src2Type,
      in.bits.ctrl.src3Type
    ))

    val dataVec = Array.fill(3)(Wire(UInt(XLEN.W)))

    val rdyVec = srcTuple.zipWithIndex.map({
      case ((src, t), i) =>
        val dependEX = isDepend(src, forward.wb.rfDest, forwardFpWen)
        val dependWB = isDepend(src, wb.rfDest, wb.fpWen)
        val forwardEX = dependEX && !dontForward1
        val forwardWB = dependWB && Mux(dontForward1, !dependEX, true.B)
        dataVec(i) := MuxCase(fpr.read(src),Seq(
          forwardEX -> forward.wb.rfData,
          forwardWB -> wb.rfData
        ))
        Debug(){
          when(io.in(0).valid && t===SrcType.fp){
            printf(
              p"i:$i forwardEx: $forwardEX data: ${Hexadecimal(forward.wb.rfData)} " +
                p"forwardWB: $forwardWB data:${Hexadecimal(wb.rfData)} " +
                p"rfRead: ${Hexadecimal(fpr.read(src))} src:$src busy:${fsb.busy(src)}\n"
            )
          }
        }
        (!fsb.busy(src) || forwardEX || forwardWB) || (t =/= SrcType.fp)
    })

    val canFire = rdyVec.reduce(_ && _)

    (canFire, dataVec)
  } else (true.B, Array.fill(3)(0.U))

  io.out.valid := io.in(0).valid && src1Ready && src2Ready && fpCanIssue

  // out1
  io.out.bits.data.src1 := Mux1H(List(
    (io.in(0).bits.ctrl.src1Type === SrcType.fp) -> fpDataVec(0),
    (io.in(0).bits.ctrl.src1Type === SrcType.pc) -> SignExt(io.in(0).bits.cf.pc, AddrBits),
    src1ForwardNextCycle -> io.forward.wb.rfData, //io.forward.wb.rfData,
    (src1Forward && !src1ForwardNextCycle) -> io.wb.rfData, //io.wb.rfData,
    ((io.in(0).bits.ctrl.src1Type =/= SrcType.pc) && !src1ForwardNextCycle && !src1Forward) -> rf.read(rfSrc1)
  ))
  io.out.bits.data.src2 := Mux1H(List(
    (io.in(0).bits.ctrl.src2Type === SrcType.fp) -> fpDataVec(1),
    (io.in(0).bits.ctrl.src2Type =/= SrcType.reg) -> io.in(0).bits.data.imm,
    src2ForwardNextCycle -> io.forward.wb.rfData, //io.forward.wb.rfData,
    (src2Forward && !src2ForwardNextCycle) -> io.wb.rfData, //io.wb.rfData,
    ((io.in(0).bits.ctrl.src2Type === SrcType.reg) && !src2ForwardNextCycle && !src2Forward) -> rf.read(rfSrc2)
  ))
  io.out.bits.data.imm  := Mux(
    io.in(0).bits.ctrl.src3Type===SrcType.fp,
    fpDataVec(2),
    io.in(0).bits.data.imm
  )

  io.out.bits.cf <> io.in(0).bits.cf
  io.out.bits.ctrl := io.in(0).bits.ctrl
  io.out.bits.ctrl.isSrc1Forward := src1ForwardNextCycle
  io.out.bits.ctrl.isSrc2Forward := src2ForwardNextCycle

  // retire: write rf
  when (io.wb.rfWen) { rf.write(io.wb.rfDest, io.wb.rfData) }

  val wbClearMask = Mux(io.wb.rfWen && !isDepend(io.wb.rfDest, io.forward.wb.rfDest, forwardRfWen), sb.mask(io.wb.rfDest), 0.U(NRReg.W))
  // val isuFireSetMask = Mux(io.out.fire(), sb.mask(rfDest), 0.U)
  val isuFireSetMask = Mux(io.out.fire(), sb.mask(rfDest1), 0.U)
  when (io.flush) { sb.update(0.U, Fill(NRReg, 1.U(1.W))) }
  .otherwise { sb.update(isuFireSetMask, wbClearMask) }

  io.in(0).ready := !io.in(0).valid || io.out.fire()
  io.in(1).ready := false.B

  Debug(io.out.fire(), "issue: pc %x npc %x instr %x src1 %x src2 %x imm %x\n", io.out.bits.cf.pc, io.out.bits.cf.pnpc, io.out.bits.cf.instr, io.out.bits.data.src1, io.out.bits.data.src2, io.out.bits.data.imm)

  // read after write
  BoringUtils.addSource(io.in(0).valid && !io.out.valid, "perfCntCondMrawStall")
  BoringUtils.addSource(io.out.valid && !io.out.fire(), "perfCntCondMexuBusy")
  BoringUtils.addSource(io.out.fire(), "perfCntCondISUIssue")

  if (!p.FPGAPlatform) {
    val difftestGpr = (0 until NRReg).map(i => rf.read(i.U))
    val difftestRegs = VecInit(difftestGpr ++ difftestFpr)
    BoringUtils.addSource(difftestRegs, "difftestRegs")
  }
}
