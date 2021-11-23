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

package utils

import chisel3._
import chisel3.util._

class S011HD1P_X32Y2D128 extends BlackBox {
  val io = IO(new Bundle {
    val Q = Output(UInt(128.W))
    val CLK = Input(Clock())
    val CEN = Input(Bool())
    val WEN = Input(Bool())
    val A = Input(UInt(6.W))
    val D = Input(UInt(128.W))
  })
}

class S011HD1P_X32Y2D128_BW extends BlackBox {
  val io = IO(new Bundle {
    val Q = Output(UInt(128.W))
    val CLK = Input(Clock())
    val CEN = Input(Bool())
    val WEN = Input(Bool())
    val BWEN = Input(UInt(128.W))
    val A = Input(UInt(6.W))
    val D = Input(UInt(128.W))
  })
}

class SRAMWrapper extends Module {
  val io = IO(new Bundle {
    val Q = Output(UInt(128.W))
    val CLK = Input(Clock())
    val CEN = Input(Bool())
    val WEN = Input(Bool())
    val BWEN = Input(UInt(128.W))
    val A = Input(UInt(7.W))
    val D = Input(UInt(128.W))
  })
  val sram_lo = Module(new S011HD1P_X32Y2D128_BW())
  val sram_hi = Module(new S011HD1P_X32Y2D128_BW())
  sram_lo.io.CLK := io.CLK
  sram_lo.io.CEN := io.A(6) || io.CEN
  sram_lo.io.WEN := io.WEN
  sram_lo.io.BWEN := io.BWEN
  sram_lo.io.A := io.A(5,0)
  sram_lo.io.D := io.D

  sram_hi.io.CLK := io.CLK
  sram_hi.io.CEN := !io.A(6) || io.CEN
  sram_hi.io.WEN := io.WEN
  sram_hi.io.BWEN := io.BWEN
  sram_hi.io.A := io.A(5,0)
  sram_hi.io.D := io.D

  io.Q := Mux(RegNext(io.A(6), false.B), sram_hi.io.Q, sram_lo.io.Q)
}

class SRAMBundleA(val set: Int) extends Bundle {
  val setIdx = Output(UInt(log2Up(set).W))

  def apply(setIdx: UInt) = {
    this.setIdx := setIdx
    this
  }
}

class SRAMBundleAW[T <: Data](private val gen: T, set: Int, val way: Int = 1) extends SRAMBundleA(set) {
  val data = Output(gen)
  val waymask = if (way > 1) Some(Output(UInt(way.W))) else None

  def apply(data: T, setIdx: UInt, waymask: UInt) = {
    super.apply(setIdx)
    this.data := data
    this.waymask.map(_ := waymask)
    this
  }
}

class SRAMBundleR[T <: Data](private val gen: T, val way: Int = 1) extends Bundle {
  val data = Output(Vec(way, gen))
}

class SRAMReadBus[T <: Data](private val gen: T, val set: Int, val way: Int = 1) extends Bundle {
  val req = Decoupled(new SRAMBundleA(set))
  val resp = Flipped(new SRAMBundleR(gen, way))

  def apply(valid: Bool, setIdx: UInt) = {
    this.req.bits.apply(setIdx)
    this.req.valid := valid
    this
  }
}

class SRAMWriteBus[T <: Data](private val gen: T, val set: Int, val way: Int = 1) extends Bundle {
  val req = Decoupled(new SRAMBundleAW(gen, set, way))

  def apply(valid: Bool, data: T, setIdx: UInt, waymask: UInt) = {
    this.req.bits.apply(data = data, setIdx = setIdx, waymask = waymask)
    this.req.valid := valid
    this
  }
}

class SRAMTemplate[T <: Data](gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false, holdRead: Boolean = false, singlePort: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  val wordType = UInt(gen.getWidth.W)
  val array = SyncReadMem(set, Vec(way, wordType))
  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetSet := _resetSet
  }

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
  val realRen = (if (singlePort) ren && !wen else ren)

  val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdataword = Mux(resetState, 0.U.asTypeOf(wordType), io.w.req.bits.data.asUInt)
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))
  val wdata = VecInit(Seq.fill(way)(wdataword))
  when (wen) { array.write(setIdx, wdata, waymask.asBools) }

  val rdata = (if (holdRead) ReadAndHold(array, io.r.req.bits.setIdx, realRen)
               else array.read(io.r.req.bits.setIdx, realRen)).map(_.asTypeOf(gen))
  io.r.resp.data := VecInit(rdata)

  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B

  Debug(false) {
    when (wen) {
      printf("%d: SRAMTemplate: write %x to idx = %d\n", GTimer(), wdata.asUInt, setIdx)
    }
    when (RegNext(realRen)) {
      printf("%d: SRAMTemplate: read %x at idx = %d\n", GTimer(), VecInit(rdata).asUInt, RegNext(io.r.req.bits.setIdx))
    }
  }
}

class BTBSRAMTemplate[T <: Data](gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false, holdRead: Boolean = false, singlePort: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  require(holdRead)
  val wordType = UInt(gen.getWidth.W)
  // val array = SyncReadMem(set, Vec(way, wordType))
  val sram = Module(new S011HD1P_X32Y2D128())

  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetSet := _resetSet
  }

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
  val realRen = (if (singlePort) ren && !wen else ren)

  val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdataword = Mux(resetState, 0.U.asTypeOf(wordType), io.w.req.bits.data.asUInt)
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))
  val wdata = VecInit(Seq.fill(way)(wdataword))
  // when (wen) { array.write(setIdx, wdata, waymask.asBools) }

  sram.io.CLK := clock
  sram.io.A := Mux(wen, setIdx, io.r.req.bits.setIdx)
  sram.io.CEN := ~(wen || realRen)
  sram.io.WEN := ~wen
  sram.io.D := Cat(0.U((128-gen.getWidth).W), Cat(wdata))

  val rdata = HoldUnless(sram.io.Q, RegNext(realRen, false.B))
  io.r.resp.data := rdata(gen.getWidth-1, 0).asTypeOf(io.r.resp.data)

  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B

  Debug(false) {
    when (wen) {
      printf("%d: SRAMTemplate: write %x to idx = %d\n", GTimer(), wdata.asUInt, setIdx)
    }
    when (RegNext(realRen)) {
      printf("%d: SRAMTemplate: read %x at idx = %d\n", GTimer(), VecInit(rdata).asUInt, RegNext(io.r.req.bits.setIdx))
    }
  }
}

class MetaSRAMTemplate[T <: Data](gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false, holdRead: Boolean = false, singlePort: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })
  require(!holdRead)
  val wordType = UInt(gen.getWidth.W)
  // val array = SyncReadMem(set, Vec(way, wordType))
  val sram = Module(new S011HD1P_X32Y2D128_BW())
  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetSet := _resetSet
  }

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
  val realRen = (if (singlePort) ren && !wen else ren)

  val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdataword = Mux(resetState, 0.U.asTypeOf(wordType), io.w.req.bits.data.asUInt)
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))
  val wdata = VecInit(Seq.fill(way)(Cat(0.U((128/way-gen.getWidth).W), wdataword)))
  // when (wen) { array.write(setIdx, wdata, waymask.asBools) }

  sram.io.CLK := clock
  sram.io.A := Mux(wen, setIdx, io.r.req.bits.setIdx)
  sram.io.CEN := ~(wen || realRen)
  sram.io.WEN := ~wen
  sram.io.BWEN := ~FillInterleaved(128/way, waymask)
  sram.io.D := Cat(wdata)

  val rdata = sram.io.Q.asTypeOf(Vec(way, UInt((128/way).W))).map(_.asTypeOf(gen))
  io.r.resp.data := VecInit(rdata)

  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B

  Debug(false) {
    when (wen) {
      printf("%d: SRAMTemplate: write %x to idx = %d\n", GTimer(), wdata.asUInt, setIdx)
      printf("%d: SRAMTemplate: BWEN: %x D: %x\n", GTimer(), sram.io.BWEN, sram.io.D)
    }
    when (RegNext(realRen)) {
      printf("%d: SRAMTemplate: read %x at idx = %d\n", GTimer(), VecInit(rdata).asUInt, RegNext(io.r.req.bits.setIdx))
      printf("%d: SRAMTemplate: Q %x at idx = %d\n", GTimer(), sram.io.Q, RegNext(io.r.req.bits.setIdx))
    }
  }
}

// The code below is quite dirtttttty!
class DataSRAMTemplate[T <: Data](gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false, holdRead: Boolean = false, singlePort: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  require(!holdRead)
  require(way == 4)
  require(gen.getWidth == 64)
  val wordType = UInt(gen.getWidth.W)
  // val array = SyncReadMem(set, Vec(way, wordType))
  val sram = Seq.fill(2)(Module(new SRAMWrapper()))  // 4 * 64 => 2 * 128
  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetSet := _resetSet
  }

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
  val realRen = (if (singlePort) ren && !wen else ren)

  val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdataword = Mux(resetState, 0.U.asTypeOf(wordType), io.w.req.bits.data.asUInt)
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))
  val wdata = VecInit(Seq.fill(2)(wdataword))
  // when (wen) { array.write(setIdx, wdata, waymask.asBools) }

  sram.map(_.io.CLK := clock)
  sram.map(_.io.A := Mux(wen, setIdx, io.r.req.bits.setIdx))
  sram.zipWithIndex.map{
    case (s, i) => s.io.CEN := ~(wen || realRen)
  }
  sram.zipWithIndex.map{
    case (s, i) => s.io.WEN := ~(wen && OHToUInt(waymask)(1) === i.U)
  }
  sram.zipWithIndex.map{
    case (s, i) => s.io.BWEN := ~(FillInterleaved(64, waymask(3,2) | waymask(1,0)))
  }
  sram.map(_.io.D := Cat(wdata))

  val rdata = Cat(sram.map(_.io.Q).reverse).asTypeOf(Vec(4, UInt(gen.getWidth.W))).map(_.asTypeOf(gen))
  io.r.resp.data := VecInit(rdata)

  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B

  Debug(false) {
    when (wen) {
      printf("%d: SRAMTemplate: write %x to idx = %d way = %x\n", GTimer(), wdata.asUInt, setIdx, OHToUInt(waymask))
      printf("%d: SRAMTemplate 0: WEN: %x BWEN: %x D: %x\n", GTimer(), sram(0).io.WEN, sram(0).io.BWEN, sram(0).io.D)
      printf("%d: SRAMTemplate 1: WEN: %x BWEN: %x D: %x\n", GTimer(), sram(1).io.WEN, sram(1).io.BWEN, sram(1).io.D)
    }
    when (RegNext(realRen)) {
      printf("%d: SRAMTemplate: read %x at idx = %d way = %x\n", GTimer(), VecInit(rdata).asUInt, RegNext(io.r.req.bits.setIdx), RegNext(OHToUInt(waymask)))
      printf("%d: SRAMTemplate 0: Q %x at idx = %d\n", GTimer(), RegNext(sram(0).io.Q), RegNext(io.r.req.bits.setIdx))
      printf("%d: SRAMTemplate 1: Q %x at idx = %d\n", GTimer(), RegNext(sram(1).io.Q), RegNext(io.r.req.bits.setIdx))
    }
  }
}

class SRAMTemplateWithArbiter[T <: Data](nRead: Int, gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false, isData: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(Vec(nRead, new SRAMReadBus(gen, set, way)))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  // do not replace sram for meta
  val ram = if (isData) Module(new DataSRAMTemplate(gen, set, way, shouldReset, holdRead = false, singlePort = true))
            else Module(new SRAMTemplate(gen, set, way, shouldReset, holdRead = false, singlePort = true))
  println("len: %d, set: %d, way: %d\n", gen.getWidth.W, set, way)
  ram.io.w <> io.w

  val readArb = Module(new Arbiter(chiselTypeOf(io.r(0).req.bits), nRead))
  readArb.io.in <> io.r.map(_.req)
  ram.io.r.req <> readArb.io.out

  // latch read results
  io.r.map{ case r => {
    r.resp.data := HoldUnless(ram.io.r.resp.data, RegNext(r.req.fire(), false.B))
  }}
}