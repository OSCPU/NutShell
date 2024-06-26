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
import top.Settings

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

class SRAM64x128Verilog extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val CLK = Input(Clock())
    val CEB = Input(Bool())
    val A = Input(UInt(6.W))
    val WEB = Input(Bool())
    val D = Input(UInt(128.W))
    val BWEB = Input(UInt(128.W))
    val Q = Output(UInt(128.W))
  })

  if (!Settings.get("ASIC")) {
    setInline("SRAM64x128Verilog.v",
      s"""
        |module SRAM64x128Verilog (
        |  input CLK,
        |  input CEB,
        |  input [5:0] A,
        |  input WEB,
        |  input [127:0] D,
        |  input [127:0] BWEB,
        |  output reg [127:0] Q
        |);
        |  wire ce = ~CEB;
        |  wire we = ~WEB;
        |  wire [127:0] wmask = ~BWEB;
        |
        |  reg  [127:0] mem [0:63];
        |  always@(posedge CLK) begin
        |    if (ce & we) begin
        |      mem[A] <= (D & wmask) | (mem[A] & ~wmask);
        |    end
        |    Q <= (ce & !we) ? mem[A] : {4{$$random}};
        |  end
        |endmodule
       """.stripMargin)
  }
}

class SRAM64x128 extends Module {
  val io = IO(new Bundle {
    val rdata = Output(UInt(128.W))
    val addr = Input(UInt(6.W))
    val wdata = Input(UInt(128.W))
    val en = Input(Bool())
    val we = Input(Bool())
    val wmask = Input(UInt(128.W))
  })

  val sram = Module(new SRAM64x128Verilog)
  sram.io.CLK := clock
  sram.io.CEB := ~io.en
  sram.io.A := io.addr
  sram.io.WEB := ~io.we
  sram.io.BWEB := ~io.wmask
  sram.io.D := io.wdata
  io.rdata := sram.io.Q
}

class SRAMTemplate[T <: Data](gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false, holdRead: Boolean = false, singlePort: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  val isASIC = true

  val wordType = UInt(gen.getWidth.W)
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

  val wSetIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdataword = Mux(resetState, 0.U.asTypeOf(wordType), io.w.req.bits.data.asUInt)
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))
  val wdata = VecInit(Seq.fill(way)(wdataword))

  val rSetIdx = io.r.req.bits.setIdx
  val rdata = if (isASIC) {
    require(set % 64 == 0)
    val nrWords = set
    val wordBits = way * wordType.getWidth
    val sramX = (wordBits + 127) / 128  // the number of SRAM in the X-axis
    val sramY = (nrWords + 63) / 64     // the number of SRAM in the Y-axis
    val array = List.fill(sramY)(List.fill(sramX)(Module(new SRAM64x128)))
    val setIdx = Mux(wen, wSetIdx, rSetIdx)
    val yIdx = setIdx(6 + log2Up(sramY) - 1, 6)
    val yIdxReg = RegNext(yIdx)

    val rdataWord = array.zipWithIndex.map{ case (word, i) => {
      word.map(sram => {
        sram.io.en := (i.U === yIdx) && (realRen || wen)
        sram.io.we := wen
        sram.io.addr := setIdx(5, 0)
      })
      word.zip(wdata.asUInt.asTypeOf(Vec(sramX, UInt(128.W)))).map{ case (sram, wdata) => sram.io.wdata := wdata }
      word.zip(Cat(waymask.asBools.reverse.map(Fill(wordType.getWidth, _))).asTypeOf(Vec(sramX, UInt(128.W)))).map { case (sram, wmask) => sram.io.wmask := wmask }
      Cat(word.reverse.map(_.io.rdata & Fill(128, i.U === yIdxReg)))
    }}.reduce(_ | _)
    rdataWord.asTypeOf(Vec(way, wordType))
  } else {
    val array = SyncReadMem(set, Vec(way, wordType))
    when (wen) { array.write(wSetIdx, wdata, waymask.asBools) }
    array.read(rSetIdx, realRen)
  }

  val rdataHold = (if (holdRead) HoldUnless(rdata, RegNext(realRen)) else rdata).map(_.asTypeOf(gen))
  io.r.resp.data := VecInit(rdataHold)

  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B

  Debug(false) {
    when (wen) {
      printf("%d: SRAMTemplate: write %x to idx = %d\n", GTimer(), wdata.asUInt, wSetIdx)
    }
    when (RegNext(realRen)) {
      printf("%d: SRAMTemplate: read %x at idx = %d\n", GTimer(), VecInit(rdataHold).asUInt, RegNext(rSetIdx))
    }
  }
}

class SRAMTemplateWithArbiter[T <: Data](nRead: Int, gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(Vec(nRead, new SRAMReadBus(gen, set, way)))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  val ram = Module(new SRAMTemplate(gen, set, way, shouldReset, holdRead = false, singlePort = true))
  ram.io.w <> io.w

  val readArb = Module(new Arbiter(chiselTypeOf(io.r(0).req.bits), nRead))
  readArb.io.in <> io.r.map(_.req)
  ram.io.r.req <> readArb.io.out

  // latch read results
  io.r.map{ case r => {
    r.resp.data := HoldUnless(ram.io.r.resp.data, RegNext(r.req.fire))
  }}
}
