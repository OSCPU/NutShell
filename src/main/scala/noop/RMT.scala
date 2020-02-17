package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

// RMT! RMT! RMT!

trait HasRMTConst{
  val rmtWidth = 2
}

// Register File Map Table
// `arf` denotes arch regfile
// `prf` denotes phy regfile
class RMT extends NOOPModule with HasROBConst with HasRegFileParameter with HasRMTConst{
  val io = IO(new Bundle {
    // write
    val wen = Input(Vec(rmtWidth, Bool()))
    val warf = Input(UInt(5.W))
    val wprf = Input(UInt(prfAddrWidth.W))
    // read
    val rarf = Input(Vec(rmtWidth, UInt(5.W)))
    val rprf = Output(Vec(rmtWidth, UInt(prfAddrWidth.W)))
    val flush = Input(Bool())
  })

  val map = Mem(NRReg, UInt(log2Up(robSize).W))
  val valid = Vec(NRReg, RegInit(false.B))

  when(io.flush){
    List.tabulate(NRReg)(i => valid(i) := false.B) // set valid to 0
  }.otherwise{
    List.tabulate(robWidth)(i => when(io.wen(i)){map(io.warf(i)) := io.wprf(i)})
  }

  List.tabulate(robWidth)(i => io.rprf(i) := map(io.rarf(i)))

}