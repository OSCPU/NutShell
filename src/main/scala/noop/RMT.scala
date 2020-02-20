package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

trait HasRMTConst{
  val rmtReadWidth = 4
  val rmtWriteWidth = 2
  val rmtRetireWidth = 2
}

// Register File Map Table
// `arf` denotes arch regfile
// `prf` denotes phy regfile
class RMT extends NOOPModule with HasROBConst with HasRegFileParameter with HasRMTConst{
  val io = IO(new Bundle {
    // write
    val wen = Input(Vec(rmtWriteWidth, Bool()))
    val warf = Input(Vec(rmtWriteWidth, UInt(5.W)))
    val wprf = Input(Vec(rmtWriteWidth, UInt(prfAddrWidth.W)))
    // read
    val rarf = Input(Vec(rmtReadWidth, UInt(5.W)))
    val rprf = Output(Vec(rmtReadWidth, UInt(prfAddrWidth.W)))
    val rvalid = Output(Vec(rmtReadWidth, Bool()))
    val rcommited = Output(Vec(rmtReadWidth, Bool()))
    // commit
    val cen = Input(Vec(rmtRetireWidth, Bool()))
    val cprf = Input(Vec(rmtRetireWidth, UInt(5.W)))
    // retire
    val rten = Input(Vec(rmtRetireWidth, Bool()))
    val rtprf = Input(Vec(rmtRetireWidth, UInt(5.W)))
    
    val flush = Input(Bool())
  })

  val map = Mem(NRReg, UInt(log2Up(robSize).W))
  val valid = Vec(NRReg, RegInit(false.B))
  val commited = Vec(NRReg, RegInit(false.B))

  List.tabulate(rmtRetireWidth)(i => when(io.rten(i)){valid(io.rtprf(i)) := false.B})
  List.tabulate(rmtRetireWidth)(i => when(io.cen(i)){commited(io.cprf(i)) := true.B})

  when(io.flush){
    List.tabulate(NRReg)(i => valid(i) := false.B) // set valid to 0
  }.otherwise{
    List.tabulate(rmtWriteWidth)(i => when(io.wen(i)){
      map(io.warf(i)) := io.wprf(i)
      valid(io.warf(i)) := true.B
      commited(io.warf(i)) := false.B
    })
  }

  List.tabulate(rmtReadWidth)(i => {
    io.rprf(i) := map(io.rarf(i))
    io.rvalid(i) := valid(io.rarf(i))
    io.rcommited(i) := commited(io.rarf(i))
  })

}