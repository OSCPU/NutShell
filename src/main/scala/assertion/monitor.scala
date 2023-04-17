package assertion

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.internal.naming._

import nutcore._
import bus.axi4._

class DecoupledMon[T <: Data](gen: T) extends Bundle{
    override def cloneType = new DecoupledMon(gen).asInstanceOf[this.type]
    val valid = Input(Bool())
    val ready = Input(Bool())
    val bits = Input(gen) 
}

object DecoupledMon {
    def apply[T<:Data](gen:T) = new DecoupledMon(gen)
}

class AXI4Monitor(val dataBits: Int = AXI4Parameters.dataBits, val idBits: Int = AXI4Parameters.idBits) extends Bundle {
  val aw = DecoupledMon(Flipped(new AXI4BundleA(idBits)))
  val w  = DecoupledMon(Flipped(new AXI4BundleW(dataBits)))
  val b  = DecoupledMon(Flipped(new AXI4BundleB(idBits)))
  val ar = DecoupledMon(Flipped(new AXI4BundleA(idBits)))
  val r  = DecoupledMon(Flipped(new AXI4BundleR(dataBits, idBits)))
}