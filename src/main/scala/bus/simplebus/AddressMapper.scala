package bus.simplebus

import chisel3._
import chisel3.util._

import utils._
import nutshell.HasNOOPParameter

class SimpleBusAddressMapper(map: (Int, Long)) extends Module with HasNOOPParameter {
  val io = IO(new Bundle {
    val in = Flipped(new SimpleBusUC)
    val out = new SimpleBusUC
  })

  io.out <> io.in
  val (regionBits, rebase) = (map._1, map._2.U(PAddrBits.W))
  if (regionBits != 0) {
    io.out.req.bits.addr := Cat(rebase(PAddrBits-1, regionBits), io.in.req.bits.addr(regionBits-1, 0))
  }
}
