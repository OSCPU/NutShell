package bus.memport

import chisel3._
import chisel3.util._

import nutshell.HasNutShellParameter

trait MemoryOpConstants
{
   val MT_X  = 0.asUInt(3.W)
   val MT_B  = 1.asUInt(3.W)
   val MT_H  = 2.asUInt(3.W)
   val MT_W  = 3.asUInt(3.W)
   val MT_D  = 4.asUInt(3.W)
   val MT_BU = 5.asUInt(3.W)
   val MT_HU = 6.asUInt(3.W)
   val MT_WU = 7.asUInt(3.W)

   val M_X   = "b0".asUInt(1.W)
   val M_XRD = "b0".asUInt(1.W) // int load
   val M_XWR = "b1".asUInt(1.W) // int store

   val DPORT = 0
   val IPORT = 1
}

object MemPortConsts extends MemoryOpConstants{}

class MemPortIo(val data_width: Int) extends Bundle 
{
   val req  = new DecoupledIO(new MemReq(data_width))
   val resp = Flipped(new DecoupledIO(new MemResp(data_width)))
}

class MemReq(val data_width: Int) extends Bundle
{
   val addr = Output(UInt(32.W)) //p(sodor_xprlen)
   val data = Output(UInt(data_width.W))
   val fcn  = Output(UInt(MemPortConsts.M_X.getWidth.W))  // memory function code
   val typ  = Output(UInt(MemPortConsts.MT_X.getWidth.W)) // memory type
}

class MemResp(val data_width: Int) extends Bundle
{
   val data = Output(UInt(data_width.W))
}