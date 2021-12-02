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

package bus.simplebus

import chisel3._
import chisel3.util._

import nutcore.HasNutCoreParameter
import utils._
import bus.axi4._
import bus.memport._

sealed abstract class SimpleBusBundle extends Bundle with HasNutCoreParameter

object SimpleBusCmd {
  // req
                                 //   hit    |    miss
  def read           = "b0000".U //  read    |   refill
  def write          = "b0001".U //  write   |   refill
  def readBurst      = "b0010".U //  read    |   refill
  def writeBurst     = "b0011".U //  write   |   refill
  def writeLast      = "b0111".U //  write   |   refill
  def probe          = "b1000".U //  read    | do nothing
  def prefetch       = "b0100".U //  read    |   refill

  // resp
  def readLast       = "b0110".U
  def writeResp      = "b0101".U
  def probeHit       = "b1100".U
  def probeMiss      = "b1000".U

  def apply() = UInt(4.W)
}

class SimpleBusReqBundle(val userBits: Int = 0, val addrBits: Int = 32, val idBits: Int = 0) extends SimpleBusBundle {
  val addr = Output(UInt(addrBits.W))
  val size = Output(UInt(3.W))
  val cmd = Output(SimpleBusCmd())
  val wmask = Output(UInt((DataBits / 8).W))
  val wdata = Output(UInt(DataBits.W))
  val user = if (userBits > 0) Some(Output(UInt(userBits.W))) else None
  val id = if (idBits > 0) Some(Output(UInt(idBits.W))) else None

  override def toPrintable: Printable = {
    p"addr = 0x${Hexadecimal(addr)}, cmd = ${cmd}, size = ${size}, " +
    p"wmask = 0x${Hexadecimal(wmask)}, wdata = 0x${Hexadecimal(wdata)}"
  }

  def apply(addr: UInt, cmd: UInt, size: UInt, wdata: UInt, wmask: UInt, user: UInt = 0.U, id: UInt = 0.U) {
    this.addr := addr
    this.cmd := cmd
    this.size := size
    this.wdata := wdata
    this.wmask := wmask
    this.user.map(_ := user)
    this.id.map(_ := id)
  }

  def isRead() = !cmd(0) && !cmd(3)
  def isWrite() = cmd(0)
  def isBurst() = cmd(1)
  def isReadBurst() = cmd === SimpleBusCmd.readBurst
  def isWriteSingle() = cmd === SimpleBusCmd.write
  def isWriteLast() = cmd === SimpleBusCmd.writeLast
  def isProbe() = cmd === SimpleBusCmd.probe
  def isPrefetch() = cmd === SimpleBusCmd.prefetch
}

class SimpleBusRespBundle(val userBits: Int = 0, val idBits: Int = 0) extends SimpleBusBundle {
  val cmd = Output(SimpleBusCmd())
  val rdata = Output(UInt(64.W))  // TODO: when frontend datapath support 32bit, set DataBits.W here
  val user = if (userBits > 0) Some(Output(UInt(userBits.W))) else None
  val id = if (idBits > 0) Some(Output(UInt(idBits.W))) else None

  override def toPrintable: Printable = p"rdata = ${Hexadecimal(rdata)}, cmd = ${cmd}"

  def isReadLast() = cmd === SimpleBusCmd.readLast
  def isProbeHit() = cmd === SimpleBusCmd.probeHit
  def isProbeMiss() = cmd === SimpleBusCmd.probeMiss
  def isWriteResp() = cmd === SimpleBusCmd.writeResp
  def isPrefetch() = cmd === SimpleBusCmd.prefetch
}

// Uncache
class SimpleBusUC(val userBits: Int = 0, val addrBits: Int = 32, val idBits: Int = 0) extends SimpleBusBundle {
  val req = Decoupled(new SimpleBusReqBundle(userBits, addrBits, idBits))
  val resp = Flipped(Decoupled(new SimpleBusRespBundle(userBits, idBits)))

  def isWrite() = req.valid && req.bits.isWrite()
  def isRead()  = req.valid && req.bits.isRead()
  def toAXI4Lite() = SimpleBus2AXI4Converter(this, new AXI4Lite, false)
  def toAXI4(isFromCache: Boolean = false) = SimpleBus2AXI4Converter(this, new AXI4, isFromCache)
  def toMemPort() = SimpleBus2MemPortConverter(this, new MemPortIo(32))

  def dump(name: String) = {
    when (req.fire) { printf(p"${GTimer()},[${name}] ${req.bits}\n") }
    when (resp.fire) { printf(p"${GTimer()},[${name}] ${resp.bits}\n") }
  }
}

class SimpleBusUCExpender(val userBits: Int, val userVal: UInt, val addrBits: Int = 32, idBits: Int = 0) extends Module {
  val io = IO(new Bundle{
    val in = Flipped(new SimpleBusUC())
    val out = new SimpleBusUC(userBits = userBits, idBits = idBits)
  })
  require(userBits > 0)
  io.out.req.valid := io.in.req.valid
  io.in.req.ready := io.out.req.ready
  io.out.req.bits.addr := io.in.req.bits.addr
  io.out.req.bits.size := io.in.req.bits.size 
  io.out.req.bits.cmd := io.in.req.bits.cmd 
  io.out.req.bits.wmask := io.in.req.bits.wmask 
  io.out.req.bits.wdata := io.in.req.bits.wdata 
  io.out.req.bits.user.get := userVal
  io.in.resp.valid := io.out.resp.valid
  io.out.resp.ready := io.in.resp.ready
  io.in.resp.bits.cmd := io.out.resp.bits.cmd
  io.in.resp.bits.rdata := io.out.resp.bits.rdata
  // DontCare := io.out.resp.bits.user.get
}

// Cache
class SimpleBusC(val userBits: Int = 0) extends SimpleBusBundle {
  val mem = new SimpleBusUC(userBits)
  val coh = Flipped(new SimpleBusUC(userBits))

  def memtoMemPort() = this.mem.toMemPort
}
