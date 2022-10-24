
package nutcore.mem.cache

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import nutcore._

import bus.simplebus._
import bus.axi4._
import chisel3.experimental.IO
import utils._
import top.Settings


// meta read
// check
class CacheStageCheck(implicit val cacheConfig: CacheConfig) extends CacheModule {
  class CacheStageCheckIO extends Bundle {
    val in = Flipped(Decoupled(new Stage1IO))
    val out = Decoupled(new Stage2IO)
    val metaReadResp = Flipped(Vec(Ways, new MetaBundle))
    val dataReadResp = Flipped(Vec(Ways, new DataBundle))
    val metaWriteBus = Input(CacheMetaArrayWriteBus())
    val dataWriteBus = Input(CacheDataArrayWriteBus())
  }
  val io = IO(new CacheStageCheckIO)

  val req = io.in.bits.req
  val addr = req.addr.asTypeOf(addrBundle)

  val isForwardMeta = io.in.valid && io.metaWriteBus.req.valid && io.metaWriteBus.req.bits.setIdx === getMetaIdx(req.addr)
  val isForwardMetaReg = RegInit(false.B)
  when (isForwardMeta) { isForwardMetaReg := true.B }
  when (io.in.fire() || !io.in.valid) { isForwardMetaReg := false.B }
  val forwardMetaReg = RegEnable(io.metaWriteBus.req.bits, isForwardMeta)

  val metaWay = Wire(Vec(Ways, chiselTypeOf(forwardMetaReg.data)))
  val pickForwardMeta = isForwardMetaReg || isForwardMeta
  val forwardMeta = Mux(isForwardMeta, io.metaWriteBus.req.bits, forwardMetaReg)
  val forwardWaymask = forwardMeta.waymask.getOrElse("1".U).asBools
  forwardWaymask.zipWithIndex.map { case (w, i) =>
    metaWay(i) := Mux(pickForwardMeta && w, forwardMeta.data, io.metaReadResp(i))
  }

  val hitVec = VecInit(metaWay.map(m => m.valid && (m.tag === addr.tag) && io.in.valid)).asUInt
  val victimWaymask = if (Ways > 1) (1.U << LFSR64()(log2Up(Ways)-1,0)) else "b1".U
   
  val invalidVec = VecInit(metaWay.map(m => !m.valid)).asUInt
  val hasInvalidWay = invalidVec.orR
  val refillInvalidWaymask = Mux(invalidVec >= 8.U, "b1000".U,
    Mux(invalidVec >= 4.U, "b0100".U,
    Mux(invalidVec >= 2.U, "b0010".U, "b0001".U)))
  
  // val waymask = Mux(io.out.bits.hit, hitVec, victimWaymask)
  val waymask = Mux(io.out.bits.hit, hitVec, Mux(hasInvalidWay, refillInvalidWaymask, victimWaymask))
  when(PopCount(waymask) > 1.U){
    metaWay.map(m => Debug("[ERROR] metaWay %x metat %x reqt %x\n", m.valid, m.tag, addr.tag))
    io.metaReadResp.map(m => Debug("[ERROR] metaReadResp %x metat %x reqt %x\n", m.valid, m.tag, addr.tag))
    Debug("[ERROR] forwardMetaReg isForwardMetaReg %x %x metat %x wm %b\n", isForwardMetaReg, forwardMetaReg.data.valid, forwardMetaReg.data.tag, forwardMetaReg.waymask.get)
    Debug("[ERROR] forwardMeta isForwardMeta %x %x metat %x wm %b\n", isForwardMeta, io.metaWriteBus.req.bits.data.valid, io.metaWriteBus.req.bits.data.tag, io.metaWriteBus.req.bits.waymask.get)
  }
  when(PopCount(waymask) > 1.U){Debug("[ERROR] hit %b wmask %b hitvec %b\n", io.out.bits.hit, forwardMeta.waymask.getOrElse("1".U), hitVec)}
  assert(!(io.in.valid && PopCount(waymask) > 1.U))

  io.out.bits.metas := metaWay
  io.out.bits.hit := io.in.valid && hitVec.orR
  io.out.bits.waymask := waymask
  io.out.bits.datas := io.dataReadResp
  io.out.bits.mmio := AddressSpace.isMMIO(req.addr)

  val isForwardData = io.in.valid && (io.dataWriteBus.req match { case r =>
    r.valid && r.bits.setIdx === getDataIdx(req.addr)
  })
  val isForwardDataReg = RegInit(false.B)
  when (isForwardData) { isForwardDataReg := true.B }
  when (io.in.fire() || !io.in.valid) { isForwardDataReg := false.B }
  val forwardDataReg = RegEnable(io.dataWriteBus.req.bits, isForwardData)
  io.out.bits.isForwardData := isForwardDataReg || isForwardData
  io.out.bits.forwardData := Mux(isForwardData, io.dataWriteBus.req.bits, forwardDataReg)

  io.out.bits.req <> req
  io.out.valid := io.in.valid
  io.in.ready := !io.in.valid || io.out.fire()

  Debug("[isFD:%d isFDreg:%d inFire:%d invalid:%d \n", isForwardData, isForwardDataReg, io.in.fire(), io.in.valid)
  Debug("[isFM:%d isFMreg:%d metawreq:%x widx:%x ridx:%x \n", isForwardMeta, isForwardMetaReg, io.metaWriteBus.req.valid, io.metaWriteBus.req.bits.setIdx, getMetaIdx(req.addr))
}