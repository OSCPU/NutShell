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
import bus.simplebus._
import bus.axi4._
import chisel3.experimental.IO
import utils._
import top.Settings

case class CacheConfig (
  ro: Boolean = false,
  name: String = "cache",
  userBits: Int = 0,
  idBits: Int = 0,
  cacheLevel: Int = 1,

  totalSize: Int = 32, // Kbytes
  ways: Int = 4
)

sealed trait HasCacheConst {
  implicit val cacheConfig: CacheConfig

  val PAddrBits: Int
  val XLEN: Int

  val cacheName = cacheConfig.name
  val userBits = cacheConfig.userBits
  val idBits = cacheConfig.idBits

  val ro = cacheConfig.ro
  val hasCoh = !ro
  val hasCohInt = (if (hasCoh) 1 else 0)
  val hasPrefetch = cacheName == "l2cache"
	
  val cacheLevel = cacheConfig.cacheLevel
  val TotalSize = cacheConfig.totalSize
  val Ways = cacheConfig.ways
  val LineSize = XLEN // byte
  val LineBeats = LineSize / 8 //DATA WIDTH 64
  val Sets = TotalSize * 1024 / LineSize / Ways
  val OffsetBits = log2Up(LineSize)
  val IndexBits = log2Up(Sets)
  val WordIndexBits = log2Up(LineBeats)
  val TagBits = PAddrBits - OffsetBits - IndexBits

  val debug = false

  def addrBundle = new Bundle {
    val tag = UInt(TagBits.W)
    val index = UInt(IndexBits.W)
    val wordIndex = UInt(WordIndexBits.W)
    val byteOffset = UInt((if (XLEN == 64) 3 else 2).W)
  }

  def CacheMetaArrayReadBus() = new SRAMReadBus(new MetaBundle, set = Sets, way = Ways)
  def CacheDataArrayReadBus() = new SRAMReadBus(new DataBundle, set = Sets * LineBeats, way = Ways)
  def CacheMetaArrayWriteBus() = new SRAMWriteBus(new MetaBundle, set = Sets, way = Ways)
  def CacheDataArrayWriteBus() = new SRAMWriteBus(new DataBundle, set = Sets * LineBeats, way = Ways)

  def getMetaIdx(addr: UInt) = addr.asTypeOf(addrBundle).index
  def getDataIdx(addr: UInt) = Cat(addr.asTypeOf(addrBundle).index, addr.asTypeOf(addrBundle).wordIndex)

  def isSameWord(a1: UInt, a2: UInt) = ((a1 >> 2) === (a2 >> 2))
  def isSetConflict(a1: UInt, a2: UInt) = (a1.asTypeOf(addrBundle).index === a2.asTypeOf(addrBundle).index)
}

sealed abstract class CacheBundle(implicit cacheConfig: CacheConfig) extends Bundle with HasNutCoreParameter with HasCacheConst
sealed abstract class CacheModule(implicit cacheConfig: CacheConfig) extends Module with HasNutCoreParameter with HasCacheConst with HasNutCoreLog

sealed class MetaBundle(implicit val cacheConfig: CacheConfig) extends CacheBundle {
  val tag = Output(UInt(TagBits.W))
  val valid = Output(Bool())
  val dirty = Output(Bool())

  def apply(tag: UInt, valid: Bool, dirty: Bool) = {
    this.tag := tag
    this.valid := valid
    this.dirty := dirty
    this
  }
}

sealed class DataBundle(implicit val cacheConfig: CacheConfig) extends CacheBundle {
  val data = Output(UInt(DataBits.W))

  def apply(data: UInt) = {
    this.data := data
    this
  }
}

sealed class Stage1IO(implicit val cacheConfig: CacheConfig) extends CacheBundle {
  val req = new SimpleBusReqBundle(userBits = userBits, idBits = idBits)
}

class CacheIO(implicit val cacheConfig: CacheConfig) extends Bundle with HasNutCoreParameter with HasCacheConst {
  val in = Flipped(new SimpleBusUC(userBits = userBits, idBits = idBits))
  val flush = Input(UInt(2.W))
  val out = new SimpleBusC
  val mmio = new SimpleBusUC
  val empty = Output(Bool())
}
trait HasCacheIO {
  implicit val cacheConfig: CacheConfig
  val io = IO(new CacheIO)
}

// meta read
sealed class CacheStage1(implicit val cacheConfig: CacheConfig) extends CacheModule {
  class CacheStage1IO extends Bundle {
    val in = Flipped(Decoupled(new SimpleBusReqBundle(userBits = userBits, idBits = idBits)))
    val out = Decoupled(new Stage1IO)
    val metaReadBus = CacheMetaArrayReadBus()
    val dataReadBus = CacheDataArrayReadBus()
  }
  val io = IO(new CacheStage1IO)

  if (ro) when (io.in.fire()) { assert(!io.in.bits.isWrite()) }
  Debug(io.in.fire(), "[L1$] cache stage1, addr in: %x, user: %x id: %x\n", io.in.bits.addr, io.in.bits.user.getOrElse(0.U), io.in.bits.id.getOrElse(0.U))

  // read meta array and data array
  val readBusValid = io.in.valid && io.out.ready
  io.metaReadBus.apply(valid = readBusValid, setIdx = getMetaIdx(io.in.bits.addr))
  io.dataReadBus.apply(valid = readBusValid, setIdx = getDataIdx(io.in.bits.addr))

  io.out.bits.req := io.in.bits
  io.out.valid := io.in.valid && io.metaReadBus.req.ready && io.dataReadBus.req.ready
  io.in.ready := (!io.in.valid || io.out.fire()) && io.metaReadBus.req.ready && io.dataReadBus.req.ready

  Debug("in.ready = %d, in.valid = %d, out.valid = %d, out.ready = %d, addr = %x, cmd = %x, dataReadBus.req.valid = %d\n", io.in.ready, io.in.valid, io.out.valid, io.out.ready, io.in.bits.addr, io.in.bits.cmd, io.dataReadBus.req.valid)
}

sealed class Stage2IO(implicit val cacheConfig: CacheConfig) extends CacheBundle {
  val req = new SimpleBusReqBundle(userBits = userBits, idBits = idBits)
  val metas = Vec(Ways, new MetaBundle)
  val datas = Vec(Ways, new DataBundle)
  val hit = Output(Bool())
  val waymask = Output(UInt(Ways.W))
  val mmio = Output(Bool())
  val isForwardData = Output(Bool())
  val forwardData = Output(CacheDataArrayWriteBus().req.bits)
}

// check
sealed class CacheStage2(implicit val cacheConfig: CacheConfig) extends CacheModule {
  class CacheStage2IO extends Bundle {
    val in = Flipped(Decoupled(new Stage1IO))
    val out = Decoupled(new Stage2IO)
    val metaReadResp = Flipped(Vec(Ways, new MetaBundle))
    val dataReadResp = Flipped(Vec(Ways, new DataBundle))
    val metaWriteBus = Input(CacheMetaArrayWriteBus())
    val dataWriteBus = Input(CacheDataArrayWriteBus())
  }
  val io = IO(new CacheStage2IO)

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

// writeback
sealed class CacheStage3(implicit val cacheConfig: CacheConfig) extends CacheModule {
  class CacheStage3IO extends Bundle {
    val in = Flipped(Decoupled(new Stage2IO))
    val out = Decoupled(new SimpleBusRespBundle(userBits = userBits, idBits = idBits))
    val isFinish = Output(Bool())
    val flush = Input(Bool())
    val dataReadBus = CacheDataArrayReadBus()
    val dataWriteBus = CacheDataArrayWriteBus()
    val metaWriteBus = CacheMetaArrayWriteBus()

    val mem = new SimpleBusUC
    val mmio = new SimpleBusUC
    val cohResp = Decoupled(new SimpleBusRespBundle)

    // use to distinguish prefetch request and normal request
    val dataReadRespToL1 = Output(Bool())
  }
  val io = IO(new CacheStage3IO)

  val metaWriteArb = Module(new Arbiter(CacheMetaArrayWriteBus().req.bits, 2))
  val dataWriteArb = Module(new Arbiter(CacheDataArrayWriteBus().req.bits, 2))

  val req = io.in.bits.req
  val addr = req.addr.asTypeOf(addrBundle)
  val mmio = io.in.valid && io.in.bits.mmio
  val hit = io.in.valid && io.in.bits.hit
  val miss = io.in.valid && !io.in.bits.hit
  val probe = io.in.valid && hasCoh.B && req.isProbe()
  val hitReadBurst = hit && req.isReadBurst()
  val meta = Mux1H(io.in.bits.waymask, io.in.bits.metas)
  assert(!(mmio && hit), "MMIO request should not hit in cache")


  // this is ugly
  if (cacheName == "dcache") {
    BoringUtils.addSource(mmio, "lsuMMIO")
  }

  val useForwardData = io.in.bits.isForwardData && io.in.bits.waymask === io.in.bits.forwardData.waymask.getOrElse("b1".U)
  val dataReadArray = Mux1H(io.in.bits.waymask, io.in.bits.datas).data
  val dataRead = Mux(useForwardData, io.in.bits.forwardData.data.data, dataReadArray)
  val wordMask = Mux(!ro.B && req.isWrite(), MaskExpand(req.wmask), 0.U(DataBits.W))

  val writeL2BeatCnt = Counter(LineBeats)
  when(io.out.fire() && (req.cmd === SimpleBusCmd.writeBurst || req.isWriteLast())) {
    writeL2BeatCnt.inc()
  }

  val hitWrite = hit && req.isWrite()
  val dataHitWriteBus = Wire(CacheDataArrayWriteBus()).apply(
    data = Wire(new DataBundle).apply(MaskData(dataRead, req.wdata, wordMask)),
    valid = hitWrite, setIdx = Cat(addr.index, Mux(req.cmd === SimpleBusCmd.writeBurst || req.isWriteLast(), writeL2BeatCnt.value, addr.wordIndex)), waymask = io.in.bits.waymask)

  val metaHitWriteBus = Wire(CacheMetaArrayWriteBus()).apply(
    valid = hitWrite && !meta.dirty, setIdx = getMetaIdx(req.addr), waymask = io.in.bits.waymask,
    data = Wire(new MetaBundle).apply(tag = meta.tag, valid = true.B, dirty = (!ro).B)
  )

  val s_idle :: s_memReadReq :: s_memReadResp :: s_memWriteReq :: s_memWriteResp :: s_mmioReq :: s_mmioResp :: s_wait_resp :: s_release :: Nil = Enum(9)
  val state = RegInit(s_idle)
  val needFlush = RegInit(false.B)

  when (io.flush && (state =/= s_idle)) { needFlush := true.B }
  when (io.out.fire() && needFlush) { needFlush := false.B }

  val readBeatCnt = Counter(LineBeats)
  val writeBeatCnt = Counter(LineBeats)

  val s2_idle :: s2_dataReadWait :: s2_dataOK :: Nil = Enum(3)
  val state2 = RegInit(s2_idle)

  io.dataReadBus.apply(valid = (state === s_memWriteReq || state === s_release) && (state2 === s2_idle),
    setIdx = Cat(addr.index, Mux(state === s_release, readBeatCnt.value, writeBeatCnt.value)))
  val dataWay = RegEnable(io.dataReadBus.resp.data, state2 === s2_dataReadWait)
  val dataHitWay = Mux1H(io.in.bits.waymask, dataWay).data

  switch (state2) {
    is (s2_idle) { when (io.dataReadBus.req.fire()) { state2 := s2_dataReadWait } }
    is (s2_dataReadWait) { state2 := s2_dataOK }
    is (s2_dataOK) { when (io.mem.req.fire() || io.cohResp.fire() || hitReadBurst && io.out.ready) { state2 := s2_idle } }
  }

  // critical word first read
  val raddr = (if (XLEN == 64) Cat(req.addr(PAddrBits-1,3), 0.U(3.W))
                          else Cat(req.addr(PAddrBits-1,2), 0.U(2.W)))
  // dirty block addr
  val waddr = Cat(meta.tag, addr.index, 0.U(OffsetBits.W))
  val cmd = Mux(state === s_memReadReq, SimpleBusCmd.readBurst,
    Mux((writeBeatCnt.value === (LineBeats - 1).U), SimpleBusCmd.writeLast, SimpleBusCmd.writeBurst))
  io.mem.req.bits.apply(addr = Mux(state === s_memReadReq, raddr, waddr),
    cmd = cmd, size = (if (XLEN == 64) "b11".U else "b10".U),
    wdata = dataHitWay, wmask = Fill(DataBytes, 1.U))

  io.mem.resp.ready := true.B
  io.mem.req.valid := (state === s_memReadReq) || ((state === s_memWriteReq) && (state2 === s2_dataOK))

  // mmio
  io.mmio.req.bits := req
  io.mmio.resp.ready := true.B
  io.mmio.req.valid := (state === s_mmioReq)

  val afterFirstRead = RegInit(false.B)
  val alreadyOutFire = RegEnable(true.B, init = false.B, io.out.fire())
  val readingFirst = !afterFirstRead && io.mem.resp.fire() && (state === s_memReadResp)
  val inRdataRegDemand = RegEnable(Mux(mmio, io.mmio.resp.bits.rdata, io.mem.resp.bits.rdata),
                                   Mux(mmio, state === s_mmioResp, readingFirst))

  // probe
  io.cohResp.valid := ((state === s_idle) && probe) ||
                      ((state === s_release) && (state2 === s2_dataOK))
  io.cohResp.bits.rdata := dataHitWay
  val releaseLast = Counter(state === s_release && io.cohResp.fire(), LineBeats)._2
  io.cohResp.bits.cmd := Mux(state === s_release, Mux(releaseLast, SimpleBusCmd.readLast, 0.U),
    Mux(hit, SimpleBusCmd.probeHit, SimpleBusCmd.probeMiss))

  val respToL1Fire = hitReadBurst && io.out.ready && state2 === s2_dataOK
  val respToL1Last = Counter((state === s_idle || state === s_release && state2 === s2_dataOK) && hitReadBurst && io.out.ready, LineBeats)._2

  switch (state) {
    is (s_idle) {
      afterFirstRead := false.B
      alreadyOutFire := false.B

      when (probe) {
        when (io.cohResp.fire()) {
          state := Mux(hit, s_release, s_idle)
          readBeatCnt.value := addr.wordIndex
        }
      } .elsewhen (hitReadBurst && io.out.ready) {
        state := s_release
        readBeatCnt.value := Mux(addr.wordIndex === (LineBeats - 1).U, 0.U, (addr.wordIndex + 1.U))
      } .elsewhen ((miss || mmio) && !io.flush) {
        state := Mux(mmio, s_mmioReq, Mux(!ro.B && meta.dirty, s_memWriteReq, s_memReadReq))
      }
    }

    is (s_mmioReq) { when (io.mmio.req.fire()) { state := s_mmioResp } }
    is (s_mmioResp) { when (io.mmio.resp.fire()) { state := s_wait_resp } }

    is (s_release) {
      when (io.cohResp.fire() || respToL1Fire) { readBeatCnt.inc() }
      when (probe && io.cohResp.fire() && releaseLast || respToL1Fire && respToL1Last) { state := s_idle }
    }

    is (s_memReadReq) { when (io.mem.req.fire()) {
      state := s_memReadResp
      readBeatCnt.value := addr.wordIndex
    }}

    is (s_memReadResp) {
      when (io.mem.resp.fire()) {
        afterFirstRead := true.B
        readBeatCnt.inc()
        when (req.cmd === SimpleBusCmd.writeBurst) { writeL2BeatCnt.value := 0.U }
        when (io.mem.resp.bits.isReadLast()) { state := s_wait_resp }
      }
    }

    is (s_memWriteReq) {
      when (io.mem.req.fire()) { writeBeatCnt.inc() }
      when (io.mem.req.bits.isWriteLast() && io.mem.req.fire()) { state := s_memWriteResp }
    }

    is (s_memWriteResp) { when (io.mem.resp.fire()) { state := s_memReadReq } }
    is (s_wait_resp) { when (io.out.fire() || needFlush || alreadyOutFire) { state := s_idle } }
  }

  val dataRefill = MaskData(io.mem.resp.bits.rdata, req.wdata, Mux(readingFirst, wordMask, 0.U(DataBits.W)))
  val dataRefillWriteBus = Wire(CacheDataArrayWriteBus).apply(
    valid = (state === s_memReadResp) && io.mem.resp.fire(), setIdx = Cat(addr.index, readBeatCnt.value),
    data = Wire(new DataBundle).apply(dataRefill), waymask = io.in.bits.waymask)

  dataWriteArb.io.in(0) <> dataHitWriteBus.req
  dataWriteArb.io.in(1) <> dataRefillWriteBus.req
  io.dataWriteBus.req <> dataWriteArb.io.out

  val metaRefillWriteBus = Wire(CacheMetaArrayWriteBus()).apply(
    valid = (state === s_memReadResp) && io.mem.resp.fire() && io.mem.resp.bits.isReadLast(),
    data = Wire(new MetaBundle).apply(valid = true.B, tag = addr.tag, dirty = !ro.B && req.isWrite()),
    setIdx = getMetaIdx(req.addr), waymask = io.in.bits.waymask
  )

  metaWriteArb.io.in(0) <> metaHitWriteBus.req
  metaWriteArb.io.in(1) <> metaRefillWriteBus.req
  io.metaWriteBus.req <> metaWriteArb.io.out

  if (cacheLevel == 2) {
    when ((state === s_memReadResp) && io.mem.resp.fire() && req.isReadBurst()) {
      // readBurst request miss
      io.out.bits.rdata := dataRefill
      io.out.bits.cmd := Mux(io.mem.resp.bits.isReadLast(), SimpleBusCmd.readLast, SimpleBusCmd.readBurst)
    }.elsewhen (req.isWriteLast() || req.cmd === SimpleBusCmd.writeBurst) {
      // writeBurst/writeLast request, no matter hit or miss
      io.out.bits.rdata := Mux(hit, dataRead, inRdataRegDemand)
      io.out.bits.cmd := DontCare
    }.elsewhen (hitReadBurst && state === s_release) {
      // readBurst request hit
      io.out.bits.rdata := dataHitWay
      io.out.bits.cmd := Mux(respToL1Last, SimpleBusCmd.readLast, SimpleBusCmd.readBurst)
    }.otherwise {
      io.out.bits.rdata := Mux(hit, dataRead, inRdataRegDemand)
      io.out.bits.cmd := req.cmd
    }
  } else {
    io.out.bits.rdata := Mux(hit, dataRead, inRdataRegDemand)
    io.out.bits.cmd := Mux(io.in.bits.req.isRead(), SimpleBusCmd.readLast, Mux(io.in.bits.req.isWrite(), SimpleBusCmd.writeResp, DontCare))//DontCare, added by lemover
  }
  io.out.bits.user.zip(req.user).map { case (o,i) => o := i }
  io.out.bits.id.zip(req.id).map { case (o,i) => o := i }

  io.out.valid := io.in.valid && Mux(req.isBurst() && (cacheLevel == 2).B,
    Mux(req.isWrite() && (hit || !hit && state === s_wait_resp), true.B, (state === s_memReadResp && io.mem.resp.fire() && req.cmd === SimpleBusCmd.readBurst)) || (respToL1Fire && respToL1Last && state === s_release),
    Mux(probe, false.B, Mux(hit, true.B, Mux(req.isWrite() || mmio, state === s_wait_resp, afterFirstRead && !alreadyOutFire)))
  )

  // With critical-word first, the pipeline registers between
  // s2 and s3 can not be overwritten before a missing request
  // is totally handled. We use io.isFinish to indicate when the
  // request really ends.
  io.isFinish := Mux(probe, io.cohResp.fire() && Mux(miss, state === s_idle, (state === s_release) && releaseLast),
    Mux(hit || req.isWrite(), io.out.fire(), (state === s_wait_resp) && (io.out.fire() || alreadyOutFire))
  )

  io.in.ready := io.out.ready && (state === s_idle && !hitReadBurst) && !miss && !probe
  io.dataReadRespToL1 := hitReadBurst && (state === s_idle && io.out.ready || state === s_release && state2 === s2_dataOK)

  assert(!(metaHitWriteBus.req.valid && metaRefillWriteBus.req.valid))
  assert(!(dataHitWriteBus.req.valid && dataRefillWriteBus.req.valid))
  assert(!(!ro.B && io.flush), "only allow to flush icache")
  Debug(" metaread idx %x waymask %b metas %x%x:%x %x%x:%x %x%x:%x %x%x:%x %x\n", getMetaIdx(req.addr), io.in.bits.waymask.asUInt, io.in.bits.metas(0).valid, io.in.bits.metas(0).dirty, io.in.bits.metas(0).tag, io.in.bits.metas(1).valid, io.in.bits.metas(1).dirty, io.in.bits.metas(1).tag, io.in.bits.metas(2).valid, io.in.bits.metas(2).dirty, io.in.bits.metas(2).tag, io.in.bits.metas(3).valid, io.in.bits.metas(3).dirty, io.in.bits.metas(3).tag, io.in.bits.datas.asUInt)
  Debug(io.metaWriteBus.req.fire(), "%d: [" + cacheName + " S3]: metawrite idx %x wmask %b meta %x%x:%x\n", GTimer(), io.metaWriteBus.req.bits.setIdx, io.metaWriteBus.req.bits.waymask.get, io.metaWriteBus.req.bits.data.valid, io.metaWriteBus.req.bits.data.dirty, io.metaWriteBus.req.bits.data.tag)
  Debug(" in.ready = %d, in.valid = %d, hit = %x, state = %d, addr = %x cmd:%d probe:%d isFinish:%d\n", io.in.ready, io.in.valid, hit, state, req.addr, req.cmd, probe, io.isFinish)
  Debug(" out.valid:%d rdata:%x cmd:%d user:%x id:%x \n", io.out.valid, io.out.bits.rdata, io.out.bits.cmd, io.out.bits.user.getOrElse(0.U), io.out.bits.id.getOrElse(0.U))
  Debug(" DHW: (%d, %d), data:%x setIdx:%x MHW:(%d, %d)\n", dataHitWriteBus.req.valid, dataHitWriteBus.req.ready, dataHitWriteBus.req.bits.data.asUInt, dataHitWriteBus.req.bits.setIdx, metaHitWriteBus.req.valid, metaHitWriteBus.req.ready)
  Debug(" DreadCache: %x \n", io.in.bits.datas.asUInt)
  Debug(" useFD:%d isFD:%d FD:%x DreadArray:%x dataRead:%x inwaymask:%x FDwaymask:%x \n", useForwardData, io.in.bits.isForwardData, io.in.bits.forwardData.data.data, dataReadArray, dataRead, io.in.bits.waymask, io.in.bits.forwardData.waymask.getOrElse("b1".U))
  Debug(io.dataWriteBus.req.fire(), "[WB] waymask: %b data:%x setIdx:%x\n", 
    io.dataWriteBus.req.bits.waymask.get.asUInt, io.dataWriteBus.req.bits.data.asUInt, io.dataWriteBus.req.bits.setIdx)
  Debug((state === s_memWriteReq) && io.mem.req.fire(), "[COUTW] cnt %x addr %x data %x cmd %x size %x wmask %x tag %x idx %x waymask %b \n", writeBeatCnt.value, io.mem.req.bits.addr, io.mem.req.bits.wdata, io.mem.req.bits.cmd, io.mem.req.bits.size, io.mem.req.bits.wmask, addr.tag, getMetaIdx(req.addr), io.in.bits.waymask)
  Debug((state === s_memReadReq) && io.mem.req.fire(), "[COUTR] addr %x tag %x idx %x waymask %b \n", io.mem.req.bits.addr, addr.tag, getMetaIdx(req.addr), io.in.bits.waymask)
  Debug((state === s_memReadResp) && io.mem.resp.fire(), "[COUTR] cnt %x data %x tag %x idx %x waymask %b \n", readBeatCnt.value, io.mem.resp.bits.rdata, addr.tag, getMetaIdx(req.addr), io.in.bits.waymask)
}

class Cache(implicit val cacheConfig: CacheConfig) extends CacheModule with HasCacheIO {
  // cpu pipeline
  val s1 = Module(new CacheStage1)
  val s2 = Module(new CacheStage2)
  val s3 = Module(new CacheStage3)
  val metaArray = Module(new SRAMTemplateWithArbiter(nRead = 1, new MetaBundle, set = Sets, way = Ways, shouldReset = true))
  val dataArray = Module(new SRAMTemplateWithArbiter(nRead = 2, new DataBundle, set = Sets * LineBeats, way = Ways))

  if (cacheName == "icache") {
    // flush icache when executing fence.i
    val flushICache = WireInit(false.B)
    BoringUtils.addSink(flushICache, "MOUFlushICache")
    metaArray.reset := reset.asBool || flushICache
  }

  val arb = Module(new Arbiter(new SimpleBusReqBundle(userBits = userBits, idBits = idBits), hasCohInt + 1))
  arb.io.in(hasCohInt + 0) <> io.in.req

  s1.io.in <> arb.io.out
  /*
  val s2BlockByPrefetch = if (cacheLevel == 2) {
      s2.io.out.valid && s3.io.in.valid && s3.io.in.bits.req.isPrefetch() && !s3.io.in.ready
    } else { false.B }
  */
  PipelineConnect(s1.io.out, s2.io.in, s2.io.out.fire(), io.flush(0))
  PipelineConnect(s2.io.out, s3.io.in, s3.io.isFinish, io.flush(1))
  io.in.resp <> s3.io.out
  s3.io.flush := io.flush(1)
  io.out.mem <> s3.io.mem
  io.mmio <> s3.io.mmio
  io.empty := !s2.io.in.valid && !s3.io.in.valid

  io.in.resp.valid := Mux(s3.io.out.valid && s3.io.out.bits.isPrefetch(), false.B, s3.io.out.valid || s3.io.dataReadRespToL1)

  if (hasCoh) {
    val cohReq = io.out.coh.req.bits
    // coh does not have user signal, any better code?
    val coh = Wire(new SimpleBusReqBundle(userBits = userBits, idBits = idBits))
    coh.apply(addr = cohReq.addr, cmd = cohReq.cmd, size = cohReq.size, wdata = cohReq.wdata, wmask = cohReq.wmask)
    arb.io.in(0).bits := coh
    arb.io.in(0).valid := io.out.coh.req.valid
    io.out.coh.req.ready := arb.io.in(0).ready
    io.out.coh.resp <> s3.io.cohResp
  } else {
    io.out.coh.req.ready := true.B
    io.out.coh.resp := DontCare
    io.out.coh.resp.valid := false.B
    s3.io.cohResp.ready := true.B
  }

  metaArray.io.r(0) <> s1.io.metaReadBus
  dataArray.io.r(0) <> s1.io.dataReadBus
  dataArray.io.r(1) <> s3.io.dataReadBus

  metaArray.io.w <> s3.io.metaWriteBus
  dataArray.io.w <> s3.io.dataWriteBus

  s2.io.metaReadResp := s1.io.metaReadBus.resp.data
  s2.io.dataReadResp := s1.io.dataReadBus.resp.data
  s2.io.dataWriteBus := s3.io.dataWriteBus
  s2.io.metaWriteBus := s3.io.metaWriteBus

  if (EnableOutOfOrderExec) {
    BoringUtils.addSource(s3.io.out.fire() && s3.io.in.bits.hit, "perfCntCondM" + cacheName + "Hit")
    BoringUtils.addSource(s3.io.in.valid && !s3.io.in.bits.hit, "perfCntCondM" + cacheName + "Loss")
    BoringUtils.addSource(s1.io.in.fire(), "perfCntCondM" + cacheName + "Req")
  }
  // io.in.dump(cacheName + ".in")
  Debug("InReq(%d, %d) InResp(%d, %d) \n", io.in.req.valid, io.in.req.ready, io.in.resp.valid, io.in.resp.ready)
  Debug("{IN s1:(%d,%d), s2:(%d,%d), s3:(%d,%d)} {OUT s1:(%d,%d), s2:(%d,%d), s3:(%d,%d)}\n", s1.io.in.valid, s1.io.in.ready, s2.io.in.valid, s2.io.in.ready, s3.io.in.valid, s3.io.in.ready, s1.io.out.valid, s1.io.out.ready, s2.io.out.valid, s2.io.out.ready, s3.io.out.valid, s3.io.out.ready)
  when (s1.io.in.valid) { Debug(p"[${cacheName}.S1]: ${s1.io.in.bits}\n") }
  when (s2.io.in.valid) { Debug(p"[${cacheName}.S2]: ${s2.io.in.bits.req}\n") }
  when (s3.io.in.valid) { Debug(p"[${cacheName}.S3]: ${s3.io.in.bits.req}\n") }
  //s3.io.mem.dump(cacheName + ".mem")
}

class Cache_fake(implicit val cacheConfig: CacheConfig) extends CacheModule with HasCacheIO {
  val s_idle :: s_memReq :: s_memResp :: s_mmioReq :: s_mmioResp :: s_wait_resp :: Nil = Enum(6)
  val state = RegInit(s_idle)

  val ismmio = AddressSpace.isMMIO(io.in.req.bits.addr)
  val ismmioRec = RegEnable(ismmio, io.in.req.fire())
  if (cacheConfig.name == "dcache") {
    BoringUtils.addSource(ismmio, "lsuMMIO")
  }

  val needFlush = RegInit(false.B)
  when (io.flush(0) && (state =/= s_idle)) { needFlush := true.B }
  when (state === s_idle && needFlush) { needFlush := false.B }

  val alreadyOutFire = RegEnable(true.B, init = false.B, io.in.resp.fire())

  switch (state) {
    is (s_idle) {
      alreadyOutFire := false.B
      when (io.in.req.fire() && !io.flush(0)) { state := Mux(ismmio, s_mmioReq, s_memReq) }
    }
    is (s_memReq) {
      when (io.out.mem.req.fire()) { state := s_memResp }
    }
    is (s_memResp) {
      when (io.out.mem.resp.fire()) { state := s_wait_resp }
    }
    is (s_mmioReq) {
      when (io.mmio.req.fire()) { state := s_mmioResp }
    }
    is (s_mmioResp) {
      when (io.mmio.resp.fire() || alreadyOutFire) { state := s_wait_resp }
    }
    is (s_wait_resp) {
      when (io.in.resp.fire() || needFlush || alreadyOutFire) { state := s_idle }
    }
  }

  val reqaddr = RegEnable(io.in.req.bits.addr, io.in.req.fire())
  val cmd = RegEnable(io.in.req.bits.cmd, io.in.req.fire())
  val size = RegEnable(io.in.req.bits.size, io.in.req.fire())
  val wdata = RegEnable(io.in.req.bits.wdata, io.in.req.fire())
  val wmask = RegEnable(io.in.req.bits.wmask, io.in.req.fire())

  io.in.req.ready := (state === s_idle)
  io.in.resp.valid := (state === s_wait_resp) && (!needFlush)

  val mmiordata = RegEnable(io.mmio.resp.bits.rdata, io.mmio.resp.fire())
  val mmiocmd = RegEnable(io.mmio.resp.bits.cmd, io.mmio.resp.fire())
  val memrdata = RegEnable(io.out.mem.resp.bits.rdata, io.out.mem.resp.fire())
  val memcmd = RegEnable(io.out.mem.resp.bits.cmd, io.out.mem.resp.fire())

  io.in.resp.bits.rdata := Mux(ismmioRec, mmiordata, memrdata)
  io.in.resp.bits.cmd := Mux(ismmioRec, mmiocmd, memcmd)

  val memuser = RegEnable(io.in.req.bits.user.getOrElse(0.U), io.in.req.fire())
  io.in.resp.bits.user.zip(if (userBits > 0) Some(memuser) else None).map { case (o,i) => o := i }

  io.out.mem.req.bits.apply(addr = reqaddr,
    cmd = cmd, size = size,
    wdata = wdata, wmask = wmask)
  io.out.mem.req.valid := (state === s_memReq)
  io.out.mem.resp.ready := true.B
  
  io.mmio.req.bits.apply(addr = reqaddr,
    cmd = cmd, size = size,
    wdata = wdata, wmask = wmask)
  io.mmio.req.valid := (state === s_mmioReq)
  io.mmio.resp.ready := true.B

  io.empty := false.B
  io.out.coh := DontCare

  Debug(io.in.req.fire(), p"in.req: ${io.in.req.bits}\n")
  Debug(io.out.mem.req.fire(), p"out.mem.req: ${io.out.mem.req.bits}\n")
  Debug(io.out.mem.resp.fire(), p"out.mem.resp: ${io.out.mem.resp.bits}\n")
  Debug(io.in.resp.fire(), p"in.resp: ${io.in.resp.bits}\n")
}

class Cache_dummy(implicit val cacheConfig: CacheConfig) extends CacheModule with HasCacheIO {

  val needFlush = RegInit(false.B)
  when (io.flush(0)) {
    needFlush := true.B
  }
  when (io.in.req.fire() && !io.flush(0)) {
    needFlush := false.B
  }

  io.in.req.ready := io.out.mem.req.ready
  io.in.resp.valid := (io.out.mem.resp.valid && !needFlush) || io.flush(0)

  io.in.resp.bits.rdata := io.out.mem.resp.bits.rdata
  io.in.resp.bits.cmd := io.out.mem.resp.bits.cmd
  val memuser = RegEnable(io.in.req.bits.user.getOrElse(0.U), io.in.req.fire())
  io.in.resp.bits.user.zip(if (userBits > 0) Some(memuser) else None).map { case (o,i) => o := i }

  io.out.mem.req.bits.apply( 
    addr = io.in.req.bits.addr,
    cmd = io.in.req.bits.cmd,
    size = io.in.req.bits.size,
    wdata = io.in.req.bits.wdata,
    wmask = io.in.req.bits.wmask
  )
  io.out.mem.req.valid := io.in.req.valid
  io.out.mem.resp.ready := io.in.resp.ready

  io.empty := false.B
  io.mmio := DontCare
  io.out.coh := DontCare
}

object Cache {
  def apply(in: SimpleBusUC, mmio: Seq[SimpleBusUC], flush: UInt, empty: Bool, enable: Boolean = true)(implicit cacheConfig: CacheConfig) = {
    val cache = if (enable) Module(new Cache) 
                else (if (Settings.get("IsRV32")) 
                        (if (cacheConfig.name == "dcache") Module(new Cache_fake) else Module(new Cache_dummy)) 
                      else 
                        (Module(new Cache_fake)))
    cache.io.flush := flush
    cache.io.in <> in
    mmio(0) <> cache.io.mmio
    empty := cache.io.empty
    cache.io.out
  }
}
