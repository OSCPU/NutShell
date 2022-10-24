
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

// writeback
class CacheStage3(implicit val cacheConfig: CacheConfig) extends CacheModule {
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
