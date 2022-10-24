
package nutcore.mem.tlb

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import nutcore._

import bus.simplebus._
import bus.axi4._
import utils._
import top.Settings


class TLBExec(implicit val tlbConfig: TLBConfig) extends TlbModule{
  class TLBExecIO extends Bundle {
    val in = Flipped(Decoupled(new SimpleBusReqBundle(userBits = userBits, addrBits = VAddrBits)))
    val out = Decoupled(new SimpleBusReqBundle(userBits = userBits))

    val md = Input(Vec(Ways, UInt(tlbLen.W)))
    val mdWrite = new TLBMDWriteBundle(IndexBits = IndexBits, Ways = Ways, tlbLen = tlbLen)
    val mdReady = Input(Bool())

    val mem = new SimpleBusUC(userBits = userBits)
    val flush = Input(Bool())
    val satp = Input(UInt(XLEN.W))
    val pf = new MMUIO
    val ipf = Output(Bool())
    val isFinish = Output(Bool())
  }
  val io = IO(new TLBExecIO)

  val md = io.md//RegEnable(mdTLB.io.tlbmd, io.in.ready)
  
  // lazy renaming
  val req = io.in.bits
  val vpn = req.addr.asTypeOf(vaBundle2).vpn.asTypeOf(vpnBundle)
  val pf = io.pf
  val satp = io.satp.asTypeOf(satpBundle)
  val ifecth = if(tlbname == "itlb") true.B else false.B

  // pf init
  pf.loadPF := false.B
  pf.storePF := false.B
  pf.addr := req.addr

  // check hit or miss
  val hitVec = VecInit(md.map(m => m.asTypeOf(tlbBundle).flag.asTypeOf(flagBundle).v && (m.asTypeOf(tlbBundle).asid === satp.asid) && MaskEQ(m.asTypeOf(tlbBundle).mask, m.asTypeOf(tlbBundle).vpn, vpn.asUInt))).asUInt
  val hit = io.in.valid && hitVec.orR
  val miss = io.in.valid && !hitVec.orR

  val victimWaymask = if (Ways > 1) (1.U << LFSR64()(log2Up(Ways)-1,0)) else "b1".U
  val waymask = Mux(hit, hitVec, victimWaymask)

  val loadPF = WireInit(false.B)
  val storePF = WireInit(false.B)

  // hit
  val hitMeta = Mux1H(waymask, md).asTypeOf(tlbBundle2).meta.asTypeOf(metaBundle)
  val hitData = Mux1H(waymask, md).asTypeOf(tlbBundle2).data.asTypeOf(dataBundle)
  val hitFlag = hitMeta.flag.asTypeOf(flagBundle)
  val hitMask = hitMeta.mask
  // hit write back pte.flag
  val hitinstrPF = WireInit(false.B)
  val hitWB = hit && (!hitFlag.a || !hitFlag.d && req.isWrite()) && !hitinstrPF && !(loadPF || storePF || io.pf.isPF())
  val hitRefillFlag = Cat(req.isWrite().asUInt, 1.U(1.W), 0.U(6.W)) | hitFlag.asUInt
  val hitWBStore = RegEnable(Cat(0.U(10.W), hitData.ppn, 0.U(2.W), hitRefillFlag), hitWB)

  // hit permission check
  val hitCheck = hit /*&& hitFlag.v */&& !(pf.priviledgeMode === ModeU && !hitFlag.u) && !(pf.priviledgeMode === ModeS && hitFlag.u && (!pf.status_sum || ifecth))
  val hitExec = hitCheck && hitFlag.x
  val hitLoad = hitCheck && (hitFlag.r || pf.status_mxr && hitFlag.x)
  val hitStore = hitCheck && hitFlag.w

  io.pf.loadPF := loadPF //RegNext(loadPF, init =false.B)
  io.pf.storePF := storePF //RegNext(storePF, init = false.B)

  if (tlbname == "itlb") { hitinstrPF := !hitExec  && hit}
  if (tlbname == "dtlb") { 
    loadPF := !hitLoad && req.isRead() && hit
    storePF := (!hitStore && req.isWrite() && hit)
    // AMO pagefault type will be fixed in LSU
  }

  // miss
  val s_idle :: s_memReadReq :: s_memReadResp :: s_write_pte :: s_wait_resp :: s_miss_slpf :: Nil = Enum(6)
  val state = RegInit(s_idle)
  val level = RegInit(Level.U(log2Up(Level).W))
  
  val memRespStore = Reg(UInt(XLEN.W))
  val missMask = WireInit("h3ffff".U(maskLen.W))
  val missMaskStore = Reg(UInt(maskLen.W))
  val missMetaRefill = WireInit(false.B)
  val missRefillFlag = WireInit(0.U(8.W))
  val memRdata = io.mem.resp.bits.rdata.asTypeOf(pteBundle)
  val raddr = Reg(UInt(PAddrBits.W))
  val alreadyOutFire = RegEnable(true.B, init = false.B, if(tlbname == "itlb") io.out.fire else io.out.valid)

  //handle flush
  val needFlush = RegInit(false.B)
  val ioFlush = io.flush
  val isFlush = needFlush || ioFlush
  when (ioFlush && (state =/= s_idle)) { needFlush := true.B}
  if(tlbname == "itlb"){
    when (io.out.fire() && needFlush) { needFlush := false.B}
  }
  if(tlbname == "dtlb"){
    when (io.out.valid && needFlush) { needFlush := false.B}
  }

  val missIPF = RegInit(false.B)

  // state machine to handle miss(ptw) and pte-writing-back
  switch (state) {
    is (s_idle) {
      when (!ioFlush && hitWB) {
        state := s_write_pte
        needFlush := false.B
        alreadyOutFire := false.B
      }.elsewhen (miss && !ioFlush) {
        state := s_memReadReq
        raddr := paddrApply(satp.ppn, vpn.vpn2) //
        level := Level.U
        needFlush := false.B
        alreadyOutFire := false.B
      }
    }

    is (s_memReadReq) { 
      when (isFlush) {
        state := s_idle
        needFlush := false.B
      }.elsewhen (io.mem.req.fire()) { state := s_memReadResp}
    }

    is (s_memReadResp) { 
      val missflag = memRdata.flag.asTypeOf(flagBundle)
      when (io.mem.resp.fire()) {
        when (isFlush) {
          state := s_idle
          needFlush := false.B
        }.elsewhen (!(missflag.r || missflag.x) && (level===3.U || level===2.U)) {
          when(!missflag.v || (!missflag.r && missflag.w)) { //TODO: fix needflush
            if(tlbname == "itlb") { state := s_wait_resp } else { state := s_miss_slpf }
            if(tlbname == "itlb") { missIPF := true.B }
            if(tlbname == "dtlb") { 
              loadPF := req.isRead()
              storePF := req.isWrite() 
            }  
            Debug("tlbException!!! ")
            Debug(false, p" req:${req}  Memreq:${io.mem.req}  MemResp:${io.mem.resp}")
            Debug(false, " level:%d",level)
            Debug(false, "\n")
          }.otherwise {
            state := s_memReadReq
            raddr := paddrApply(memRdata.ppn, Mux(level === 3.U, vpn.vpn1, vpn.vpn0))
          }
        }.elsewhen (level =/= 0.U) { //TODO: fix needFlush
          val permCheck = missflag.v && !(pf.priviledgeMode === ModeU && !missflag.u) && !(pf.priviledgeMode === ModeS && missflag.u && (!pf.status_sum || ifecth))
          val permExec = permCheck && missflag.x
          val permLoad = permCheck && (missflag.r || pf.status_mxr && missflag.x)
          val permStore = permCheck && missflag.w
          val updateAD = if (Settings.get("FPGAPlatform")) !missflag.a || (!missflag.d && req.isWrite()) else false.B
          val updateData = Cat( 0.U(56.W), req.isWrite(), 1.U(1.W), 0.U(6.W) )
          missRefillFlag := Cat(req.isWrite(), 1.U(1.W), 0.U(6.W)) | missflag.asUInt
          memRespStore := io.mem.resp.bits.rdata | updateData 
          if(tlbname == "itlb") {
            when (!permExec) { missIPF := true.B ; state := s_wait_resp}
            .otherwise { 
              state := Mux(updateAD, s_write_pte, s_wait_resp)
              missMetaRefill := true.B
            }
          }
          if(tlbname == "dtlb") {
            when((!permLoad && req.isRead()) || (!permStore && req.isWrite())) { 
              state := s_miss_slpf
              loadPF := req.isRead()
              storePF := req.isWrite()
            }.otherwise {
              state := Mux(updateAD, s_write_pte, s_wait_resp)
              missMetaRefill := true.B
            }
          }
          missMask := Mux(level===3.U, 0.U(maskLen.W), Mux(level===2.U, "h3fe00".U(maskLen.W), "h3ffff".U(maskLen.W)))
          missMaskStore := missMask
        }
        level := level - 1.U
      }
    }

    is (s_write_pte) {
      when (isFlush) {
        state := s_idle
        needFlush := false.B
      }.elsewhen (io.mem.req.fire()) { state := s_wait_resp }
    }

    is (s_wait_resp) { 
      if(tlbname == "itlb"){
        when (io.out.fire() || ioFlush || alreadyOutFire){
          state := s_idle
          missIPF := false.B
          alreadyOutFire := false.B
        }
      }
      if(tlbname == "dtlb"){
        state := s_idle
        missIPF := false.B
        alreadyOutFire := false.B
      }
    }

    is (s_miss_slpf) {
      state := s_idle
    }
  }

  // mem
  val cmd = Mux(state === s_write_pte, SimpleBusCmd.write, SimpleBusCmd.read)
  io.mem.req.bits.apply(addr = Mux(hitWB, hitData.pteaddr, raddr), cmd = cmd, size = (if (XLEN == 64) "b11".U else "b10".U), wdata =  Mux( hitWB, hitWBStore, memRespStore), wmask = 0xff.U)
  io.mem.req.valid := ((state === s_memReadReq || state === s_write_pte) && !isFlush)
  io.mem.resp.ready := true.B

  // tlb refill
  io.mdWrite.apply(wen = RegNext((missMetaRefill && !isFlush) || (hitWB && state === s_idle && !isFlush), init = false.B), 
    windex = RegNext(getIndex(req.addr)), waymask = RegNext(waymask), vpn = RegNext(vpn.asUInt), 
    asid = RegNext(Mux(hitWB, hitMeta.asid, satp.asid)), mask = RegNext(Mux(hitWB, hitMask, missMask)), 
    flag = RegNext(Mux(hitWB, hitRefillFlag, missRefillFlag)), ppn = RegNext(Mux(hitWB, hitData.ppn, memRdata.ppn)), 
    pteaddr = RegNext((Mux(hitWB, hitData.pteaddr, raddr))))

  // io
  io.out.bits := req
  io.out.bits.addr := Mux(hit, maskPaddr(hitData.ppn, req.addr(PAddrBits-1, 0), hitMask), maskPaddr(memRespStore.asTypeOf(pteBundle).ppn, req.addr(PAddrBits-1, 0), missMaskStore))
  io.out.valid := io.in.valid && Mux(hit && !hitWB, !(io.pf.isPF() || loadPF || storePF), state === s_wait_resp)// && !alreadyOutFire
  
  io.in.ready := io.out.ready && (state === s_idle) && !miss && !hitWB && io.mdReady && (!io.pf.isPF() && !loadPF && !storePF)//maybe be optimized

  io.ipf := Mux(hit, hitinstrPF, missIPF)
  io.isFinish := io.out.fire() || io.pf.isPF()

  if(tlbname == "dtlb") {
    io.isFinish := io.out.valid || io.pf.isPF()
    io.out.valid := io.in.valid && (Mux(hit && !hitWB, true.B, state === s_wait_resp) || loadPF || storePF)// && !alreadyOutFire
  }
  Debug("In(%d, %d) Out(%d, %d) InAddr:%x OutAddr:%x cmd:%d \n", io.in.valid, io.in.ready, io.out.valid, io.out.ready, req.addr, io.out.bits.addr, req.cmd)
  Debug("io.Flush:%d needFlush:%d alreadyOutFire:%d isFinish:%d\n", io.flush, needFlush, alreadyOutFire, io.isFinish)
  Debug("hit:%d hitWB:%d hitVPN:%x hitFlag:%x hitPPN:%x hitRefillFlag:%x hitWBStore:%x hitCheck:%d hitExec:%d hitLoad:%d hitStore:%d\n", hit, hitWB, hitMeta.vpn, hitFlag.asUInt, hitData.ppn, hitRefillFlag, hitWBStore, hitCheck, hitExec, hitLoad, hitStore)
  Debug("miss:%d state:%d level:%d raddr:%x memRdata:%x missMask:%x missRefillFlag:%x missMetaRefill:%d\n", miss, state, level, raddr, memRdata.asUInt, missMask, missRefillFlag, missMetaRefill)
  Debug("meta/data: (0)%x|%b|%x (1)%x|%b|%x (2)%x|%b|%x (3)%x|%b|%x rread:%d\n", md(0).asTypeOf(tlbBundle).vpn, md(0).asTypeOf(tlbBundle).flag, md(0).asTypeOf(tlbBundle).ppn, md(1).asTypeOf(tlbBundle).vpn, md(1).asTypeOf(tlbBundle).flag, md(1).asTypeOf(tlbBundle).ppn, md(2).asTypeOf(tlbBundle).vpn, md(2).asTypeOf(tlbBundle).flag, md(2).asTypeOf(tlbBundle).ppn, md(3).asTypeOf(tlbBundle).vpn, md(3).asTypeOf(tlbBundle).flag, md(3).asTypeOf(tlbBundle).ppn, io.mdReady)
  Debug("md: wen:%d windex:%x waymask:%x vpn:%x asid:%x mask:%x flag:%x asid:%x ppn:%x pteaddr:%x\n", io.mdWrite.wen, io.mdWrite.windex, io.mdWrite.waymask, io.mdWrite.wdata.asTypeOf(tlbBundle).vpn, io.mdWrite.wdata.asTypeOf(tlbBundle).asid, io.mdWrite.wdata.asTypeOf(tlbBundle).mask, io.mdWrite.wdata.asTypeOf(tlbBundle).flag, io.mdWrite.wdata.asTypeOf(tlbBundle).asid, io.mdWrite.wdata.asTypeOf(tlbBundle).ppn, io.mdWrite.wdata.asTypeOf(tlbBundle).pteaddr)
  Debug("MemReq(%d, %d) MemResp(%d, %d) addr:%x cmd:%d rdata:%x cmd:%d\n", io.mem.req.valid, io.mem.req.ready, io.mem.resp.valid, io.mem.resp.ready, io.mem.req.bits.addr, io.mem.req.bits.cmd, io.mem.resp.bits.rdata, io.mem.resp.bits.cmd)
  Debug("io.ipf:%d hitinstrPF:%d missIPF:%d pf.loadPF:%d pf.storePF:%d loadPF:%d storePF:%d\n", io.ipf, hitinstrPF, missIPF, io.pf.loadPF, io.pf.storePF, loadPF, storePF)
}
