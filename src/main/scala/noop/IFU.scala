package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import bus.simplebus._

trait HasResetVector{
  val resetFromSpiFlash = false
  val resetVector = if(resetFromSpiFlash){0x40000000L}else{0x80000000L}
}

class IFU extends NOOPModule with HasResetVector {
  val io = IO(new Bundle {

    val imem = new SimpleBusUC(userBits = ICacheUserBundleWidth, addrBits = VAddrBits)
    // val pc = Input(UInt(VAddrBits.W))
    val out = Decoupled(new InstFetchIO)

    val redirect = Flipped(new RedirectIO)
    val flushVec = Output(UInt(4.W))
    val bpFlush = Output(Bool())
    val ipf = Input(Bool())
  })

  // pc
  val pc = RegInit(resetVector.U(VAddrBits.W))
  // val pcBrIdx = RegInit(0.U(4.W))
  val pcInstValid = RegInit("b1111".U)
  val pcUpdate = io.redirect.valid || io.imem.req.fire()
  val snpc = Cat(pc(VAddrBits-1, 3), 0.U(3.W)) + CacheReadWidth.U  // IFU will always ask icache to fetch next instline 
  // Note: we define instline as 8 Byte aligned data from icache 

  val nlp = Module(new NLP)

  // cross instline inst branch predict logic "lateJump"
  // 
  // if "lateJump", icache will need to fetch next instline, then fetch redirect addr
  // "latejump" mechanism is used to speed up such code:
  // ```
  // 000c BranchCondition (32 bit inst)
  // ```
  // in this case, full inst is needed by BRU to get the right branch result, 
  // so we need to fetch the next inst line to get the higher bits of a 32bit branch inst
  // but in order to use BP result to avoid pipeline flush,
  // the next inst provided by icache should be predicted npc, instead of sequential npc
  val lateJump = nlp.io.lateJump
  val lateJumpLatch = RegInit(false.B) 
  when(pcUpdate || nlp.io.flush) {
    lateJumpLatch := Mux(nlp.io.flush, false.B, lateJump && !lateJumpLatch)
  }
  val lateJumpTarget = RegEnable(nlp.io.out.target, lateJump && pcUpdate) // ???

  // predicted next pc
  val pnpc = Mux(lateJump, snpc, nlp.io.out.target)
 
  // next pc
  val npc = Wire(UInt(VAddrBits.W))
  npc := Mux(io.redirect.valid, io.redirect.target, Mux(lateJumpLatch, lateJumpTarget, Mux(nlp.io.out.valid, pnpc, snpc)))
  // val npcIsSeq = Mux(io.redirect.valid , false.B, Mux(lateJumpLatch, false.B, Mux(lateJump, true.B, Mux(nlp.io.out.valid, false.B, true.B)))) //for debug only

  // instValid: which part of an instline contains an valid inst
  // e.g. 1100 means inst(s) in instline(63,32) is/are valid
  val npcInstValid = Wire(UInt(4.W))
  def genInstValid(pc: UInt) = LookupTree(pc(2,1), List(
    "b00".U -> "b1111".U,
    "b01".U -> "b1110".U,
    "b10".U -> "b1100".U,
    "b11".U -> "b1000".U
  ))
  npcInstValid := Mux(lateJump && !lateJumpLatch && !io.redirect.valid, "b0001".U, genInstValid(npc))

  // branch position index, 4 bit vector
  // e.g. brIdx 0010 means a branch is predicted/assigned at pc (offset 2)
  val brIdx = Wire(UInt(4.W))
  // predicted branch position index, 4 bit vector
  val pbrIdx = nlp.io.brIdx.asUInt | (lateJump << 3)
  def genBrIdx(pc: UInt) = LookupTree(pc(2,1), List(
  "b00".U -> "b0001".U,
  "b01".U -> "b0010".U,
  "b10".U -> "b0100".U,
  "b11".U -> "b1000".U
))
  brIdx := Mux(io.redirect.valid, 0.U, Mux(lateJumpLatch, 0.U, pbrIdx))
  
  //TODO: BP will be disabled shortly after a redirect request

  nlp.io.in.pc.valid := io.imem.req.fire() // only predict when Icache accepts a request
  nlp.io.in.pc.bits := npc  // predict one cycle early
  nlp.io.flush := io.redirect.valid // redirect means BPU may need to be updated

  //val bp2 = Module(new BPU2)
  //bp2.io.in.bits := io.out.bits
  //bp2.io.in.valid := io.imem.resp.fire()

  when (pcUpdate) { 
    pc := npc
    pcInstValid := npcInstValid
    // pcBrIdx := brIdx // just for debug
    // printf("[IF1] pc=%x\n", pc)
  }

  Debug(){
    when(pcUpdate) {
      printf("[IFUIN] pc:%x pcUpdate:%d npc:%x RedValid:%d RedTarget:%x LJL:%d LJTarget:%x LJ:%d snpc:%x bpValid:%d pnpc:%x \n",pc, pcUpdate, npc, io.redirect.valid,io.redirect.target,lateJumpLatch,lateJumpTarget,lateJump,snpc,nlp.io.out.valid,nlp.io.out.target)
      //printf(p"[IFUIN] redirect: ${io.redirect} \n")
    }
  }

  io.flushVec := Mux(io.redirect.valid, Mux(io.redirect.rtype === 0.U, "b1111".U, "b0011".U), 0.U)
  io.bpFlush := false.B

  io.imem.req.bits.apply(addr = Cat(pc(VAddrBits-1,1),0.U(1.W)), //cache will treat it as Cat(pc(63,3),0.U(3.W))
    size = "b11".U, cmd = SimpleBusCmd.read, wdata = 0.U, wmask = 0.U, user = Cat(pcInstValid, brIdx & pcInstValid, Mux(lateJump, nlp.io.out.target, npc), pc))
  io.imem.req.valid := io.out.ready
  //TODO: add ctrlFlow.exceptionVec
  io.imem.resp.ready := io.out.ready || io.flushVec(0)

  io.out.bits := DontCare
    //inst path only uses 32bit inst, get the right inst according to pc(2)

  Debug(){
    when(io.imem.req.fire()){
      printf("[IFI] pc=%x user=%x redirect %x pcInstValid %b brIdx %b npc %x pc %x pnpc %x\n", io.imem.req.bits.addr, io.imem.req.bits.user.getOrElse(0.U), io.redirect.valid, pcInstValid.asUInt, (pcInstValid & brIdx).asUInt, npc, pc, nlp.io.out.target)
    }
    when (io.out.fire()) {
          printf("[IFO] pc=%x user=%x inst=%x npc=%x bridx %b valid %b ipf %x\n", io.out.bits.pc, io.imem.resp.bits.user.get, io.out.bits.instr, io.out.bits.pnpc, io.out.bits.brIdx.asUInt, io.out.bits.instValid.asUInt, io.ipf)
    }
  }

  // io.out.bits.instr := (if (XLEN == 64) io.imem.resp.bits.rdata.asTypeOf(Vec(2, UInt(32.W)))(io.out.bits.pc(2))
                      //  else io.imem.resp.bits.rdata)
  io.out.bits.instr := io.imem.resp.bits.rdata
  io.imem.resp.bits.user.map{ case x =>
    io.out.bits.pc := x(VAddrBits-1,0)
    io.out.bits.pnpc := x(VAddrBits*2-1,VAddrBits)
    io.out.bits.brIdx := x(VAddrBits*2 + 3, VAddrBits*2)
    io.out.bits.instValid := x(VAddrBits*2 + 7, VAddrBits*2 + 4)
  }
  io.out.bits.icachePF := io.ipf
  // assert(!io.out.bits.icachePF)
  io.out.valid := io.imem.resp.valid && !io.flushVec(0)


  // Illegal branch predict check
  val FixInvalidBranchPredict = false

  def preDecodeIsBranch(x: UInt) = {
    require(x.getWidth == 16)
    val res :: Nil = ListLookup(x, List(false.B), PreDecode.branchTable)
    res
  }

  if(FixInvalidBranchPredict){
    val maybeBranch = Wire(Vec(4, Bool()))
    (0 until 4).map(i => maybeBranch(i) := preDecodeIsBranch(io.out.bits.instr(16*(i+1)-1, 16*i))) //TODO: use icache pre-decode result
    // When branch predicter set non-sequential npc for a non-branch inst,
    // flush IFU, fetch sequential inst instead.
    when((io.out.bits.brIdx & ~maybeBranch.asUInt).orR && io.out.fire()){
      io.bpFlush := true.B
      io.out.bits.brIdx := 0.U
      npc := io.out.bits.pc + 8.U
      pcUpdate := true.B
    }
    // TODO: update BPU
  }

  BoringUtils.addSource(BoolStopWatch(io.imem.req.valid, io.imem.resp.fire()), "perfCntCondMimemStall")
  BoringUtils.addSource(io.flushVec.orR, "perfCntCondMifuFlush")
}
