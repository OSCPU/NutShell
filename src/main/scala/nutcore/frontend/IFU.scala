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

import utils._
import bus.simplebus._
import top.Settings
import difftest._

trait HasResetVector {
  val resetVector = Settings.getLong("ResetVector")
}

class ICacheUserBundle extends NutCoreBundle {
    val pc = UInt(VAddrBits.W)
    val brIdx = UInt(4.W) // mark if an inst is predicted to branch
    val pnpc = UInt(VAddrBits.W)
    val instValid = UInt(4.W) // mark which part of this inst line is valid
}
// Note: update ICacheUserBundleWidth when change ICacheUserBundle

class IFU_ooo extends NutCoreModule with HasResetVector {
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
  val pcUpdate = Wire(Bool())
  pcUpdate := io.redirect.valid || io.imem.req.fire
  val snpc = Cat(pc(VAddrBits-1, 3), 0.U(3.W)) + CacheReadWidth.U  // IFU will always ask icache to fetch next instline 
  // Note: we define instline as 8 Byte aligned data from icache 

  // Next-line branch predictor
  val nlp = Module(new BPU_ooo)

  // nlpxxx_latch is used for the situation when I$ is disabled
  val nlpvalidreg = RegInit(false.B)
  val nlpvalid_latch = nlpvalidreg & !io.redirect.valid
  val nlpbridx_latch = RegInit(0.U(4.W))
  val nlptarget_latch = RegInit(0.U(VAddrBits.W))

  when (nlp.io.out.valid) {
    nlpvalidreg := true.B
    nlpbridx_latch := nlp.io.brIdx.asUInt
    nlptarget_latch := nlp.io.out.target
  }

  when (io.imem.req.fire || io.redirect.valid) {
    nlpvalidreg := false.B
    nlpbridx_latch := 0.U
    nlptarget_latch := 0.U
  }

  val bpuValid = if (Settings.get("HasIcache")) nlp.io.out.valid else nlpvalid_latch
  val bpuTarget = if (Settings.get("HasIcache")) nlp.io.out.target else nlptarget_latch
  val bpuBrIdx = if (Settings.get("HasIcache")) nlp.io.brIdx.asUInt else nlpbridx_latch

  // cross instline inst branch predict logic "crosslineJump"
  // 
  // if "crosslineJump", icache will need to fetch next instline, then fetch redirect addr
  // "crosslineJump" mechanism is used to speed up such code:
  // ```
  // 000c BranchCondition (32 bit inst)
  // ```
  // in this case, full inst is needed by BRU to get the right branch result, 
  // so we need to fetch the next inst line to get the higher bits of a 32bit branch inst
  // but in order to use BP result to avoid pipeline flush,
  // the next inst provided by icache should be predicted npc, instead of sequential npc
  //
  // There is another way: BPU uses "high 16 bit half" pc to index not-compressed crossline branch insts.

  val crosslineJump = nlp.io.crosslineJump
  val s_idle :: s_crosslineJump :: Nil = Enum(2)
  val state = RegInit(s_idle) 
  switch(state){
    is(s_idle){
      when(pcUpdate && crosslineJump && !io.redirect.valid){ state := s_crosslineJump }
    }
    is(s_crosslineJump){
      when(pcUpdate){ state := s_idle }
      when(io.redirect.valid){ state := s_idle }
    }
  }
  val crosslineJumpTarget = RegEnable(nlp.io.out.target, crosslineJump && pcUpdate)

  // predicted next pc
  val pnpc = Mux(crosslineJump, snpc, bpuTarget)
 
  // next pc
  val npc = Wire(UInt(VAddrBits.W))
  npc := Mux(io.redirect.valid, io.redirect.target, Mux(state === s_crosslineJump, crosslineJumpTarget, Mux(bpuValid, pnpc, snpc)))
  // val npcIsSeq = Mux(io.redirect.valid , false.B, Mux(state === s_crosslineJump, false.B, Mux(crosslineJump, true.B, Mux(nlp.io.out.valid, false.B, true.B)))) //for debug only

  // instValid: which part of an instline contains an valid inst
  // e.g. 1100 means inst(s) in instline(63,32) is/are valid
  val npcInstValid = Wire(UInt(4.W))
  def genInstValid(pc: UInt) = LookupTree(pc(2,1), List(
    "b00".U -> "b1111".U,
    "b01".U -> "b1110".U,
    "b10".U -> "b1100".U,
    "b11".U -> "b1000".U
  ))
  npcInstValid := Mux(crosslineJump && !(state === s_crosslineJump) && !io.redirect.valid, "b0001".U, genInstValid(npc))

  // branch position index, 4 bit vector
  // e.g. brIdx 0010 means a branch is predicted/assigned at pc (offset 2)
  val brIdx = Wire(UInt(4.W))
  // predicted branch position index, 4 bit vector
  val pbrIdx = bpuBrIdx | (crosslineJump << 3)
  brIdx := Mux(io.redirect.valid, 0.U, Mux(state === s_crosslineJump, 0.U, pbrIdx))
  
  // BP will be disabled shortly after a redirect request
  nlp.io.in.pc.valid := io.imem.req.fire // only predict when Icache accepts a request
  nlp.io.in.pc.bits := npc  // predict one cycle early
  nlp.io.flush := io.redirect.valid // redirect means BPU may need to be updated

  // Multi-cycle branch predictor
  // Multi-cycle branch predictor will not be synthesized if EnableMultiCyclePredictor is set to false
  val mcp = Module(new DummyPredicter)
  mcp.io.in.pc.valid := io.imem.req.fire
  mcp.io.in.pc.bits := pc
  mcp.io.flush := io.redirect.valid
  mcp.io.ignore := state === s_crosslineJump

  class MCPResult extends NutCoreBundle{
    val redirect = new RedirectIO
    val brIdx = Output(Vec(4, Bool()))
  }
  val mcpResultQueue = Module(new Queue(new MCPResult, entries = 4, pipe = true, flow = true, hasFlush = true))
  mcpResultQueue.io.flush.get := io.redirect.valid || io.bpFlush
  mcpResultQueue.io.enq.valid := mcp.io.valid
  mcpResultQueue.io.enq.bits.redirect := mcp.io.out
  mcpResultQueue.io.enq.bits.brIdx := mcp.io.brIdx
  mcpResultQueue.io.deq.ready := io.imem.resp.fire

  val validMCPRedirect = 
    mcpResultQueue.io.deq.bits.redirect.valid && //mcp predicts branch
    mcpResultQueue.io.deq.bits.brIdx.asUInt =/= io.imem.resp.bits.user.get(VAddrBits*2 + 3, VAddrBits*2) //brIdx is different
    (mcpResultQueue.io.deq.bits.brIdx.asUInt & io.imem.resp.bits.user.get(VAddrBits*2 + 7, VAddrBits*2 + 4)).orR //mcp reports a valid branch


  //val bp2 = Module(new BPU_nodelay)
  //bp2.io.in.bits := io.out.bits
  //bp2.io.in.valid := io.imem.resp.fire

  when (pcUpdate) { 
    pc := npc
    pcInstValid := npcInstValid
    // pcBrIdx := brIdx // just for debug
    // printf("[IF1] pc=%x\n", pc)
  }

  Debug(pcUpdate, "[IFUIN] pc:%x pcUpdate:%d npc:%x RedValid:%d RedTarget:%x LJL:%d LJTarget:%x LJ:%d snpc:%x bpValid:%d pnpc:%x \n",
        pc, pcUpdate, npc, io.redirect.valid, io.redirect.target,state === s_crosslineJump, crosslineJumpTarget, 
        crosslineJump,snpc,nlp.io.out.valid,nlp.io.out.target)

  io.flushVec := Mux(io.redirect.valid, Mux(io.redirect.rtype === 0.U, "b1111".U, "b0011".U), 0.U)
  io.bpFlush := false.B

  val icacheUserGen = Wire(new ICacheUserBundle)
  icacheUserGen.pc := pc
  icacheUserGen.pnpc := Mux(crosslineJump, nlp.io.out.target, npc)
  icacheUserGen.brIdx := brIdx & pcInstValid
  icacheUserGen.instValid := pcInstValid

  io.imem.req.bits.apply(addr = Cat(pc(VAddrBits-1,1),0.U(1.W)), //cache will treat it as Cat(pc(63,3),0.U(3.W))
    size = "b11".U, cmd = SimpleBusCmd.read, wdata = 0.U, wmask = 0.U, user = icacheUserGen.asUInt)
  io.imem.req.valid := io.out.ready
  //TODO: add ctrlFlow.exceptionVec
  io.imem.resp.ready := io.out.ready || io.flushVec(0)

  io.out.bits := DontCare
    //inst path only uses 32bit inst, get the right inst according to pc(2)

  Debug(io.imem.req.fire, "[IFI] pc=%x user=%x redirect %x pcInstValid %b brIdx %b npc %x pc %x pnpc %x\n", io.imem.req.bits.addr, io.imem.req.bits.user.getOrElse(0.U), io.redirect.valid, pcInstValid.asUInt, (pcInstValid & brIdx).asUInt, npc, pc, nlp.io.out.target)
  Debug(io.out.fire, "[IFO] pc=%x user=%x inst=%x npc=%x bridx %b valid %b ipf %x\n", io.out.bits.pc, io.imem.resp.bits.user.get, io.out.bits.instr, io.out.bits.pnpc, io.out.bits.brIdx.asUInt, io.out.bits.instValid.asUInt, io.ipf)

  // io.out.bits.instr := (if (XLEN == 64) io.imem.resp.bits.rdata.asTypeOf(Vec(2, UInt(32.W)))(io.out.bits.pc(2))
                      //  else io.imem.resp.bits.rdata)
  io.out.bits.instr := io.imem.resp.bits.rdata
  io.out.bits.pc := io.imem.resp.bits.user.get.asTypeOf(new ICacheUserBundle).pc
  io.out.bits.pnpc := io.imem.resp.bits.user.get.asTypeOf(new ICacheUserBundle).pnpc
  io.out.bits.brIdx := io.imem.resp.bits.user.get.asTypeOf(new ICacheUserBundle).brIdx
  io.out.bits.instValid := io.imem.resp.bits.user.get.asTypeOf(new ICacheUserBundle).instValid

  io.out.bits.icachePF := io.ipf
  // assert(!io.out.bits.icachePF)
  io.out.valid := io.imem.resp.valid && !io.flushVec(0)

  if(EnableMultiCyclePredictor){
    when(validMCPRedirect && !io.redirect.valid){
      io.out.bits.pnpc := mcpResultQueue.io.deq.bits.redirect.target
      io.out.bits.brIdx := mcpResultQueue.io.deq.bits.brIdx.asUInt
      npc := mcpResultQueue.io.deq.bits.redirect.target
      pcUpdate := true.B
      state := s_idle
      io.bpFlush := true.B      // flush imem
    }
  }

  // Illegal branch predict check
  val FixInvalidBranchPredict = false

  def preDecodeIsBranch(x: UInt) = {
    require(x.getWidth == 16)
    val res :: Nil = ListLookup(x, List(false.B), PreDecode.branchTable)
    res
  }

  if(FixInvalidBranchPredict){
    val maybeBranch = Wire(Vec(4, Bool()))
    val brIdxByPredictor = Mux(validMCPRedirect, mcpResultQueue.io.deq.bits.brIdx.asUInt, io.imem.resp.bits.user.get(VAddrBits*2 + 3, VAddrBits*2))
    (0 until 4).map(i => maybeBranch(i) := preDecodeIsBranch(io.out.bits.instr(16*(i+1)-1, 16*i))) //TODO: use icache pre-decode result
    // When branch predicter set non-sequential npc for a non-branch inst,
    // flush IFU, fetch sequential inst instead.
    when((brIdxByPredictor & ~maybeBranch.asUInt).orR && io.out.fire){
      Debug("[ERROR] FixInvalidBranchPredict\n")
      io.bpFlush := true.B
      io.out.bits.brIdx := 0.U
      npc := io.out.bits.pc + 8.U
      pcUpdate := true.B
    }
    // TODO: update BPU
  }

  BoringUtils.addSource(BoolStopWatch(io.imem.req.valid, io.imem.resp.fire), "perfCntCondMimemStall")
  BoringUtils.addSource(io.flushVec.orR, "perfCntCondMifuFlush")
}

class IFU_embedded extends NutCoreModule with HasResetVector {
  val io = IO(new Bundle {
    val imem = new SimpleBusUC(userBits = 64, addrBits = VAddrBits)
    val out = Decoupled(new CtrlFlowIO)
    val redirect = Flipped(new RedirectIO)
    val flushVec = Output(UInt(4.W))
    val bpFlush = Output(Bool())
    val ipf = Input(Bool())
  })

  // pc
  val pc = RegInit(resetVector.U(32.W))
  val pcUpdate = io.redirect.valid || io.imem.req.fire
  val snpc = pc + 4.U  // sequential next pc

  val bpu = Module(new BPU_embedded)

  // predicted next pc
  val pnpc = bpu.io.out.target
  val npc = Mux(io.redirect.valid, io.redirect.target, Mux(bpu.io.out.valid, pnpc, snpc))
  
  bpu.io.in.pc.valid := io.imem.req.fire // only predict when Icache accepts a request
  bpu.io.in.pc.bits := npc  // predict one cycle early
  bpu.io.flush := io.redirect.valid

  when (pcUpdate) { pc := npc }

  io.flushVec := Mux(io.redirect.valid, "b1111".U, 0.U)
  io.bpFlush := false.B

  io.imem := DontCare
  io.imem.req.bits.apply(addr = pc, size = "b10".U, cmd = SimpleBusCmd.read, wdata = 0.U, wmask = 0.U, user = Cat(pc, npc))
  io.imem.req.valid := io.out.ready
  io.imem.resp.ready := io.out.ready || io.flushVec(0)

  io.out.bits := DontCare
  io.out.bits.instr := io.imem.resp.bits.rdata
  io.imem.resp.bits.user.map{ case x =>
    io.out.bits.pc := x(2*VAddrBits-1, VAddrBits)
    io.out.bits.pnpc := x(VAddrBits-1, 0)
  }
  io.out.valid := io.imem.resp.valid && !io.flushVec(0)

  Debug(io.imem.req.fire, "[IFI] pc=%x user=%x redirect %x npc %x pc %x pnpc %x\n", io.imem.req.bits.addr, io.imem.req.bits.user.getOrElse(0.U), io.redirect.valid, npc, pc, bpu.io.out.target)
  Debug(io.out.fire, "[IFO] pc=%x user=%x inst=%x npc=%x ipf %x\n", io.out.bits.pc, io.imem.resp.bits.user.get, io.out.bits.instr, io.out.bits.pnpc, io.ipf)

  BoringUtils.addSource(BoolStopWatch(io.imem.req.valid, io.imem.resp.fire), "perfCntCondMimemStall")
  BoringUtils.addSource(io.flushVec.orR, "perfCntCondMifuFlush")
}

class IFU_inorder extends NutCoreModule with HasResetVector {
  val io = IO(new Bundle {

    val imem = new SimpleBusUC(userBits = VAddrBits*2 + 4, addrBits = VAddrBits)
    val out = Decoupled(new CtrlFlowIO)

    val redirect = Flipped(new RedirectIO)
    val flushVec = Output(UInt(4.W))
    val bpFlush = Output(Bool())
    val ipf = Input(Bool())
  })

  // pc
  val pc = RegInit(resetVector.U(VAddrBits.W))
  val pcUpdate = io.redirect.valid || io.imem.req.fire
  val snpc = Mux(pc(1), pc + 2.U, pc + 4.U)  // sequential next pc

  val bp1 = Module(new BPU_inorder)

  val crosslineJump = bp1.io.crosslineJump
  val crosslineJumpLatch = RegInit(false.B) 
  when(pcUpdate || bp1.io.flush) {
    crosslineJumpLatch := Mux(bp1.io.flush, false.B, crosslineJump && !crosslineJumpLatch)
  }
  val crosslineJumpTarget = RegEnable(bp1.io.out.target, crosslineJump)
  val crosslineJumpForceSeq = crosslineJump && bp1.io.out.valid
  val crosslineJumpForceTgt = crosslineJumpLatch && !bp1.io.flush

  // predicted next pc
  val pnpc = Mux(crosslineJump, snpc, bp1.io.out.target)
  val pbrIdx = bp1.io.brIdx
  val npc = Mux(io.redirect.valid, io.redirect.target, Mux(crosslineJumpLatch, crosslineJumpTarget, Mux(bp1.io.out.valid, pnpc, snpc)))
  val npcIsSeq = Mux(io.redirect.valid , false.B, Mux(crosslineJumpLatch, false.B, Mux(crosslineJump, true.B, Mux(bp1.io.out.valid, false.B, true.B))))
  // Debug("[NPC] %x %x %x %x %x %x\n",crosslineJumpLatch, crosslineJumpTarget, crosslineJump, bp1.io.out.valid, pnpc, snpc)

  // val npc = Mux(io.redirect.valid, io.redirect.target, Mux(io.redirectRVC.valid, io.redirectRVC.target, snpc))
  val brIdx = Wire(UInt(4.W)) 
  // brIdx(0) -> branch at pc offset 0 (mod 4)
  // brIdx(1) -> branch at pc offset 2 (mod 4)
  // brIdx(2) -> branch at pc offset 6 (mod 8), and this inst is not rvc inst
  brIdx := Cat(npcIsSeq, Mux(io.redirect.valid, 0.U, pbrIdx))
  //TODO: BP will be disabled shortly after a redirect request

  bp1.io.in.pc.valid := io.imem.req.fire // only predict when Icache accepts a request
  bp1.io.in.pc.bits := npc  // predict one cycle early

  // Debug(bp1.io.in.pc.valid, p"pc: ${Hexadecimal(pc)} npc: ${Hexadecimal(npc)}\n")
  // Debug(bp1.io.out.valid, p"valid!!\n")

  bp1.io.flush := io.redirect.valid

  when (pcUpdate) { 
    pc := npc 
    // printf("[IF1] pc=%x\n", pc)
  }

  Debug(pcUpdate, "[IFUPC] pc:%x pcUpdate:%d npc:%x RedValid:%d RedTarget:%x LJL:%d LJTarget:%x LJ:%d snpc:%x bpValid:%d pnpn:%x \n",pc, pcUpdate, npc, io.redirect.valid,io.redirect.target,crosslineJumpLatch,crosslineJumpTarget,crosslineJump,snpc,bp1.io.out.valid,pnpc)

  io.flushVec := Mux(io.redirect.valid, "b1111".U, 0.U)
  io.bpFlush := false.B

  io.imem.req.bits.apply(addr = Cat(pc(VAddrBits-1,1),0.U(1.W)), //cache will treat it as Cat(pc(63,3),0.U(3.W))
    size = "b11".U, cmd = SimpleBusCmd.read, wdata = 0.U, wmask = 0.U, user = Cat(brIdx(3,0), npc(VAddrBits-1, 0), pc(VAddrBits-1, 0)))
  io.imem.req.valid := io.out.ready
  //TODO: add ctrlFlow.exceptionVec
  io.imem.resp.ready := io.out.ready || io.flushVec(0)

  io.out.bits := DontCare
    //inst path only uses 32bit inst, get the right inst according to pc(2)

  Debug(io.imem.req.fire, "[IFI] pc=%x user=%x %x %x %x \n", io.imem.req.bits.addr, io.imem.req.bits.user.getOrElse(0.U), io.redirect.valid, pbrIdx, brIdx)
  Debug(io.out.fire, "[IFO] pc=%x inst=%x\n", io.out.bits.pc, io.out.bits.instr)

  // io.out.bits.instr := (if (XLEN == 64) io.imem.resp.bits.rdata.asTypeOf(Vec(2, UInt(32.W)))(io.out.bits.pc(2))
                      //  else io.imem.resp.bits.rdata)
  io.out.bits.instr := io.imem.resp.bits.rdata
  io.imem.resp.bits.user.map{ case x =>
    io.out.bits.pc := x(VAddrBits-1,0)
    io.out.bits.pnpc := x(VAddrBits*2-1,VAddrBits)
    io.out.bits.brIdx := x(VAddrBits*2 + 3, VAddrBits*2)
  }
  io.out.bits.exceptionVec(instrPageFault) := io.ipf
  io.out.valid := io.imem.resp.valid && !io.flushVec(0)

  BoringUtils.addSource(BoolStopWatch(io.imem.req.valid, io.imem.resp.fire), "perfCntCondMimemStall")
  BoringUtils.addSource(io.flushVec.orR, "perfCntCondMifuFlush")
}