package nutcore.frontend.instr_fetch

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import nutcore._
import nutcore.frontend.instr_fetch.branch_predict._

import utils._
import bus.simplebus._
import top.Settings
import difftest._

class InstrFetchEmbedded extends NutCoreModule with HasResetVector {
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
  val pcUpdate = io.redirect.valid || io.imem.req.fire()
  val snpc = pc + 4.U  // sequential next pc

  val bpu = Module(new BranchPredictEmbedded)

  // predicted next pc
  val pnpc = bpu.io.out.target
  val npc = Mux(io.redirect.valid, io.redirect.target, Mux(bpu.io.out.valid, pnpc, snpc))
  
  bpu.io.in.pc.valid := io.imem.req.fire() // only predict when Icache accepts a request
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

  Debug(io.imem.req.fire(), "[IFI] pc=%x user=%x redirect %x npc %x pc %x pnpc %x\n", io.imem.req.bits.addr, io.imem.req.bits.user.getOrElse(0.U), io.redirect.valid, npc, pc, bpu.io.out.target)
  Debug(io.out.fire(), "[IFO] pc=%x user=%x inst=%x npc=%x ipf %x\n", io.out.bits.pc, io.imem.resp.bits.user.get, io.out.bits.instr, io.out.bits.pnpc, io.ipf)

  BoringUtils.addSource(BoolStopWatch(io.imem.req.valid, io.imem.resp.fire()), "perfCntCondMimemStall")
  BoringUtils.addSource(io.flushVec.orR, "perfCntCondMifuFlush")
}
