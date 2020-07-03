package nutshell

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import bus.simplebus._
import top.Settings

class EXU(implicit val p: NOOPConfig) extends NOOPModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(Vec(2, new DecodeIO)))
    val out = Decoupled(Vec(2, new CommitIO))
    val flush = Input(Bool())
    val dmem = new SimpleBusUC(addrBits = VAddrBits)
    val forward = Vec(2, new ForwardIO)
    val memMMU = Flipped(new MemMMUIO)
  })

  val src1 = io.in.bits(0).data.src1(XLEN-1,0)
  val src2 = io.in.bits(0).data.src2(XLEN-1,0)
  val src3 = io.in.bits(1).data.src1(XLEN-1,0)
  val src4 = io.in.bits(1).data.src2(XLEN-1,0)

  val pipeline2valid = io.in.valid && !io.flush && io.in.bits(1).pipeline2 && EnableSuperScalarExec.B

  val (fuType, fuOpType) = (io.in.bits(0).ctrl.fuType, io.in.bits(0).ctrl.fuOpType)

  val fuValids = Wire(Vec(FuType.num, Bool()))
  (0 until FuType.num).map (i => fuValids(i) := (fuType === i.U) && io.in.valid && !io.flush)

  val alu = Module(new ALU(hasBru = true))
  val aluOut = alu.access(valid = fuValids(FuType.alu), src1 = src1, src2 = src2, func = fuOpType)
  alu.io.cfIn := io.in.bits(0).cf
  alu.io.offset := io.in.bits(0).data.imm
  alu.io.out.ready := true.B

  def isBru(func: UInt) = func(4)
  val alu2 = Module(new ALU)
  val alu2Out = alu2.access(valid = pipeline2valid, src1 = src3, src2 = src4, func = io.in.bits(1).ctrl.fuOpType)
  alu2.io.cfIn := io.in.bits(1).cf
  alu2.io.offset := io.in.bits(1).data.imm
  alu2.io.out.ready := true.B

  val lsu = Module(new UnpipelinedLSU)
  val lsuTlbPF = WireInit(false.B)
  val lsuOut = lsu.access(valid = fuValids(FuType.lsu), src1 = src1, src2 = io.in.bits(0).data.imm, func = fuOpType, dtlbPF = lsuTlbPF)
  lsu.io.wdata := src2
  lsu.io.instr := io.in.bits(0).cf.instr
  io.out.bits(0).isMMIO := lsu.io.isMMIO || (AddressSpace.isMMIO(io.in.bits(0).cf.pc) && io.out.valid)
  io.dmem <> lsu.io.dmem
  lsu.io.out.ready := true.B

  val mdu = Module(new MDU)
  val mduOut = mdu.access(valid = fuValids(FuType.mdu), src1 = src1, src2 = src2, func = fuOpType)
  mdu.io.out.ready := true.B

  // val csr = if (Settings.get("MmodeOnly")) Module(new CSR_M) else Module(new CSR)
  val csr = Module(new CSR)
  val csrOut = csr.access(valid = fuValids(FuType.csr), src1 = src1, src2 = src2, func = fuOpType)
  csr.io.cfIn := io.in.bits(0).cf
  csr.io.cfIn.exceptionVec(loadAddrMisaligned) := lsu.io.loadAddrMisaligned
  csr.io.cfIn.exceptionVec(storeAddrMisaligned) := lsu.io.storeAddrMisaligned
  csr.io.instrValid := io.in.valid && !io.flush
  csr.io.isBackendException := false.B
  io.out.bits(0).intrNO := csr.io.intrNO
  csr.io.isBackendException := false.B
  csr.io.out.ready := true.B

  csr.io.imemMMU <> io.memMMU.imem
  csr.io.dmemMMU <> io.memMMU.dmem

  val mou = Module(new MOU)
  // mou does not write register
  mou.access(valid = fuValids(FuType.mou), src1 = src1, src2 = src2, func = fuOpType)
  mou.io.cfIn := io.in.bits(0).cf
  mou.io.out.ready := true.B
  
  io.out.bits(1) := DontCare
  io.out.bits(0).decode := DontCare
  (io.out.bits(0).decode.ctrl, io.in.bits(0).ctrl) match { case (o, i) =>
    o.rfWen := i.rfWen && (!lsuTlbPF && !lsu.io.loadAddrMisaligned && !lsu.io.storeAddrMisaligned || !fuValids(FuType.lsu)) && !(csr.io.wenFix && fuValids(FuType.csr))
    o.rfDest := i.rfDest
    o.fuType := i.fuType
  }
  io.out.bits(1).decode.ctrl := io.in.bits(1).ctrl
  
  io.out.bits(0).decode.cf.pc := io.in.bits(0).cf.pc
  io.out.bits(1).decode.cf.pc := io.in.bits(1).cf.pc

  io.out.bits(0).decode.cf.instr := io.in.bits(0).cf.instr
  io.out.bits(1).decode.cf.instr := io.in.bits(1).cf.instr

  io.out.bits(1).decode.pipeline2 := io.in.bits(1).pipeline2

  io.out.bits(0).decode.cf.redirect <>
    Mux(mou.io.redirect.valid, mou.io.redirect,
      Mux(csr.io.redirect.valid, csr.io.redirect, alu.io.redirect))
  io.out.bits(1).decode.cf.redirect.valid := false.B

  Debug(){
    when(mou.io.redirect.valid || csr.io.redirect.valid || alu.io.redirect.valid){
      printf("[REDIRECT] mou %x csr %x alu %x \n", mou.io.redirect.valid, csr.io.redirect.valid, alu.io.redirect.valid)
      printf("[REDIRECT] flush: %d mou %x csr %x alu %x\n", io.flush, mou.io.redirect.target, csr.io.redirect.target, alu.io.redirect.target)
    }
  }

  // FIXME: should handle io.out.ready == false
  io.out.valid := io.in.valid && MuxLookup(fuType, true.B, List(
    FuType.lsu -> lsu.io.out.valid,
    FuType.mdu -> mdu.io.out.valid
  ))

  io.out.bits(0).commits(FuType.alu) := aluOut
  io.out.bits(0).commits(FuType.lsu) := lsuOut
  io.out.bits(0).commits(FuType.csr) := csrOut
  io.out.bits(0).commits(FuType.mdu) := mduOut
  io.out.bits(0).commits(FuType.mou) := 0.U

  io.out.bits(1).commits(FuType.alu) := alu2Out

  io.in.ready := !io.in.valid || io.out.fire()

  io.forward(0).valid := io.in.valid
  io.forward(0).wb.rfWen := io.in.bits(0).ctrl.rfWen
  io.forward(0).wb.rfDest := io.in.bits(0).ctrl.rfDest
  io.forward(0).wb.rfData := Mux(alu.io.out.fire(), aluOut, lsuOut)
  io.forward(0).fuType := io.in.bits(0).ctrl.fuType

  io.forward(1).valid := pipeline2valid
  io.forward(1).wb.rfWen := io.in.bits(1).ctrl.rfWen
  io.forward(1).wb.rfDest := io.in.bits(1).ctrl.rfDest
  io.forward(1).wb.rfData := alu2Out
  io.forward(1).fuType := FuType.alu //io.in.bits(1).ctrl.fuType

  val isBru = ALUOpType.isBru(fuOpType)
  BoringUtils.addSource(alu.io.out.fire() && !isBru, "perfCntCondMaluInstr")
  BoringUtils.addSource(alu.io.out.fire() && isBru, "perfCntCondMbruInstr")
  BoringUtils.addSource(lsu.io.out.fire(), "perfCntCondMlsuInstr")
  BoringUtils.addSource(mdu.io.out.fire(), "perfCntCondMmduInstr")
  BoringUtils.addSource(csr.io.out.fire(), "perfCntCondMcsrInstr")

  if (!p.FPGAPlatform) {
    val mon = Module(new Monitor)
    val cycleCnt = WireInit(0.U(64.W))
    val instrCnt = WireInit(0.U(64.W))
    val nooptrap = io.in.bits(0).ctrl.isNoopTrap && io.in.valid
    mon.io.clk := clock
    mon.io.reset := reset.asBool
    mon.io.isNoopTrap := nooptrap
    mon.io.trapCode := io.in.bits(0).data.src1
    mon.io.trapPC := io.in.bits(0).cf.pc
    mon.io.cycleCnt := cycleCnt
    mon.io.instrCnt := instrCnt

    BoringUtils.addSink(cycleCnt, "simCycleCnt")
    BoringUtils.addSink(instrCnt, "simInstrCnt")
    BoringUtils.addSource(nooptrap, "nooptrap")
  }
}
