package noop

import chisel3._
import chisel3.util._

class CtrlSignalIO extends NOOPBundle {
  val src1Type = Output(SrcType())
  val src2Type = Output(SrcType())
  val fuType = Output(FuType())
  val fuOpType = Output(FuOpType())
  val rfSrc1 = Output(UInt(5.W))
  val rfSrc2 = Output(UInt(5.W))
  val rfWen = Output(Bool())
  val rfDest = Output(UInt(5.W))
  val isNoopTrap = Output(Bool())
  val isSrc1Forward = Output(Bool())
  val isSrc2Forward = Output(Bool())
  val noSpecExec = Output(Bool())  // This inst is a branch inst, and this branch is speculated
  val isPipeLined = Output(Bool()) // Function unti for this inst is pipelined
  val isBlocked = Output(Bool())   // This inst requires pipeline to be blocked
}

class DataSrcIO extends NOOPBundle {
  val src1 = Output(UInt(XLEN.W))
  val src2 = Output(UInt(XLEN.W))
  val imm  = Output(UInt(XLEN.W))
}

class RedirectIO extends NOOPBundle {
  val target = Output(UInt(VAddrBits.W))
  val rtype = Output(UInt(1.W)) // 1: branch mispredict: only need to flush frontend  0: others: flush the whole pipeline
  val valid = Output(Bool())
}

class CtrlFlowIO extends NOOPBundle {
  val instr = Output(UInt(64.W))
  val pc = Output(UInt(VAddrBits.W))
  val pnpc = Output(UInt(VAddrBits.W))
  val redirect = new RedirectIO
  val exceptionVec = Output(Vec(16, Bool()))
  val intrVec = Output(Vec(12, Bool()))
  val brIdx = Output(Bool())
  val instValid = Output(UInt(4.W))
  val crossPageIPFFix = Output(Bool())
}

class DecodeIO extends NOOPBundle {
  val cf = new CtrlFlowIO
  val ctrl = new CtrlSignalIO
  val data = new DataSrcIO
  val pipeline2 = Output(Bool())
}

class WriteBackIO extends NOOPBundle {
  val rfWen = Output(Bool())
  val rfDest = Output(UInt(5.W))
  val rfData = Output(UInt(XLEN.W))
}

class CommitIO extends NOOPBundle {
  val decode = new DecodeIO
  val isMMIO = Output(Bool())
  val intrNO = Output(UInt(XLEN.W))
  val commits = Output(Vec(FuType.num, UInt(XLEN.W)))
}

class OOCommitIO extends NOOPBundle with HasBackendConst{
  val decode = new DecodeIO
  val isMMIO = Output(Bool())
  val intrNO = Output(UInt(XLEN.W))
  val commits = Output(UInt(XLEN.W))
  val prfidx = Output(UInt(prfAddrWidth.W)) //also as robidx
  val exception = Output(Bool())
}

class FunctionUnitIO extends NOOPBundle {
  val in = Flipped(Decoupled(new Bundle {
    val src1 = Output(UInt(XLEN.W))
    val src2 = Output(UInt(XLEN.W))
    val func = Output(FuOpType())
  }))
  val out = Decoupled(Output(UInt(XLEN.W)))
}

class ForwardIO extends NOOPBundle {
  val valid = Output(Bool())
  val wb = new WriteBackIO
  val fuType = Output(FuType())
}

class MMUIO extends NOOPBundle {
  // val ptev = Output(Bool())
  // val pteu = Output(Bool())
  // val ptex = Output(Bool())
  // val valid = Output(Bool())
  // val isStore = Output(Bool())

  val priviledgeMode = Input(UInt(2.W))
  val status_sum = Input(Bool())
  val status_mxr = Input(Bool())

  val loadPF = Output(Bool())
  val storePF = Output(Bool())
  val addr = Output(UInt(VAddrBits.W)) 
  
  def isPF() = loadPF || storePF
}

class MemMMUIO extends NOOPBundle {
  val imem = new MMUIO
  val dmem = new MMUIO
}

class TLBExuIO extends NOOPBundle {
  val satp = Output(UInt(XLEN.W))
  val sfence = new Bundle {
    val valid = Output(Bool())
    val asid  = Output(UInt(9.W))
    val vaddr = Output(UInt(XLEN.W))
  }

  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt, satp: UInt) = {//func no use here for just sfence.vma only
    this.sfence.valid := valid
    this.sfence.vaddr := src1
    this.sfence.asid  := src2(8,0)
    this.satp := satp
  }
}

class InstFetchIO extends NOOPBundle {
  val pc = Output(UInt(VAddrBits.W)) // real PC will be regenerated in IBF 
  val pnpc = Output(UInt(VAddrBits.W))
  val brIdx = Output(UInt(4.W))
  val instValid = Output(UInt(4.W))
  //above will be used as user bits in icache
  val icachePF = Output(Bool())
  val instr = Output(UInt(64.W))
}

class RenamedDecodeIO extends NOOPBundle with HasBackendConst {
  val decode = new DecodeIO
  val prfDest = Output(UInt(prfAddrWidth.W))
  val prfSrc1 = Output(UInt(prfAddrWidth.W))
  val prfSrc2 = Output(UInt(prfAddrWidth.W))
  val src1Rdy = Output(Bool())
  val src2Rdy = Output(Bool())
}