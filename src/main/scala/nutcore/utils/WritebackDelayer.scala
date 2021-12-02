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

import utils._

class WritebackDelayer(bru: Boolean = false, name: String = "unnamedDelayer") extends NutCoreModule with HasRSConst with HasBackendConst {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new OOCommitIO))
    val out = Decoupled(new OOCommitIO)
    val mispredictRec = Flipped(new MisPredictionRecIO)
    val flush = Input(Bool())
    val checkpointIn = if (bru) Some(Input(UInt(brTagWidth.W))) else None
    val freeCheckpoint = if (bru) Some(Output(Valid(UInt(brTagWidth.W)))) else None
  })

  val valid = RegInit(false.B)
  val brMask = Reg(UInt(checkpointSize.W))
  
  def needMispredictionRecovery(brMask: UInt) = {
    io.mispredictRec.valid && io.mispredictRec.redirect.valid && brMask(io.mispredictRec.checkpoint)
  }

  def updateBrMask(brMask: UInt) = {
    brMask & ~ (UIntToOH(io.mispredictRec.checkpoint) & Fill(checkpointSize, io.mispredictRec.valid))
  }

  brMask := updateBrMask(brMask)
  when(io.in.fire){brMask := updateBrMask(io.in.bits.brMask)}
  when(needMispredictionRecovery(brMask) || io.out.fire){valid := false.B}
  when(io.in.fire){valid := true.B}
  when(io.flush) {valid := false.B}

  io.in.ready := (!valid || io.out.fire) && !needMispredictionRecovery(io.in.bits.brMask)
  io.out.bits <> RegEnable(io.in.bits, io.in.fire)
  io.out.bits.brMask := brMask
  io.out.valid := valid

  if(bru){
    io.freeCheckpoint.get.bits <> RegEnable(io.checkpointIn.get, io.in.fire)
    io.freeCheckpoint.get.valid := io.out.fire
  }

  Debug(valid, "[WBDelay-"+name+"] delayer valid: pc %x brMask %x\n", io.out.bits.decode.cf.pc, brMask)
}


object WritebackDelayer {
  def apply(in: Data, mispredictRec: Data, flush: Bool) = {
    val delayer = Module(new WritebackDelayer())
    delayer.io.in := in
    delayer.io.mispredictRec := mispredictRec
    delayer.io.flush := flush
    delayer
  }
}
