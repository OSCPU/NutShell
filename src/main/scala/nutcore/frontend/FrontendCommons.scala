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

package nutcore.frontend

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import nutcore._
import nutcore.frontend.decode._
import nutcore.frontend.instr_fetch._

import utils._
import bus.simplebus._
import chisel3.experimental.IO

class FrontendIO(implicit val p: NutCoreConfig) extends Bundle with HasNutCoreConst {
  val imem = new SimpleBusUC(userBits = ICacheUserBundleWidth, addrBits = VAddrBits)
  val out = Vec(2, Decoupled(new DecodeIO))
  val flushVec = Output(UInt(4.W))
  val redirect = Flipped(new RedirectIO)
  val bpFlush = Output(Bool())
  val ipf = Input(Bool())
}


trait HasFrontendIO {
  implicit val p: NutCoreConfig
  val io = IO(new FrontendIO)
}


