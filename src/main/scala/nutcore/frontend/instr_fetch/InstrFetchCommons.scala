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
