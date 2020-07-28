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

package bus.simplebus

import chisel3._
import chisel3.util._

import utils._
import nutcore.HasNutCoreParameter

class SimpleBusAddressMapper(map: (Int, Long)) extends Module with HasNutCoreParameter {
  val io = IO(new Bundle {
    val in = Flipped(new SimpleBusUC)
    val out = new SimpleBusUC
  })

  io.out <> io.in
  val (regionBits, rebase) = (map._1, map._2.U(PAddrBits.W))
  if (regionBits != 0) {
    io.out.req.bits.addr := Cat(rebase(PAddrBits-1, regionBits), io.in.req.bits.addr(regionBits-1, 0))
  }
}
