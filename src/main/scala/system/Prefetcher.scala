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

package system

import nutcore.{NutCore, NutCoreConfig, HasNutCoreParameter, AddressSpace, Cache, CacheConfig}
import bus.axi4.{AXI4, AXI4Lite}
import bus.simplebus._
import utils._

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

trait HasPrefetcherParameter extends HasNutCoreParameter {
  val supportPrefetch = HasDcache
}

class Prefetcher extends Module with HasPrefetcherParameter {
	val io = IO(new Bundle {
		val in = Flipped(Decoupled(new SimpleBusReqBundle))
		val out = Decoupled(new SimpleBusReqBundle)
	})
  val getNewReq = RegInit(false.B)
  val prefetchReq = RegNext(io.in.bits)
  prefetchReq.cmd := SimpleBusCmd.prefetch
  prefetchReq.addr := io.in.bits.addr + XLEN.U

  //lastReqAddr not be initted, in vivado simulation maybe fail
  //val lastReqAddr = (RegEnable(io.in.bits.addr, io.in.fire()))
  val lastReqAddr = RegInit(0.U(AddrBits.W))
  when (io.in.fire()) {
     lastReqAddr := io.in.bits.addr
  }
  val thisReqAddr = io.in.bits.addr
  val lineMask = Cat(Fill(AddrBits - 6, 1.U(1.W)), 0.U(6.W))
  val neqAddr = (thisReqAddr & lineMask) =/= (lastReqAddr & lineMask)

  when (!getNewReq) {
    io.out.bits <> io.in.bits
    io.out.valid := io.in.valid
    io.in.ready := !io.in.valid || io.out.fire()
    getNewReq := io.in.fire() && io.in.bits.isBurst() && neqAddr
  }.otherwise {
    io.out.bits <> prefetchReq
    io.out.valid := !AddressSpace.isMMIO(prefetchReq.addr)
    io.in.ready := false.B
    getNewReq := !(io.out.fire() || AddressSpace.isMMIO(prefetchReq.addr))
  }
  
  Debug() {
    printf("%d: [Prefetcher]: in(%d,%d), out(%d,%d), in.bits.addr = %x\n",
      GTimer(), io.in.valid, io.in.ready, io.out.valid, io.out.ready, io.in.bits.addr)
  }
}
