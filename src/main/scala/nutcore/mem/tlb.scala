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

trait HasNBTlbConst extends HasNutCoreParameter with HasBackendConst{
  val Level = 3

  val offLen  = 12
  val ppnLen  = PAddrBits - offLen
  val vpnnLen = 9
  val vpnLen  = VAddrBits - offLen
  val flagLen = 8
  val pteResLen = XLEN - ppnLen - 2 - flagLen
  val asidLen = 16

  val RoqIdxWidth = log2Up(robSize)
  val LsroqIdxWidth = 1 // Temp
  val TlbEntrySize = 32

  def vaBundle = new Bundle {
    val vpn  = UInt(vpnLen.W)
    val off  = UInt(offLen.W)
  }
  def pteBundle = new Bundle {
    val reserved  = UInt(pteResLen.W)
    val ppn  = UInt(ppnLen.W)
    val rsw  = UInt(2.W)
    val perm = new Bundle {
      val d    = Bool()
      val a    = Bool()
      val g    = Bool()
      val u    = Bool()
      val x    = Bool()
      val w    = Bool()
      val r    = Bool()
      val v    = Bool()
    }
  }
}

abstract class NBTlbBundle extends NutCoreBundle with HasNBTlbConst
abstract class NBTlbModule extends NutCoreModule with HasNBTlbConst

class PermBundle(val hasV: Boolean = true) extends NBTlbBundle {
  val d = Bool()
  val a = Bool()
  val g = Bool()
  val u = Bool()
  val x = Bool()
  val w = Bool()
  val r = Bool()
  if (hasV) { val v = Bool() }

  override def toPrintable: Printable = {
    p"d:${d} a:${a} g:${g} u:${u} x:${x} w:${w} r:${r}"// + 
    //(if(hasV) (p"v:${v}") else p"")
  }
}

object ParallelOperation {
  def apply[T <: Data](nut: Seq[T], func: (T, T) => T): T = {
    nut match {
      case Seq(a) => a
      case Seq(a, b) => func(a, b)
      case _ =>
        apply(Seq(apply(nut take nut.size/2, func), apply(nut drop nut.size/2, func)), func)
    }
  }
}

object ParallelOR {
  def apply[T <: Data](nut: Seq[T]): T = {
    ParallelOperation(nut, (a: T, b: T) => (a.asUInt() | b.asUInt()).asTypeOf(nut.head))
  }
}

object ParallelAND {
  def apply[T <: Data](nut: Seq[T]): T = {
    ParallelOperation(nut, (a: T, b:T) => (a.asUInt() & b.asUInt()).asTypeOf(nut.head))
  }
}

object ParallelMux {
  def apply[T<:Data](in: Seq[(Bool, T)]): T = {
    val nut = in map { case (cond, x) => (Fill(x.getWidth, cond) & x.asUInt()).asTypeOf(in.head._2) }
    ParallelOR(nut)
  }
}

class comBundle extends NBTlbBundle {
  val valid = Bool()
  val roqIdx = UInt(1.W) // TODO
  val bits = new PtwReq
  def isPrior(that: comBundle): Bool = {
    (this.valid && !that.valid) || (this.valid && that.valid && (this.roqIdx <= that.roqIdx)/*tmp*/)
  }
}
object Compare {
  def apply[T<:Data](nut: Seq[comBundle]): comBundle = {
    ParallelOperation(nut, (a: comBundle, b: comBundle) => Mux(a isPrior b, a, b))
  }
}

class TlbEntry extends NBTlbBundle {
  val vpn = UInt(vpnLen.W) // tag is vpn
  val ppn = UInt(ppnLen.W)
  val level = UInt(log2Up(Level).W) // 0 for 4KB, 1 for 2MB, 2 for 1GB
  // val asid = UInt(asidLen.W), asid maybe expensive to support, but useless
  // val v = Bool() // v&g is special, may need sperate storage?
  val perm = new PermBundle(hasV = false)

  def vpnHit(vpn: UInt):Bool = {
    val fullMask = VecInit((Seq.fill(vpnLen)(true.B))).asUInt
    val maskLevel = VecInit((Level-1 to 0 by -1).map{i => // NOTE: level 2 for 4KB, 1 for 2MB, 0 for 1GB
      VecInit(Seq.fill(vpnLen-i*vpnnLen)(true.B) ++ Seq.fill(i*vpnnLen)(false.B)).asUInt})
    val mask = maskLevel(level)
    (mask&this.vpn) === (mask&vpn)
  }

  // def asidHit(asid: UInt) = {
  //   this.asid === asid
  // }

  def hit(vpn: UInt/*, asid: UInt*/):Bool = {
    vpnHit(vpn) // && asidHit(asid)
  }

  def genTlbEntry(pte: UInt, level: UInt, vpn: UInt/*, asid: UInt*/) = {
    val e = Wire(new TlbEntry)
    e.ppn := pte.asTypeOf(pteBundle).ppn
    e.level := level
    e.vpn := vpn
    e.perm := pte.asTypeOf(pteBundle).perm
    // e.asid := asid
    e
  }

  override def toPrintable: Printable = {
    p"vpn:0x${Hexadecimal(vpn)} ppn:0x${Hexadecimal(ppn)} level:${level} perm:${perm}"
  }
}

object TlbCmd {
  def read  = "b00".U
  def write = "b01".U
  def exec  = "b10".U

  def apply() = UInt(2.W)
  def isRead(a: UInt) = a===read
  def isWrite(a: UInt) = a===write
  def isExec(a: UInt) = a===exec
}

class TlbReq extends NBTlbBundle {
  val vaddr = UInt(VAddrBits.W)
  val cmd = TlbCmd()
  val roqIdx = UInt(RoqIdxWidth.W)
  val debug = new Bundle {
    val pc = UInt(XLEN.W)
    val lsroqIdx = UInt(LsroqIdxWidth.W)
  }

  override def toPrintable: Printable = {
    p"vaddr:0x${Hexadecimal(vaddr)} cmd:${cmd} pc:0x${Hexadecimal(debug.pc)} roqIdx:${roqIdx} lsroqIdx:${debug.lsroqIdx}"
  }
}

class TlbResp extends NBTlbBundle {
  val paddr = UInt(PAddrBits.W)
  val miss = Bool()
  val excp = new Bundle {
    val pf = new Bundle {
      val ld = Bool()
      val st = Bool()
      val instr = Bool()
    }
  }
  override def toPrintable: Printable = {
    p"paddr:0x${Hexadecimal(paddr)} miss:${miss} excp.pf: ld:${excp.pf.ld} st:${excp.pf.st} instr:${excp.pf.instr}"
  }
}

class TlbRequestIO() extends NBTlbBundle {
  val req = Valid(new TlbReq)
  val resp = Flipped(Valid(new TlbResp))

  // override def cloneType: this.type = (new TlbRequestIO(Width)).asInstanceOf[this.type]
}

class TlbPtwIO extends NBTlbBundle {
  val req = DecoupledIO(new PtwReq)
  val resp = Flipped(DecoupledIO(new PtwResp))
}

class TlbIO(Width: Int) extends NBTlbBundle {
  val requestor = Vec(Width, Flipped(new TlbRequestIO))
  val ptw = new TlbPtwIO

  override def cloneType: this.type = (new TlbIO(Width)).asInstanceOf[this.type]
}

// Non-block TLB
class NBTLB(Width: Int, isDtlb: Boolean)/*(implicit m: Module)*/ extends NBTlbModule with HasCSRConst{
  val io = IO(new TlbIO(Width))

  val req    = io.requestor.map(_.req)
  val resp   = io.requestor.map(_.resp)
  val ptw    = io.ptw

  val sfence = WireInit(0.U.asTypeOf(new SfenceBundle))
  val csr    = WireInit(0.U.asTypeOf(new TlbCsrBundle))
  val satp   = csr.satp
  val priv   = csr.priv
  val ifecth = if (isDtlb) false.B else true.B
  val mode   = if (isDtlb) priv.dmode else priv.imode
  val vmEnable = satp.mode === 8.U && (mode < ModeM) // NOTE: normal mode
  // val vmEnable = true.B // NOTE: tlb simple test use it
  BoringUtils.addSink(sfence, "SfenceBundle")
  BoringUtils.addSink(csr, "TLBCSRIO")

  if (isDtlb) {
    BoringUtils.addSource(vmEnable, "DTLBENABLE")
  } else {
    BoringUtils.addSource(vmEnable, "ITLBENABLE")
  }

  val reqAddr = req.map(_.bits.vaddr.asTypeOf(vaBundle))
  val cmd     = req.map(_.bits.cmd)
  val valid   = req.map(_.valid)

  def widthMapSeq[T <: Seq[Data]](f: Int => T) = (0 until Width).map(f)
  def widthMap[T <: Data](f: Int => T) = (0 until Width).map(f)

  val v = RegInit(0.U(TlbEntrySize.W))
  val pf = RegInit(0.U(TlbEntrySize.W)) // TODO: when ptw resp a pf(now only page not found), store here
  val entry = Reg(Vec(TlbEntrySize, new TlbEntry))
  val g = VecInit(entry.map(_.perm.g)).asUInt // TODO: need check if reverse is needed

  val entryHitVec = widthMapSeq{i => VecInit(entry.map(_.hit(reqAddr(i).vpn/*, satp.asid*/))) }
  val hitVec  = widthMapSeq{ i => (v.asBools zip entryHitVec(i)).map{ case (a,b) => a&b } }
  val pfHitVec   = widthMapSeq{ i => (pf.asBools zip entryHitVec(i)).map{ case (a,b) => a&b } }
  val pfArray = widthMap{ i => ParallelOR(pfHitVec(i)).asBool && valid(i) && vmEnable }
  val hit     = widthMap{ i => ParallelOR(hitVec(i)).asBool && valid(i) && vmEnable && ~pfArray(i) }
  val miss    = widthMap{ i => !hit(i) && valid(i) && vmEnable && ~pfArray(i) }
  val hitppn  = widthMap{ i => ParallelMux(hitVec(i) zip entry.map(_.ppn)) }
  val hitPerm = widthMap{ i => ParallelMux(hitVec(i) zip entry.map(_.perm)) }
  val multiHit = {
    val hitSum = widthMap{ i => PopCount(hitVec(i)) }
    val pfHitSum = widthMap{ i => PopCount(pfHitVec(i)) }
    ParallelOR(widthMap{ i => !(hitSum(i)===0.U || hitSum(i)===1.U) || !(pfHitSum(i)===0.U || pfHitSum(i)===1.U)})
  }

  // resp  // TODO: A/D has not being concerned
  for(i <- 0 until Width) {
    resp(i).valid := valid(i)
    resp(i).bits.paddr := Mux(vmEnable, Cat(hitppn(i), reqAddr(i).off), SignExt(req(i).bits.vaddr, PAddrBits))
    resp(i).bits.miss := miss(i)

    val perm = hitPerm(i) // NOTE: given the excp, the out module choose one to use?
    val update = false.B && hit(i) && (!hitPerm(i).a || !hitPerm(i).d && TlbCmd.isWrite(cmd(i))) // update A/D through exception
    val modeCheck = !(mode === ModeU && !perm.u || mode === ModeS && perm.u && (!priv.sum || ifecth))
    val ldPf = (pfArray(i) && TlbCmd.isRead(cmd(i)) && true.B /*!isAMO*/) || hit(i) && !(modeCheck && (perm.r || priv.mxr && perm.x)) && (TlbCmd.isRead(cmd(i)) && true.B/*!isAMO*/) // TODO: handle isAMO
    val stPf = (pfArray(i) && TlbCmd.isWrite(cmd(i)) || false.B /*isAMO*/ ) || hit(i) && !(modeCheck && perm.w) && (TlbCmd.isWrite(cmd(i)) || false.B/*TODO isAMO. */)
    val instrPf = (pfArray(i) && TlbCmd.isExec(cmd(i))) || hit(i) && !(modeCheck && perm.x) && TlbCmd.isExec(cmd(i))
    resp(i).bits.excp.pf.ld    := ldPf || update
    resp(i).bits.excp.pf.st    := stPf || update
    resp(i).bits.excp.pf.instr := instrPf || update
  }

  // ptw
  val state_idle :: state_wait :: Nil = Enum(2)
  val state = RegInit(state_idle)

  ptw <> DontCare // TODO: need check it
  ptw.req.valid := ParallelOR(miss).asBool && state===state_idle
  ptw.resp.ready := state===state_wait

  // val ptwReqSeq = Wire(Seq.fill(Width)(new comBundle()))
  val ptwReqSeq = Seq.fill(Width)(Wire(new comBundle()))
  for (i <- 0 until Width) {
    ptwReqSeq(i).valid := valid(i) && miss(i)
    ptwReqSeq(i).roqIdx := req(i).bits.roqIdx
    ptwReqSeq(i).bits.vpn := reqAddr(i).vpn
  }
  ptw.req.bits := Compare(ptwReqSeq).bits

  switch (state) {
    is (state_idle) {
      when (ParallelOR(miss).asBool && ptw.req.fire()) {
        state := state_wait
      }
      assert(!ptw.resp.valid)
    }

    is (state_wait) {
      when (ptw.resp.fire()) {
        state := state_idle
      }
    }
  }

  // reset pf when pf hit
  val pfHitReset = ParallelOR(widthMap{i => Mux(valid(i), VecInit(pfHitVec(i)).asUInt, 0.U) })
  val pfHitRefill = ParallelOR(pfHitReset.asBools)

  // refill
  val refill = ptw.resp.fire()
  val randIdx = LFSR64()(log2Up(TlbEntrySize)-1,0)
  val priorIdx = PriorityEncoder(~(v|pf))
  val tlbfull = ParallelAND((v|pf).asBools)
  val refillIdx = Mux(tlbfull, randIdx, priorIdx)
  val re2OH = UIntToOH(refillIdx)
  when (refill) {
    v := Mux(ptw.resp.bits.pf, v & ~re2OH, v | re2OH)
    entry(refillIdx) := ptw.resp.bits.entry
    Debug(p"Refill: idx:${refillIdx} entry:${ptw.resp.bits.entry}\n")
  }

  // pf update
  when (refill) {
    when (pfHitRefill) {
      pf := Mux(ptw.resp.bits.pf, pf | re2OH, pf & ~re2OH) & ~pfHitReset
    } .otherwise {
      pf := Mux(ptw.resp.bits.pf, pf | re2OH, pf & ~re2OH)
    }
  } .otherwise {
    when (pfHitRefill) {
      pf := pf & ~pfHitReset
    }
  }
  when (PopCount(pf) > 10.U) { // when too much pf, just clear
    pf := Mux(refill && ptw.resp.bits.pf, re2OH, 0.U)
  }

  // sfence (flush)
  when (sfence.valid) {
    when (sfence.bits.rs1) { // virtual address *.rs1 <- (rs1===0.U)
      when (sfence.bits.rs2) { // asid, but i do not want to support asid, *.rs2 <- (rs2===0.U)
        // all addr and all asid
        v := 0.U
        pf := 0.U
      }.otherwise {
        // all addr but specific asid
        v := v & g // TODO: need check if reverse is needed
        pf := pf & g
      }
    }.otherwise {
      when (sfence.bits.rs2) {
        // specific addr but all asid
        v := v & ~VecInit(entry.map(_.hit(sfence.bits.addr.asTypeOf(vaBundle).vpn))).asUInt
        pf := pf & ~VecInit(entry.map(_.hit(sfence.bits.addr.asTypeOf(vaBundle).vpn))).asUInt
      }.otherwise {
        // specific addr and specific asid
        v := v & ~VecInit(entry.map(e => e.hit(sfence.bits.addr.asTypeOf(vaBundle).vpn) && (/*e.asid === sfence.bits.asid && */!e.perm.g))).asUInt
        pf := pf & ~VecInit(entry.map(e => e.hit(sfence.bits.addr.asTypeOf(vaBundle).vpn) && (/*e.asid === sfence.bits.asid && */!e.perm.g))).asUInt
      }
    }
  }

  // Log
  for(i <- 0 until Width) {
    Debug(req(i).valid, p"req(${i.U}): ${req(i).bits}\n")
    Debug(resp(i).valid, p"resp(${i.U}): ${resp(i).bits}\n")
  }

  Debug(sfence.valid, p"Sfence: ${sfence}\n")
  Debug(ParallelOR(valid)|| ptw.resp.valid, p"CSR: ${csr}\n")
  Debug(ParallelOR(valid) || ptw.resp.valid, p"vmEnable:${vmEnable} hit:${Binary(VecInit(hit).asUInt)} miss:${Binary(VecInit(miss).asUInt)} v:${Hexadecimal(v)} pf:${Hexadecimal(pf)} state:${state}\n")
  Debug(ptw.req.fire(), p"PTW req:${ptw.req.bits}\n")
  Debug(ptw.resp.valid, p"PTW resp:${ptw.resp.bits} (v:${ptw.resp.valid}r:${ptw.resp.ready}) \n")

  // assert check, can be remove when tlb can work
  for(i <- 0 until Width) {
    assert((hit(i)&pfArray(i))===false.B, "hit(%d):%d pfArray(%d):%d v:0x%x pf:0x%x", i.U, hit(i), i.U, pfArray(i), v, pf)
  }
  for(i <- 0 until Width) {
    Debug(multiHit, p"vpn:0x${Hexadecimal(reqAddr(i).vpn)} hitVec:0x${Hexadecimal(VecInit(hitVec(i)).asUInt)} pfHitVec:0x${Hexadecimal(VecInit(pfHitVec(i)).asUInt)}\n")
  }
  for(i <- 0 until TlbEntrySize) {
    Debug(multiHit, p"entry(${i.U}): v:${v(i)} ${entry(i)}\n")
  }
  assert(!multiHit) // add multiHit here, later it should be removed (maybe), turn to miss and flush

  // for (i <- 0 until Width) {
  //   Debug(resp(i).valid && hit(i) && !(req(i).bits.vaddr===resp(i).bits.paddr), p"vaddr:0x${Hexadecimal(req(i).bits.vaddr)} paddr:0x${Hexadecimal(resp(i).bits.paddr)} hitVec:0x${Hexadecimal(VecInit(hitVec(i)).asUInt)}}\n")
  //   when (resp(i).valid && hit(i) && !(req(i).bits.vaddr===resp(i).bits.paddr)) {
  //     for (j <- 0 until TlbEntrySize) {
  //       Debug(true.B, p"TLBEntry(${j.U}): v:${v(j)} ${entry(j)}\n")
  //     }
  //   } // FIXME: remove me when tlb may be ok
  //   when(resp(i).valid && hit(i)) {
  //     assert(req(i).bits.vaddr===resp(i).bits.paddr, "vaddr:0x%x paddr:0x%x hitVec:%x ", req(i).bits.vaddr, resp(i).bits.paddr, VecInit(hitVec(i)).asUInt)
  //   } // FIXME: remove me when tlb may be ok
  // }
  
  assert((v&pf)===0.U, "v and pf can't be true at same time: v:0x%x pf:0x%x", v, pf)
}

object NBTLB {
  def apply(in: SimpleBusUC, mem: TlbPtwIO, cacheEmpty: Bool, excp: TlbExcpBundle, isDtlb: Boolean, ifecth: Bool, userBits: Int = 0) = {
    val tlb = Module(new NBTLB(1, isDtlb))
    tlb.io.requestor(0).req.valid := in.req.valid
    tlb.io.requestor(0).req.bits.vaddr := in.req.bits.addr
    tlb.io.requestor(0).req.bits.cmd   := Mux(ifecth, TlbCmd.exec, Mux(in.req.bits.isRead, TlbCmd.read, TlbCmd.write)) // TODO: need check AMO
    tlb.io.requestor(0).req.bits.roqIdx := DontCare
    tlb.io.requestor(0).req.bits.debug := DontCare
    

    val pf = LookupTree(tlb.io.requestor(0).req.bits.cmd, List(
      TlbCmd.read -> tlb.io.requestor(0).resp.bits.excp.pf.ld,
      TlbCmd.write -> tlb.io.requestor(0).resp.bits.excp.pf.st,
      TlbCmd.exec -> tlb.io.requestor(0).resp.bits.excp.pf.instr
    ))

    val out = Wire(new SimpleBusUC(userBits = userBits))
    out.req.valid := Mux(pf, false.B, in.req.valid && !tlb.io.requestor(0).resp.bits.miss)
    out.req.bits := in.req.bits
    out.req.bits.addr := tlb.io.requestor(0).resp.bits.paddr
    in.req.ready := !tlb.io.requestor(0).resp.bits.miss && out.req.ready

    val isAMO = WireInit(false.B)
    if (isDtlb) {
      BoringUtils.addSink(isAMO, "ISAMO")
    }

    excp.pf.ld := tlb.io.requestor(0).resp.bits.excp.pf.ld && !isAMO
    excp.pf.st := tlb.io.requestor(0).resp.bits.excp.pf.st && (tlb.io.requestor(0).resp.bits.excp.pf.ld && isAMO)
    excp.pf.instr := false.B//tlb.io.requestor(0).resp.bits.excp.pf.instr
    excp.pf.addr := in.req.bits.addr

    assert(!(pf && out.req.valid))
    
    in.resp <> out.resp
    if(!isDtlb) {
      when (pf && cacheEmpty) {
        in.resp.valid := in.req.valid
        in.resp.bits.rdata := 0.U
        in.resp.bits.cmd := SimpleBusCmd.readLast
        in.resp.bits.user.map(_ := in.req.bits.user.getOrElse(0.U))
        excp.pf.instr := true.B
      }
      when (pf) {
        in.req.ready := in.resp.ready && cacheEmpty
      }
    }
    Debug(in.req.valid && pf, p"PF: inReq(${in.req.valid} ${in.req.ready}) Resp(${in.resp.valid} ${in.resp.valid}) OutReq(${out.req.valid} ${out.req.ready}) Resp(${out.resp.valid} ${out.resp.ready}) paddr:0x${Hexadecimal(in.req.bits.addr)} vaddr:0x${Hexadecimal(out.req.bits.addr)} cacheEmpty:${cacheEmpty} cmd:${tlb.io.requestor(0).req.bits.cmd} tlbResp:${tlb.io.requestor(0).resp.bits}\n")(name = tlb.name)

    mem <> tlb.io.ptw

    out
  }
}

object OOTLB {
  def apply(in: SimpleBusUC, mem: TlbPtwIO, excp: TlbExcpBundle, isDtlb: Boolean, ifecth: Bool, userBits: Int = 0) = {
    val tlb = Module(new NBTLB(1, isDtlb))
    tlb.io.requestor(0).req.valid := in.req.valid
    tlb.io.requestor(0).req.bits.vaddr := in.req.bits.addr
    tlb.io.requestor(0).req.bits.cmd   := Mux(ifecth, TlbCmd.exec, Mux(in.req.bits.isRead, TlbCmd.read, TlbCmd.write)) // TODO: need check AMO
    tlb.io.requestor(0).req.bits.roqIdx := DontCare
    tlb.io.requestor(0).req.bits.debug := DontCare
    in.req.ready := !tlb.io.requestor(0).resp.bits.miss

    val pf = LookupTree(tlb.io.requestor(0).req.bits.cmd, List(
      TlbCmd.read -> tlb.io.requestor(0).resp.bits.excp.pf.ld,
      TlbCmd.write -> tlb.io.requestor(0).resp.bits.excp.pf.st,
      TlbCmd.exec -> tlb.io.requestor(0).resp.bits.excp.pf.instr
    ))
    in.req.ready := !tlb.io.requestor(0).resp.bits.miss && in.resp.ready
    in.resp := DontCare // FIXME
    in.resp.valid := tlb.io.requestor(0).resp.valid && !tlb.io.requestor(0).resp.bits.miss
    in.resp.bits.rdata := tlb.io.requestor(0).resp.bits.paddr

    val isAMO = WireInit(false.B)
    if (isDtlb) {
      BoringUtils.addSink(isAMO, "ISAMO")
    }

    // excp := tlb.io.requestor(0).resp.bits.excp
    excp.pf.ld := tlb.io.requestor(0).resp.bits.excp.pf.ld && !isAMO
    excp.pf.st := tlb.io.requestor(0).resp.bits.excp.pf.st && (tlb.io.requestor(0).resp.bits.excp.pf.ld && isAMO)
    excp.pf.instr := tlb.io.requestor(0).resp.bits.excp.pf.instr
    excp.pf.addr := in.req.bits.addr

    mem <> tlb.io.ptw

    tlb
  }
}