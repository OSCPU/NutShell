package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import utils._
import bus.simplebus._

trait HasVectorParameter extends HasNutCoreParameter{
    val NUMVREG = 32
    val bVLEN  = log2Ceil(VLEN)
    val BLEN = 8
    val NLane = 4
    // val SLEN   = 128
    val MLEN   = VLEN / 8
    val bMLEN  = log2Ceil(MLEN)
    val VLMAX = 256
    val bVLMAX = log2Ceil(VLMAX)+1
    val VLMULMAX = 8
    val bVLMULMAX = 3
    val HasNonBLockingCache = false
}


class VMEMIO(val userBits: Int = 0) extends NutCoreBundle {
    val mem = new SimpleBusUC(userBits = userBits, addrBits = VAddrBits)
    val dtlbPF = Output(Bool())
    val loadAddrMisaligned = Output(Bool())
    val storeAddrMisaligned = Output(Bool())
}


class VMUIO extends Bundle with HasVectorParameter with HasNutCoreConst {
    val in = Flipped(Decoupled(new Bundle {
        val rs1 = Output(UInt(XLEN.W))
        val rs2 = Output(UInt(XLEN.W))
        val func = Output(UInt(6.W))
        val vs2 = Output(UInt(5.W))
        val vd  = Output(UInt(5.W))
    }))
    val out = Decoupled(Bool())
    val cfg = Flipped(new VCFGIO)
    val vm = Input(Bool())
    val dmem = new VMEMIO(userBits = DVMemUserBits)
    val vreg = new VRegRWBus
}

class Position extends Bundle with HasVectorParameter {
    val vd = Output(UInt(5.W))
    val mask = Output(UInt((XLEN/8).W))
    val idx2 = Output(UInt(2.W))
    val idx3 = Output(UInt(3.W))
    // 5+8+2+3 => 18
}

class VLSUQueueEntry extends Bundle with HasVectorParameter {
    val position = new Bundle {
        val vd   = UInt(5.W)
        val mask = UInt((XLEN/8).W)
        val idx2 = UInt(2.W)
        val idx3 = UInt(3.W)
    }
    val addr = UInt(3.W)
    val func = UInt(6.W) // 9 bit
}

class VMUExecIO(val userBits: Int = 0) extends Bundle with HasVectorParameter {
    val in = Flipped(Decoupled(new Bundle {
        val addr = Output(UInt(XLEN.W))
        val wdata = Output(UInt(XLEN.W))
        val func = Output(UInt(6.W))
        val position = new Position
        val v0Wmask = Output(UInt((XLEN/8).W))
    }))
    val out = Decoupled(new Bundle {
        val rdata = Output(UInt(XLEN.W))
        val position = new Position
    })
    val dmem = new VMEMIO(userBits = userBits)
}

class VMU extends NutCoreModule with HasVectorParameter {
    val io = IO(new VMUIO)

    val (valid, rs1, rs2, func, vs2, vd) = (io.in.valid, io.in.bits.rs1, io.in.bits.rs2, io.in.bits.func, io.in.bits.vs2, io.in.bits.vd)
    def access(valid: UInt, rs1: UInt, rs2: UInt, func: UInt, vs2: UInt, vd: UInt) {
        this.valid := valid
        this.rs1 := rs1
        this.rs2 := rs2
        this.func := func
        this.vs2 := vs2
        this.vd := vd
    }

    // load can widen, can't narrow
    // store can't widen, can narrow, this can be check out of VMU ?
    // val check_size = WireInit(false.B)
    // check_size = Mux(isLoad(func), !slt(size, vsew), slt(size, vsew))

    // store the req, and do the job, output an finish
    // should have an cycle to store the req? one almost empty cycle
    // but still need to store the req for further usage.
    val rs1_reg = Reg(UInt(XLEN.W))
    val rs2_reg = Reg(UInt(XLEN.W))
    val vs2_reg = Reg(UInt(5.W))
    val vs3_reg = Reg(UInt(5.W))
    val vd_reg  = Reg(UInt(5.W))
    val vsrc2   = Reg(UInt(VLEN.W))
    val vsrc3   = Reg(UInt(VLEN.W))
    // val vm      = Reg(Bool())
    val vsew    = Reg(UInt(2.W))
    val func_reg = Reg(FuOpType())
    // val vlmul   = Reg(UInt(2.W)) // unused
    val vs2_next = vs2_reg + 1.U
    val vs3_next = vs3_reg + 1.U
    val vd_next  = vd_reg + 1.U

    // count for req

    val vlen = Reg(UInt(bVLMAX.W))
    val vlen_consume = Wire(UInt(4.W)) //maxium is 8, so len is 4(even 3?)
    val vlen_consume_log2 = Wire(UInt(2.W))
    val idx = Reg(UInt(bMLEN.W)) // idx: index in one vreg
    // val vlen_next = vlen - 1.U
    val vlen_next = vlen - vlen_consume
    // val idx_next = Mux((idx + 1.U) === (VLEN.U((bVLEN+1).W) >> vsew), 0.U, idx + 1.U)
    val idx_next_temp = idx + vlen_consume
    val idx_next = Mux(idx_next_temp === (VLEN.U((bVLEN+1).W) >> vsew), 0.U, idx_next_temp)
    val idx2 = (idx << vsew)(4,3) // idx2: index that in which 64-bit block
    val idx3 = (idx << vsew)(2,0) // idx3: index that in which 8-bit block in a 64-bit block
    val idx2_next = (idx_next << vsew)(4,3)
    val idx3_next = (idx_next << vsew)(2,0)
    val count_req = Reg(UInt(9.W)) // count for req time
    val count_req_next = count_req + 1.U

    // count for resp
    val count_resp = Reg(UInt(9.W))
    val count_resp_next = count_resp + 1.U

    // v0 - iterate
    val tmp0 = WireInit(8.U(7.W))
    val v0Compact = Reg(UInt(VLEN.W)) // store compact bit
    val v0CompactInuse = v0Compact(7,0)
    val v0Compact_next = v0Compact >> vlen_consume
    val v0Mask = Wire(UInt(7.W))
    val vmEnable = (v0CompactInuse & v0Mask) =/= 0.U

    val vmuExec = Module(new VMUExecUnit)
    val reqFire = vmuExec.io.in.fire()
    val reqValid = vmuExec.io.in.valid
    val reqReady = vmuExec.io.in.ready
    val respFire = vmuExec.io.out.fire()
    val addr_reg = Reg(UInt(XLEN.W))
    val size = Mux(func_reg(1,0)===3.U, vsew(1,0), func_reg(1,0))
    val stride = Mux(VMUOpType.isUnit(func_reg), 1.U << size, rs2)

    val ilast = (vlen_next === 0.U)
    val valid_reg = RegInit(false.B)
    val vlenNonZero = io.in.fire() && (io.cfg.vlen=/=0.U)
    when (ilast && ((reqReady || !vmEnable) && valid_reg)) { valid_reg := false.B}
    when (vlenNonZero) { valid_reg := true.B }

    when (vlenNonZero) {
        vsew := io.cfg.vsew
        func_reg := func
        idx := 0.U
        vlen := io.cfg.vlen

        addr_reg := rs1
        vs2_reg := vs2
        vs3_reg := vd
        vd_reg := vd
        vsrc2 := io.vreg.vsrc2
        vsrc3 := io.vreg.vsrc3
        // v0 := extract(io.vreg.v0, io.cfg.vsew, io.cfg.vlmul)
        v0Compact := Fill(VLEN, io.vm).asUInt | compact(io.vreg.v0, io.cfg.vsew, io.cfg.vlmul)
        count_req := 0.U
        count_resp := 0.U
    }

    when ((reqReady || !vmEnable) && valid_reg) {
        idx := idx_next
        vlen := vlen_next
        // v0 := v0_next
        v0Compact := v0Compact_next
        addr_reg := Mux(VMUOpType.isIdxd(func_reg), addr_reg, addr_reg + (stride<<vlen_consume_log2))
        when (idx2_next === 0.U && idx3_next === 0.U) {
            vs2_reg := vs2_next
            vs3_reg := vs3_next
            vd_reg := vd_next
            vsrc2 := io.vreg.vsrc2
            vsrc3 := io.vreg.vsrc3
        }
        // when (idx2_next === 0.U && idx3_next === 0.U) {
        //   vd_reg := vd_next
        // }
        when (vmEnable) {
            count_req := count_req_next
        }
    }

    when (respFire) {
        count_resp := count_resp_next
    }

    val olast1 = (count_resp_next === count_req) && ~valid_reg
    val olast2 = (count_resp === count_req)  && ~valid_reg
    val ready_reg = RegInit(true.B)
    val finish = (olast1 && respFire || olast2) && ~ready_reg
    when (vlenNonZero) { ready_reg := false.B }
    when (io.out.fire()) { ready_reg := true.B }

    def extract(data: UInt, vsew: UInt, vlmul: UInt) = {
        // extract mask and keep their position
        val lenRes = Wire(Vec(7, UInt(VLEN.W)))
        (0 until 7).map(i => {
            val len = 1 << i // 1,2,4,8,16,32,64
            val num = VLEN/len // 256,128,64,32,16,8,4
            lenRes(i) := VecInit((0 until num).map(j => ZeroExt(data(j*len), len))).asUInt
        }) // (lenRes1, lenRes2, lenRes4, lenRes8,,,,)
        val lenMap = (0 until 16).map(i => {
            val i1 = i & 3
            val i2 = i / 4
            val idx = 3 + i1 - i2
            i.U -> lenRes(idx)
        })
        LookupTree(Cat(vsew, vlmul), lenMap) // TODO: check order
    }

    def compact(data: UInt, vsew: UInt, vlmul: UInt) = {
        // extract mask and compact to low bit
        val lenRes = Wire(Vec(7,UInt(VLEN.W)))
        (0 until 7).map(i => {
            val len = 1 << i
            val num = VLEN/len
            val pure = VecInit((0 until num).map(j => data(j*len))).asUInt
            lenRes(i) := ZeroExt(pure, VLEN)
        }) // 1:2:4:8:16:32:64
        // 0:1:2:3: 4: 5: 6
        val lenMap = (0 until 16).map(i => {
            val i1 = i & 3
            val i2 = i >> 2
            val idx = 3 - i1 + i2
            i.U -> lenRes(idx)
        })
        LookupTree(Cat(vsew, vlmul), lenMap)
    }

    def toLsuFunc(func: UInt, size: UInt, vlen_consume: UInt) : UInt = {
        // translate VMU func to vmuExec func
        // size:  0/1/2/3
        // width: 8/16/32/64
        // vlen_consume: 1:1x / 2:2x / 4:4x / 8:8x
        // log2(vlen_consume): 1:0 / 2:1 / 4:2 / 8:3
        // size + log2(vlen_consume) => final size
        val final_size = size + Log2(vlen_consume)
        Cat(0.U(2.W), VMUOpType.isStore(func), ~VMUOpType.isSigned(func) & (final_size=/=3.U), final_size)
    }

    def Part256_64(data: UInt, idx2: UInt, idx3: UInt) : UInt = { // data from 256 to 64
        val part1 = LookupTree(idx2(1,0), List(
            "b00".U   -> data(XLEN*2-1, 0),
            "b01".U   -> data(XLEN*3-1, XLEN*1),
            "b10".U   -> data(XLEN*4-1, XLEN*2),
            "b11".U   -> ZeroExt(data(XLEN*4-1, XLEN*3), XLEN*2)
        ))
        val part2 =LookupTree(idx3(2,0), List(
            "b000".U -> part1(XLEN-1, 0),
            "b001".U -> part1(XLEN-1+8, 8),
            "b010".U -> part1(XLEN-1+16, 16),
            "b011".U -> part1(XLEN-1+24, 24),
            "b100".U -> part1(XLEN-1+32, 32),
            "b101".U -> part1(XLEN-1+40, 40),
            "b110".U -> part1(XLEN-1+48, 48),
            "b111".U -> part1(XLEN-1+56, 56)
        ))
        part2
    }

    def All64_256(data: UInt, idx2: UInt, idx3: UInt) : UInt = {
        val part64 = LookupTree(idx3(2,0), List(
            "b000".U  -> data,
            "b001".U  -> Cat(data(55, 0), 0.U(8.W)),
            "b010".U  -> Cat(data(47, 0), 0.U(16.W)),
            "b011".U  -> Cat(data(39, 0), 0.U(24.W)),
            "b100".U  -> Cat(data(31, 0), 0.U(32.W)),
            "b101".U  -> Cat(data(23, 0), 0.U(40.W)),
            "b110".U  -> Cat(data(15, 0), 0.U(48.W)),
            "b111".U  -> Cat(data( 8, 0), 0.U(56.W))
        ))
        LookupTree(idx2(1,0), List(
            "b00".U   -> Cat(0.U(192.W), part64),
            "b01".U   -> Cat(0.U(128.W), part64, 0.U(64.W)),
            "b10".U   -> Cat(0.U(64.W),  part64, 0.U(128.W)),
            "b11".U   -> Cat(            part64, 0.U(192.W))
        ))
    }

    def gen_wmask (vlen_consume: UInt, v0Compact: UInt) : UInt = {
        // output vmuExec.wmask
        LookupTree(vlen_consume << vsew, List( // TODO: check length
            1.U  -> ("b1".U & v0Compact(0)),
            2.U  -> ("b11".U & Mux(vsew===0.U, v0Compact(1,0), Fill(2, v0Compact(0)))),
            4.U  -> ("b1111".U & LookupTree(vsew(1,0), List(
                "b00".U -> v0Compact(3,0),
                "b01".U -> Cat(Fill(2, v0Compact(1)), Fill(2, v0Compact(0))),
                "b10".U -> Fill(4, v0Compact(0))//,
                // "b11".U -> 0.U
            ))),
            8.U  -> ("b11111111".U & LookupTree(vsew(1,0), List( // TODO: if vlen_consume is 8, then vsew must be 0
                "b00".U -> v0Compact(7,0),
                "b01".U -> Cat(Fill(2, v0Compact(3)), Fill(2, v0Compact(2)), Fill(2, v0Compact(1)), Fill(2, v0Compact(0))),
                "b10".U -> Cat(Fill(4, v0Compact(1)), Fill(4, v0Compact(0))),
                "b11".U -> Fill(8, v0Compact(0))
            )))
        ))
    }

    def gen_mask (wmask: UInt, idx: UInt) : UInt = {
        // output vreg.wmask
        val mask = ZeroExt(wmask, MLEN)
        val shift = idx << vsew
        (mask << shift(bMLEN-1, 0))
    }

    def gen_mask_shift (wmask: UInt, shift: UInt) : UInt = {
        val mask = ZeroExt(wmask, MLEN)
        (mask << shift(bMLEN-1, 0))
    }

    def get_vlenConsumeCheck (vlen: UInt, idx2: UInt, idx3: UInt, addr: UInt, vsew: UInt, size: UInt, mode: UInt, stride: UInt) = {
        // if mode is idxed, return 1
        // if mode is strided is 8<<vsew, then go to next, else return 1
        // need concern vlen, idx, addr, vsew and width (vsew is the vreg width, and width is mem width)
        // there is three kind width
        // 1st: vsew -> width in vreg
        // 2nd: size -> width in mem
        // 3rd: stri -> width in addr
        // when (size == stri) and (size == vsew), could optimize

        // if mode is idxed.      => 1
        // if stried =/= 1<<vsew  => 1
        // if vsew =/= width(maybe) => 1(for hard to handle widen and narrow)

        // if addr is odd,        => 1
        // if addr is 2x, not 4x. => 1/2
        // if addr is 4x, not 8x. => 1/2/4
        // if addr is 8x.         => 1/2/4/8

        // if vlen <= 1.          => 1
        // if vlen <= 3.          => 1/2
        // if vlen <= 7.          => 1/2/4
        // if vlen >= 8.          => 1/2/4/8

        // if idx2 <= 2.          => 1/2/4/8
        //
        // if idx2 == 3:
        // if idx3 <= 0.          => 1/2/4/8
        //for simplified(should concern vsew): when idx2 === 3.U && idx3 >= 0.U => 1
        // if idx3 <= 4.          => 1/2/4
        // if idx3 <= 6.          => 1/2
        // if idx3 <= 7.          => 1

        // vsew == 0.             => 1/2/4/8
        // vsew == 1.             => 1/2/4
        // vsew == 2.             => 1/2
        // vsew == 3.             => 1

        // size == 0.            => 1/2/4/8
        // size == 1.            => 1/2/4
        // size == 2.            => 1/2
        // size == 3.            => 1

        val addr_one = /*(size===3.U) || */(size===2.U && addr(2,0)=/=0.U) || (size===1.U && addr(1,0)=/=0.U) || (size===0.U && addr(0)=/=0.U)
        val addr_two = /*(size===2.U) || */(size===1.U && addr(2,0)=/=0.U) || (size===0.U && addr(1,0)=/=0.U)
        val addr_four = /*(size===1.U) || */(size===0.U && addr(2,0)=/=0.U)

        val one = (mode === "b01".U || mode === "b11".U) || (size =/= vsew || (1.U << size) =/= stride) || addr_one || (vlen===1.U) || (idx2===3.U && idx3=/=0.U) || (vsew===3.U) || (size===3.U)
        // when one is false, then: (mode is unit/strided) && (size==vsew && size===stride) && (addr is even) && (vlen > 1.U) && (idx2 <= 2.U) && (vsew<3.U) && (size<3.U)
        val two = addr_two || (vlen===2.U || vlen===3.U) || (vsew===2.U)
        val four = addr_four || (vlen===4.U || vlen===5.U || vlen===6.U || vlen===7.U) || (vsew===1.U)
        // (Mux(one, 1.U, Mux(two, 2.U, Mux(four, 4.U, 8.U))), Mux(one, 0.U, Mux(two, 1.U, Mux(four, 2.U, 3.U))))
        (one, two, four)
    }

    val part_mask = LookupTree(vsew(1,0), List(
        "b00".U -> 0x0ff.U(XLEN.W),
        "b01".U -> 0x0ffff.U(XLEN.W),
        "b10".U -> ZeroExt(Fill(XLEN/2, true.B).asUInt, XLEN),
        "b11".U -> Fill(XLEN, true.B).asUInt
    ))

    val vsrc3_part = Part256_64(vsrc3, idx2, idx3)// & part_mask
    val vsrc2_part = Part256_64(vsrc2, idx2, idx3) & part_mask
    val addr = Mux(VMUOpType.isIdxd(func_reg), addr_reg+vsrc2_part, addr_reg)

    val (vlen_one, vlen_two, vlen_four) = get_vlenConsumeCheck(vlen(2,0), idx2, idx3, addr, vsew, size, VMUOpType.getMode(func_reg), stride)
    vlen_consume := Mux(vlen_one, 1.U, Mux(vlen_two, 2.U, Mux(vlen_four, 4.U, 8.U)))
    vlen_consume_log2 := Mux(vlen_one, 0.U, Mux(vlen_two, 1.U, Mux(vlen_four, 2.U, 3.U)))
    v0Mask := Mux(vlen_one, "b1".U, Mux(vlen_two, "b11".U, Mux(vlen_four, "b1111".U, "b11111111".U)))
    assert(vlen_consume===1.U || vlen_consume===2.U || vlen_consume===4.U || vlen_consume===8.U)

    val vmuWmask = gen_wmask(vlen_consume, v0Compact)
    vmuExec.io.in.bits.wdata := vsrc3_part
    vmuExec.io.in.bits.func := toLsuFunc(func_reg, size, vlen_consume)
    vmuExec.io.in.bits.addr := addr
    vmuExec.io.in.bits.position.vd := vd_reg
    // vmuExec.io.in.bits.position.mask := gen_mask(1.U, idx)
    vmuExec.io.in.bits.position.mask := vmuWmask
    vmuExec.io.in.bits.position.idx2 := idx2
    vmuExec.io.in.bits.position.idx3 := idx3
    vmuExec.io.in.bits.v0Wmask := vmuWmask
    vmuExec.io.in.valid := valid_reg && vmEnable // v0-mask
    vmuExec.io.out.ready := !ready_reg
    assert(~(vmuExec.io.out.valid && ready_reg))
    // TODO: maybe long latency

    // store to vreg, it would be better if there is read_burst
    val wmask = vmuExec.io.out.bits.position.mask
    val rdata = All64_256(vmuExec.io.out.bits.rdata, vmuExec.io.out.bits.position.idx2, vmuExec.io.out.bits.position.idx3)
    io.vreg.wdata := rdata
    io.vreg.wmask := gen_mask_shift(wmask, Cat(vmuExec.io.out.bits.position.idx2, vmuExec.io.out.bits.position.idx3))
    io.vreg.wen.bits := vmuExec.io.out.valid && VMUOpType.isLoad(func_reg)
    io.vreg.wen.valid := 1.U
    io.vreg.vs1 := DontCare
    io.vreg.vs2 := Mux(!valid_reg, vs2, vs2_next)
    io.vreg.vs3 := Mux(!valid_reg,  vd, vs3_next)
    io.vreg.vd  := vmuExec.io.out.bits.position.vd

    io.dmem <> vmuExec.io.dmem

    io.in.ready := ready_reg
    io.out.valid := finish || (!vlenNonZero && io.in.fire()) // assume io.out.ready is always true
    io.out.bits := !ready_reg
}

class VMUExecUnit extends NutCoreModule {
    val io = IO(new VMUExecIO(userBits = DVMemUserBits))
    // VMUExecUnit is just a lsu unit, vmu offer "better" func and addr, wdata
    // almost the same with LSUExecUnit
    val (valid, wdata, func, position) = (io.in.valid, io.in.bits.wdata, io.in.bits.func, io.in.bits.position)

    def genWmask(addr: UInt, sizeEncode: UInt, v0Wmask: UInt): UInt = {
        (LookupTree(sizeEncode, List(
            "b00".U -> 0x1.U, //0001 << addr(2:0)
            "b01".U -> 0x3.U, //0011
            "b10".U -> 0xf.U, //1111
            "b11".U -> 0xff.U //11111111
        )) & v0Wmask)<< addr(2, 0)
    }

    def genWdata(data: UInt, sizeEncode: UInt): UInt = {
        LookupTree(sizeEncode, List(
            "b00".U -> Fill(8, data(7, 0)),
            "b01".U -> Fill(4, data(15, 0)),
            "b10".U -> Fill(2, data(31, 0)),
            "b11".U -> data
        ))
    }

    val dmem = io.dmem.mem
    val addr = io.in.bits.addr
    // val addrLatch = RegEnable(addr, io.in.fire())
    // val positionLatch = RegEnable(position, io.in.fire())
    val dtlbFinish = WireInit(false.B)
    val dtlbPF = WireInit(false.B)
    val dtlbEnable = WireInit(false.B)
    // BoringUtils.addSink(dtlbFinish, "DTLBFINISH")
    // BoringUtils.addSink(dtlbPF, "DTLBPF")
    // BoringUtils.addSink(dtlbEnable, "DTLBENABLE")

    // val queue = Module(new Queue(new VLSUQueueEntry, 4))
    // io.in.ready := queue.io.enq.ready
    // val couldReq = io.in.valid && queue.io.enq.ready && !io.dmem.loadAddrMisaligned && !io.dmem.storeAddrMisaligned
    // dmem.req.valid := couldReq
    dmem.req.valid := io.in.valid && !io.dmem.loadAddrMisaligned && !io.dmem.storeAddrMisaligned
    dmem.resp.ready := io.out.ready
    io.out.valid := dmem.resp.valid
    io.in.ready := dmem.req.ready
    // queue.io.enq.valid := couldReq && dmem.req.ready
    // queue.io.enq.bits.position := io.in.bits.position
    // queue.io.enq.bits.addr := io.in.bits.addr(2,0)
    // queue.io.enq.bits.func := io.in.bits.func

    // queue.io.deq.ready := dmem.resp.fire()
    // dmem.resp.ready := io.out.ready
    // io.out.valid := (dtlbPF || io.dmem.loadAddrMisaligned || io.dmem.storeAddrMisaligned) || (dmem.resp.valid)

    // val addrLatch = queue.io.deq.bits.addr
    // val positionLatch = queue.io.deq.bits.position
    // val funcLatch = queue.io.deq.bits.func

    val addrLatch = dmem.resp.bits.user.get(3+6-1, 6)
    val funcLatch = dmem.resp.bits.user.get(6-1,0)
    val positionLatch = dmem.resp.bits.user.get(PositionLen+3+6-1, 3+6)

    // val s_idle :: s_wait_tlb :: s_wait_resp :: s_partialLoad :: Nil = Enum(4)
    // val state = RegInit(s_idle)

    // val func_reg = RegEnable(func, io.in.fire())
    // val func_use = Mux(state===s_idle, func, func_reg)
    // val isStore = (valid || state=/= s_idle) && LSUOpType.isStore(func_use) // need check
    val isStore = valid && LSUOpType.isStore(func) // need check
    // val partialLoad = !isStore && (func_use =/= LSUOpType.ld) // need check

    io.dmem.dtlbPF := dtlbPF
    // switch (state) {
    //   is (s_idle) {
    //     when (dmem.req.fire() && dtlbEnable)  { state := s_wait_tlb  }
    //     when (dmem.req.fire() && !dtlbEnable) { state := s_wait_resp }
    //     //when (dmem.req.fire()) { state := Mux(isStore, s_partialLoad, s_wait_resp) }
    //   }
    //   is (s_wait_tlb) {
    //     when (dtlbFinish && dtlbPF ) { state := s_idle }
    //     when (dtlbFinish && !dtlbPF) { state := s_wait_resp/*Mux(isStore, s_partialLoad, s_wait_resp) */}
    //   }
    //   is (s_wait_resp) { when (dmem.resp.fire()) { state := Mux(partialLoad, s_partialLoad, s_idle) } }
    //   is (s_partialLoad) { state := s_idle }
    // }

    val size = func(1,0)
    val v0Wmask = io.in.bits.v0Wmask
    dmem.req.bits.apply(
        addr = addr(VAddrBits-1, 0),
        size = size,
        wdata = genWdata(io.in.bits.wdata, size),
        wmask = genWmask(addr, size, v0Wmask),
        cmd = Mux(isStore, SimpleBusCmd.write, SimpleBusCmd.read),
        user = Cat(io.in.bits.position.asUInt, addr(2,0), func)
    )
    // dmem.req.valid := valid && (state === s_idle) && !io.dmem.loadAddrMisaligned && !io.dmem.storeAddrMisaligned
    // dmem.resp.ready := true.B

    // io.out.valid := Mux( dtlbPF || io.dmem.loadAddrMisaligned || io.dmem.storeAddrMisaligned, true.B, Mux(partialLoad, state === s_partialLoad, dmem.resp.fire() && (state === s_wait_resp)))
    // io.in.ready := (state === s_idle && dmem.req.ready) || dtlbPF

    val rdata = dmem.resp.bits.rdata
    // val rdataLatch = RegNext(rdata)
    val rdataSel = LookupTree(addrLatch(2, 0), List(
        "b000".U -> rdata(63, 0),
        "b001".U -> rdata(63, 8),
        "b010".U -> rdata(63, 16),
        "b011".U -> rdata(63, 24),
        "b100".U -> rdata(63, 32),
        "b101".U -> rdata(63, 40),
        "b110".U -> rdata(63, 48),
        "b111".U -> rdata(63, 56)
    ))
    val rdataPartialLoad = LookupTree(funcLatch, List(
        LSUOpType.lb   -> SignExt(rdataSel(7, 0) , XLEN),
        LSUOpType.lh   -> SignExt(rdataSel(15, 0), XLEN),
        LSUOpType.lw   -> SignExt(rdataSel(31, 0), XLEN),
        LSUOpType.ld   -> SignExt(rdataSel(63, 0), XLEN),
        LSUOpType.lbu  -> ZeroExt(rdataSel(7, 0) , XLEN),
        LSUOpType.lhu  -> ZeroExt(rdataSel(15, 0), XLEN),
        LSUOpType.lwu  -> ZeroExt(rdataSel(31, 0), XLEN)
    ))

    val addrAligned = LookupTree(func(1,0), List(
        "b00".U   -> true.B,            //b
        "b01".U   -> (addr(0) === 0.U),   //h
        "b10".U   -> (addr(1,0) === 0.U), //w
        "b11".U   -> (addr(2,0) === 0.U)  //d
    ))

    // io.out.bits.rdata := Mux(partialLoad, rdataPartialLoad, rdata)
    // io.out.bits.position := Mux(partialLoad, RegNext(dmem.resp.bits.user.get.asTypeOf(new Position)), dmem.resp.bits.user.get.asTypeOf(new Position))//positionLatch
    // io.isMMIO := DontCare
    io.out.bits.rdata := rdataPartialLoad
    io.out.bits.position := positionLatch.asTypeOf(new Position)

    // val isAMO = WireInit(false.B)
    // BoringUtils.addSink(isAMO, "ISAMO2")
    // BoringUtils.addSource(addr, "LSUADDR")

    // io.dmem.loadAddrMisaligned :=  valid && !isStore /*&& !isAMO*/ && !addrAligned && state===s_idle
    // io.dmem.storeAddrMisaligned := valid && (isStore/* || isAMO*/) && !addrAligned && state===s_idle

    io.dmem.loadAddrMisaligned :=  valid && !isStore /*&& !isAMO*/ && !addrAligned
    io.dmem.storeAddrMisaligned := valid && (isStore/* || isAMO*/) && !addrAligned

    // Debug() {
    when(io.dmem.loadAddrMisaligned || io.dmem.storeAddrMisaligned) {
        printf("[VLSU] valid:%d loadAddrMisalgnied:%d StoreAddrMisalgined:%d addr:%x func:%b Time:%d\n", valid, io.dmem.loadAddrMisaligned, io.dmem.storeAddrMisaligned, addr, func, GTimer())
    }
    // }
}
