package nutcore

import Chisel.{Cat, MuxLookup}
import chisel3._
import chisel3.util._

trait VLaneParameter extends HasNutCoreParameter {
    val NLane = VLEN / XLEN
}

class VEXUIO extends NutCoreBundle {
    val in = Flipped(Decoupled(new Bundle{
        val rsrc1 = Output(UInt(XLEN.W))
        val rsrc2 = Output(UInt(XLEN.W))
        val func = Output(FuOpType())
        val vs1 = Output(UInt(5.W))
        val vs2 = Output(UInt(5.W))
        val vd  = Output(UInt(5.W))
        // val vv  = Output(UInt(3.W))
    }))
    val out = Decoupled(new Bundle {
        val busy = Output(Bool())
        val scala = Output(UInt(XLEN.W))
    })
    val cfg = Flipped(new VCFGIO)
    val vm = Input(Bool())
    val vreg = new VRegRWBus
    // VRegRWBus is designed for VLEN vreg, here we only utilize XLEN
}

class VEXU(val Lane_number: Int) extends NutCoreModule with HasVectorParameter {
    val io = IO(new VEXUIO)
    def access(valid: UInt, rsrc1: UInt, rsrc2: UInt, func: UInt, vs1: UInt, vs2: UInt, vd: UInt) {
        io.in.valid := valid
        io.in.bits.rsrc1 := rsrc1
        io.in.bits.rsrc2 := rsrc2
        io.in.bits.func := func
        io.in.bits.vs1 := vs1
        io.in.bits.vs2 := vs2
        io.in.bits.vd := vd
    }
    // Lane number will affect mask generation
    val state = RegInit(0.U)
    val sNone :: sWorking :: Nil = Enum(2)

    io.vreg.vs3 := DontCare
    // we use vs1 and vs2 port to read necessary registers, vs3 is reserved
    val current_vs1 = Reg(UInt(5.W))
    val current_vs2 = Reg(UInt(5.W))
    val current_vd = Reg(UInt(5.W))
    val in_fire = Wire(Bool())
    in_fire := io.in.valid & io.in.ready
    when (in_fire && state === sNone) {
        // this particular cycle need data just incoming
        io.vreg.vs1 := io.in.bits.vs1
        io.vreg.vs2 := io.in.bits.vs2
        io.vreg.vd := io.in.bits.vd
    } .otherwise {
        io.vreg.vs1 := current_vs1
        io.vreg.vs2 := current_vs2
        io.vreg.vd :=  current_vd
    }


    val vm = Wire(Bool())
    vm := io.vm

    // val vsrc1 = Wire(UInt(XLEN.W))
    def gen_scalar_src(src: UInt) = {
        val ret = Wire(UInt(XLEN.W))
        val src_8 = Wire(UInt(8.W))
        val src_16 = Wire(UInt(16.W))
        val src_32 = Wire(UInt(32.W))
        val src_64 = Wire(UInt(64.W))
        src_8 := src
        src_16 := src
        src_32 := src
        src_64 := src
        val src_8_comp = Cat(src_8, src_8, src_8, src_8, src_8, src_8, src_8, src_8)
        val src_16_comp = Cat(src_16, src_16, src_16, src_16)
        val src_32_comp = Cat(src_32, src_32)
        val src_64_comp = src_64
        ret := MuxLookup(io.cfg.vsew, DontCare, List(
            VMUOpType.byte -> src_8_comp,
            VMUOpType.half -> src_16_comp,
            VMUOpType.word -> src_32_comp,
            VMUOpType.elem -> src_64_comp
        ))
        ret
    }


    when (state === 0.U) {
        io.in.ready :=  1.U
    }

    val no_next = Wire(Bool())

    val alu = Module(new VALU)
    val alu_src1 = Wire(UInt(XLEN.W))
    val alu_src2 = Wire(UInt(XLEN.W))
    val alu_func = Wire(UInt(VALUOpType.OpLen.W))
    val func_reverse = Wire(FuOpType())

    when (VXUOpType.isReverse(io.in.bits.func) && VXUOpType.isSrc1Scala(io.in.bits.func)) {
        alu_src1 := gen_scalar_src(io.in.bits.rsrc1)
    } .elsewhen (VXUOpType.isReverse(io.in.bits.func)) {
        alu_src1 := gen_scalar_src(io.in.bits.rsrc2) // io.vreg.vs1((Lane_number + 1) * XLEN - 1,  (Lane_number) * XLEN)
    } .otherwise {
        // vs2 is always presented
        alu_src1 := io.vreg.vsrc2 // io.vreg.vs2((Lane_number + 1) * XLEN - 1,  (Lane_number) * XLEN)
    }

    when (VXUOpType.isReverse(io.in.bits.func)) {
        alu_src2 := io.vreg.vsrc2
    } .elsewhen (VXUOpType.isSrc1Vector(io.in.bits.func)) {
        alu_src2 := io.vreg.vsrc1 // io.vreg.vs1((Lane_number + 1) * XLEN - 1,  (Lane_number) * XLEN)
    } .elsewhen (VXUOpType.isSrc1Imm(io.in.bits.func)) {
        alu_src2 := gen_scalar_src(io.in.bits.rsrc2) // for VI
    } .otherwise {
        // rsrc2 has been processed by ISU: either from scalar reg or from imm
        alu_src2 := gen_scalar_src(io.in.bits.rsrc1) // for VX
    }


    when (VXUOpType.isReverse(io.in.bits.func)) {
        func_reverse := VXUOpType.subvv // any subv(i/x/v) is valid here, just notify ALU we are doing a sub
    } .otherwise {
        func_reverse := io.in.bits.func
    }
    alu_func := Cat(0.U, func_reverse(8, 5), io.cfg.vsew(1, 0))

    val alu_res =  alu.access(alu_src1, alu_src2, alu_func)

    val vlmul_left = RegInit(UInt(4.W), 11.U) // should always sync with VCFG except for the very beginning
    no_next := (state === sNone && io.cfg.vlmul === 0.U) || vlmul_left === 0.U

    when (state === sNone && io.in.fire()) {
        // not necessarily state === sNone // really?
        state := sWorking
        vlmul_left := (1.U << io.cfg.vlmul) - 1.U
        current_vs1 := io.in.bits.vs1
        current_vs2 := io.in.bits.vs2
        current_vd := io.in.bits.vd
    } .elsewhen (state === sWorking && io.vreg.wen.fire() && no_next) {
        // any instrs that come into this unit write register(s)

        // finished executing this instruction, return to idle
        state := sNone
        vlmul_left := "b11".U // reset to a state that will not affect no_next
    } .elsewhen (state === sWorking && io.vreg.wen.fire()) {
        // execute for next register with the same instruction
        vlmul_left := vlmul_left - 1.U
        current_vs1 := current_vs1 + 1.U
        current_vs2 := current_vs2 + 1.U
        current_vd := current_vd + 1.U
    }

    val wen = Wire(Bool())
    val wmask = Wire(UInt((XLEN / BLEN).W))
    val wdata = Wire(UInt(XLEN.W))

    def gen_wmask() = {
        val default_wmask = "b1111_1111".U
        val wmask_8_en = Wire(Vec(8, UInt(1.W)))
        for (i <- 0 until XLEN / BLEN) {
            wmask_8_en(i) := io.vreg.v0(Lane_number * VLEN / BLEN / NLane + i)
        }
        val wmask_8 = Mux(!vm, wmask_8_en.reduce(Cat(_, _)), default_wmask)

        val wmask_16_en = Wire(Vec(8, UInt(1.W)))
        for (i <- 0 until XLEN / BLEN / 2) {
            wmask_16_en(i * 2 + 1) := io.vreg.v0(Lane_number * VLEN / BLEN / NLane / 2 + i)
            wmask_16_en(i * 2) := io.vreg.v0(Lane_number * VLEN / BLEN / NLane / 2 + i)
        }
        val wmask_16 = Mux(!vm, wmask_16_en.reduce(Cat(_, _)), default_wmask)

        val wmask_32_en = Wire(Vec(8, UInt(1.W)))
        for (i <- 0 until XLEN / BLEN / 4) {
            wmask_32_en(i * 4 + 3) := io.vreg.v0(Lane_number * VLEN / BLEN / NLane / 4 + i)
            wmask_32_en(i * 4 + 2) := io.vreg.v0(Lane_number * VLEN / BLEN / NLane / 4 + i)
            wmask_32_en(i * 4 + 1) := io.vreg.v0(Lane_number * VLEN / BLEN / NLane / 4 + i)
            wmask_32_en(i * 4) := io.vreg.v0(Lane_number * VLEN / BLEN / NLane / 4 + i)
        }
        val wmask_32 = Mux(!vm, wmask_32_en.reduce(Cat(_, _)), default_wmask)

        val wmask_64_en = Wire(Vec(8, UInt(1.W)))
        for (i <- 0 until XLEN / BLEN) {
            wmask_64_en(i) := io.vreg.v0(Lane_number * VLEN / BLEN / NLane / 8 + i)
        }
        val wmask_64 = Mux(!vm, wmask_64_en.reduce(Cat(_, _)), default_wmask)

        val ret = MuxLookup(io.cfg.vsew, DontCare, List(
            VMUOpType.byte -> wmask_8,
            VMUOpType.half -> wmask_16,
            VMUOpType.word -> wmask_32,
            VMUOpType.elem -> wmask_64
        ))
        ret
    }
    def gen_vlen_mask() = {
        val ret = Wire(UInt(8.W))
        val vl_wmask_8 = Wire(Vec(8, UInt(1.W)))
        val vl_wmask_16 = Wire(Vec(8, UInt(1.W)))
        val vl_wmask_32 = Wire(Vec(8, UInt(1.W)))
        val vl_wmask_64 = Wire(Vec(8, UInt(1.W)))

        for (i <- 0 until XLEN / BLEN) {
            vl_wmask_8(i) := io.cfg.vlen > (Lane_number * (XLEN / BLEN) + i).asUInt
        }
        for (i <- 0 until XLEN / BLEN / 2) {
            vl_wmask_16(i * 2) := io.cfg.vlen > (Lane_number * (XLEN / BLEN / 2) + i).asUInt
            vl_wmask_16(i * 2 + 1) := io.cfg.vlen > (Lane_number * (XLEN / BLEN / 2) + i).asUInt
        }
        for (i <- 0 until XLEN / BLEN / 4) {
            vl_wmask_32(i * 4) := io.cfg.vlen > (Lane_number * (XLEN / BLEN / 4) + i).asUInt
            vl_wmask_32(i * 4 + 1) := io.cfg.vlen > (Lane_number * (XLEN / BLEN / 4) + i).asUInt
            vl_wmask_32(i * 4 + 2) := io.cfg.vlen > (Lane_number * (XLEN / BLEN / 4) + i).asUInt
            vl_wmask_32(i * 4 + 3) := io.cfg.vlen > (Lane_number * (XLEN / BLEN / 4) + i).asUInt
        }
        for (i <- 0 until XLEN / BLEN / 8) {
            vl_wmask_64(i * 8) := io.cfg.vlen > (Lane_number * (XLEN / BLEN / 8) + i).asUInt
            vl_wmask_64(i * 8 + 1) := io.cfg.vlen > (Lane_number * (XLEN / BLEN / 8) + i).asUInt
            vl_wmask_64(i * 8 + 2) := io.cfg.vlen > (Lane_number * (XLEN / BLEN / 8) + i).asUInt
            vl_wmask_64(i * 8 + 3) := io.cfg.vlen > (Lane_number * (XLEN / BLEN / 8) + i).asUInt
            vl_wmask_64(i * 8 + 4) := io.cfg.vlen > (Lane_number * (XLEN / BLEN / 8) + i).asUInt
            vl_wmask_64(i * 8 + 5) := io.cfg.vlen > (Lane_number * (XLEN / BLEN / 8) + i).asUInt
            vl_wmask_64(i * 8 + 6) := io.cfg.vlen > (Lane_number * (XLEN / BLEN / 8) + i).asUInt
            vl_wmask_64(i * 8 + 7) := io.cfg.vlen > (Lane_number * (XLEN / BLEN / 8) + i).asUInt
        }
        ret := MuxLookup(io.cfg.vsew, DontCare, List(
            VMUOpType.byte -> vl_wmask_8.reduce(Cat(_, _)),
            VMUOpType.half -> vl_wmask_16.reduce(Cat(_, _)),
            VMUOpType.word -> vl_wmask_32.reduce(Cat(_, _)),
            VMUOpType.elem -> vl_wmask_64.reduce(Cat(_, _))
        ))
        ret
    }

    wen := state === sWorking  // as long as we are at working stage, we always have something to write
    val ori_wmask = Wire(UInt((XLEN / BLEN).W))
    val vl_wmask = Wire(UInt((XLEN / BLEN).W))
    ori_wmask := gen_wmask()
    vl_wmask := gen_vlen_mask()
    wmask := ori_wmask & vl_wmask
    wdata := alu.io.out
    io.vreg.wmask := wmask
    io.vreg.wdata := wdata
    io.vreg.wen.bits := wen
    io.vreg.wen.valid := wen
    io.out.bits.scala := DontCare
    io.out.bits.busy := state === sWorking
    // io.vreg.vd :=

    io.in.ready := (state  === sNone) || (no_next)
    io.out.valid := (state === sWorking && no_next && io.vreg.wen.fire())

}
