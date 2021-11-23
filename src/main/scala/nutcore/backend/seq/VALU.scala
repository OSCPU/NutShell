package nutcore

import Chisel.RegEnable
import chisel3._
import chisel3.util.{Cat, Fill}
import utils.{Debug, LookupTreeDefault}

object VALUOpType {
    // rearrange OpType according to element size
    // mostly sync with VXUOpType
    def OpLen = 8
    def add8  = "b00000_00".U
    def add16 = "b00000_01".U
    def add32 = "b00000_10".U
    def add64 = "b00000_11".U

    def sub8  = "b00001_00".U
    def sub16 = "b00001_01".U
    def sub32 = "b00001_10".U
    def sub64 = "b00001_11".U

    // 00010 reserved for rsub

    def sll8  = "b00011_00".U
    def sll16 = "b00011_01".U
    def sll32 = "b00011_10".U
    def sll64 = "b00011_11".U

    def srl8  = "b00100_00".U
    def srl16 = "b00100_01".U
    def srl32 = "b00100_10".U
    def srl64 = "b00100_11".U

    def sra8  = "b00101_00".U
    def sra16 = "b00101_01".U
    def sra32 = "b00101_10".U
    def sra64 = "b00101_11".U


    // note: normal calculation does NOT use this!!!
    // thus this encoding may NOT be the most proper choice
    // may introduce confliction with other function
    def slt8  = "b00110_00".U
    def slt16 = "b00110_01".U
    def slt32 = "b00110_10".U
    def slt64 = "b00110_11".U

    def sltu8  = "b00111_00".U
    def sltu16 = "b00111_01".U
    def sltu32 = "b00111_10".U
    def sltu64 = "b00111_11".U



    // no extra handling needed
    def xor  = "b01100".U

    def or   = "b01011".U

    def and  = "b01010".U




}

class VALUIO extends VFunctionUnitIO {
    // note that ALU does not need to be able to support VLEN
    // mixed precision results will be compressed in order to make sure they fit in XLEN
}

class VALU extends NutCoreModule {
    val io = IO(new VALUIO)
    def access(src1: UInt, src2: UInt, func: UInt) = {
        io.in.src1 := src1
        io.in.src2 := src2
        io.in.func := func
        io.out
    }

    val (src1, src2, func) = (io.in.src1, io.in.src2,  io.in.func)

    val isAdd_64 = VALUOpType.add64 === io.in.func
    val isAdd_32 = VALUOpType.add32 === io.in.func
    val isAdd_16 = VALUOpType.add16 === io.in.func
    val isAdd_8 = VALUOpType.add8 === io.in.func
    val isAdd = isAdd_64 | isAdd_32 | isAdd_16 | isAdd_8


    val isSll_64 = VALUOpType.sll64 === io.in.func
    val isSll_32 = VALUOpType.sll32 === io.in.func
    val isSll_16 = VALUOpType.sll16 === io.in.func
    val isSll_8 = VALUOpType.sll8 === io.in.func
    val isSll = isSll_64 | isSll_32 | isSll_16 | isSll_8

    val isSlt_64 = VALUOpType.slt64 === io.in.func
    val isSlt_32 = VALUOpType.slt32 === io.in.func
    val isSlt_16 = VALUOpType.slt16 === io.in.func
    val isSlt_8 = VALUOpType.slt8 === io.in.func
    val isSlt = isSlt_64 | isSlt_32 | isSlt_16 | isSlt_8

    val isSltu_64 = VALUOpType.sltu64 === io.in.func
    val isSltu_32 = VALUOpType.sltu32 === io.in.func
    val isSltu_16 = VALUOpType.sltu16 === io.in.func
    val isSltu_8 = VALUOpType.sltu8 === io.in.func
    val isSltu = isSltu_64 | isSltu_32 | isSltu_16 | isSltu_8

    val isSrl_64 = VALUOpType.srl64 === io.in.func
    val isSrl_32 = VALUOpType.srl32 === io.in.func
    val isSrl_16 = VALUOpType.srl16 === io.in.func
    val isSrl_8 = VALUOpType.srl8 === io.in.func
    val isSrl = isSrl_64 | isSrl_32 | isSrl_16 | isSrl_8

    val isXor = VALUOpType.xor === io.in.func(6, 2)
    val isOr = VALUOpType.or === io.in.func(6, 2)
    val isAnd = VALUOpType.and === io.in.func(6, 2)

    val isSub_64 = VALUOpType.sub64 === io.in.func
    val isSub_32 = VALUOpType.sub32 === io.in.func
    val isSub_16 = VALUOpType.sub16 === io.in.func
    val isSub_8 = VALUOpType.sub8 === io.in.func
    val isSub = isSub_64 | isSub_32 | isSub_16 | isSub_8

    val isSra_64 = VALUOpType.sra64 === io.in.func
    val isSra_32 = VALUOpType.sra32 === io.in.func
    val isSra_16 = VALUOpType.sra16 === io.in.func
    val isSra_8 = VALUOpType.sra8 === io.in.func
    val isSra = isSra_64 | isSra_32 | isSra_16 | isSra_8

    def MixPrecisionLen = XLEN + XLEN / 8

    val adderRes_ori = Wire(UInt(MixPrecisionLen.W))
    val adderRes = Wire(UInt(XLEN.W))
    val add1 = Wire(UInt(MixPrecisionLen.W))
    val add2 = Wire(UInt(MixPrecisionLen.W))

    // todo: if possible, rewrite add_src_map like adder_gather
    def add_src_map(width: Int, src:UInt, isSub: Bool) = {
        var l = List(0.U)
        for (i <- 0 until XLEN / width) {
            val tmp = (src(i * width + width - 1, i * width) ^ Fill(width, isSub)) + isSub
            val tmp_list = List.concat(List(0.U), List(tmp))
            l = List.concat(tmp_list ,l)
        }
        l.dropRight(1).reduce(Cat(_, _)) // drop leading zero which we added for convenience
    }

    if (XLEN == 32) {
        when (isAdd_8 | isSub_8) {
            add1 := add_src_map(8, src1, 0.B)
            add2 := add_src_map(8, src2, isSub_8)
        } .elsewhen (isAdd_16 | isSub_16) {
            add1 := add_src_map(16, src1, 0.B)
            add2 := add_src_map(16, src2, isSub_16)
        } .elsewhen (isAdd_32 | isSub_32) {
            add1 := add_src_map(32, src1, 0.B)
            add2 := add_src_map(32, src2, isSub_32)
        } .otherwise {
            add1 := DontCare
            add2 :=DontCare
        }
    } else if (XLEN == 64) {
        when (isAdd_8 | isSub_8) {
            add1 := add_src_map(8, src1, 0.B)
            add2 := add_src_map(8, src2, isSub_8)
        } .elsewhen (isAdd_16 | isSub_16) {
            add1 := add_src_map(16, src1, 0.B)
            add2 := add_src_map(16, src2, isSub_16)
        } .elsewhen (isAdd_32 | isSub_32) {
            add1 := add_src_map(32, src1, 0.B)
            add2 := add_src_map(32, src2, isSub_32)
        } .elsewhen (isAdd_64 | isSub_64) {
            add1 := add_src_map(64, src1, 0.B)
            add2 := add_src_map(64,  src2, isSub_64)
        } .otherwise {
            add1 := DontCare
            add2 :=DontCare
        }
    } else {
        Debug(prefix = true, "Unexpected XLEN for VALU")
        add1 := DontCare
        add2 := DontCare
    }
    adderRes_ori := add1 +& add2

    def gather_offset(width: Int, index: Int) = (width + 1) * index + width - 1

    def gather_offset_end(width: Int, index: Int) = gather_offset(width, index) - width + 1

    def adder_gather(adderRes_ori: UInt, width: Int) = {
        var l: List[UInt] = List(adderRes_ori(gather_offset(width, 0), gather_offset_end(width, 0)))
        if ((XLEN / width - 2) >= 0) {
            for (i <- 1 until XLEN / width) {
                l =  List.concat(List(adderRes_ori(gather_offset(width, i), gather_offset_end(width, i))), l)
            }
        }
        l.reduce(Cat(_, _))
    }
    when (isAdd_8 | isSub_8) {
        adderRes := adder_gather(adderRes_ori, 8)
    } .elsewhen (isAdd_16 | isSub_16) {
        adderRes := adder_gather(adderRes_ori, 16)
    } .elsewhen (isAdd_32 | isSub_32) {
        adderRes := adder_gather(adderRes_ori, 32)
    } .elsewhen(isAdd_64 | isSub_64) {
        adderRes := adder_gather(adderRes_ori, 64)
    } .otherwise {
        adderRes := DontCare
    }


    val andRes = src1 & src2
    val orRes = src1 | src2
    val xorRes = src1 ^ src2

    // todo: deal with mixed precision slt sltu
    private def comparator_eq(width: Int, src0: UInt, src1: UInt) = {
        src0 === src1
    }
    private def comparator_ltu(width: Int, src0: UInt, src1:UInt) = {
        src0 < src1
    }
    private def comparator_lt(width: Int, src0: UInt, src1: UInt) = {
        val src0_signed = src0.asSInt()
        val src1_signed = src1.asSInt()
        src0_signed < src1_signed
    }

    // compose a 64 bit comparator
    val eq_vec = Wire(Vec(XLEN / 8, UInt(1.W)))
    val lt_vec = Wire(Vec(XLEN / 8, UInt(1.W)))
    val ltu_vec = Wire(Vec(XLEN / 8, UInt(1.W)))
    for (i <- 0 until XLEN / 8) {
        val j = XLEN / 8 - i - 1
        eq_vec(i) := comparator_eq(8, src1(j * 8 + 7, j * 8), src2(j * 8 + 7, j * 8))
        lt_vec(i) := comparator_lt(8, src1(j * 8 + 7, j * 8), src2(j * 8 + 7, j * 8))
        ltu_vec(i) := comparator_ltu(8, src1(j * 8 + 7, j * 8), src2(j * 8 + 7, j * 8))
    }

    // XLEN / 4 - 1: (RV64) 8 + 4 + 2 + 1 / (RV32) 4 + 2 + 1
    val eq_res = Wire(Vec(XLEN / 4 - 1, UInt(1.W)))
    val lt_res = Wire(Vec(XLEN / 4 - 1, UInt(1.W)))
    val ltu_res = Wire(Vec(XLEN / 4 - 1, UInt(1.W)))

    def handle_eq() = {
        var eq_res = Wire(Vec(XLEN / 4 - 1, UInt(1.W)))
        for (i <- 0 until XLEN / 8) {
            eq_res(i) := eq_vec(i)
        }
        var divisor = 8
        var base = XLEN / divisor

        while (XLEN / divisor > 0) {
            for (i <- base until base + XLEN / divisor / 2) {
                val offset = i - base
                var l = List(eq_vec(offset * divisor / 4))
                for (j <- offset * (divisor / 4) + 1 until (offset + 1) * (divisor / 4)) {
                    l = List.concat(l, List(eq_vec(j)))
                }
                eq_res(i) := l.reduce(Cat(_, _)).andR()
            }
            divisor = divisor * 2
            base = base + XLEN / divisor
        }
        eq_res
    }
    eq_res := handle_eq()

    // generic for lt and ltu
    def handle_lt(lt_vec: Vec[UInt]) = {
        var lt_res = Wire(Vec(XLEN / 4 - 1, UInt(1.W)))
        for (i <- 0 until XLEN / 8) {
            lt_res(i) := lt_vec(i)
        }
        var divisor = 8
        var base = XLEN / divisor
        while (XLEN / divisor > 0) {
            for (i <- base until base + XLEN / divisor / 2) {
                val offset = i - base
                var l = List(1.U) // when and, true is harmless
                var res_l = List(0.U) // when or, false is harmless
                for (j <- offset * (divisor / 4) until (offset + 1) * (divisor / 4) ) {
                    val tmp_list = List.concat(l, List(lt_vec(j)))
                    res_l = List.concat(res_l, List(tmp_list.reduce(Cat(_, _)).andR().asUInt() ))
                    l = List.concat(l, List(eq_vec(j)))
                }
                lt_res(i) := res_l.reduce(Cat(_, _)).orR()
            }
            divisor = divisor * 2
            base = base + XLEN / divisor
        }
        lt_res
    }
    def handle_ltu(ltu_vec: Vec[UInt]) = {
        var ltu_res = Wire(Vec(XLEN / 4 - 1, UInt(1.W)))
        for (i <- 0 until XLEN / 8) {
            ltu_res(i) := ltu_vec(i)
        }
        var divisor = 8
        var base = XLEN / divisor
        while (XLEN / divisor > 0) {
            for (i <- base until base + XLEN / divisor / 2) {
                val offset = i - base
                var l = List(1.U) // when and, true is harmless
                var res_l = List(0.U) // when or, false is harmless
                for (j <- offset * (divisor / 4) until (offset + 1) * (divisor / 4) ) {
                    val tmp_list = List.concat(l, List(ltu_vec(j)))
                    res_l = List.concat(res_l, List(tmp_list.reduce(Cat(_, _)).andR().asUInt() ))
                    l = List.concat(l, List(eq_vec(j)))
                }
                ltu_res(i) := res_l.reduce(Cat(_, _)).orR()
            }
            divisor = divisor * 2
            base = base + XLEN / divisor
        }
        ltu_res
    }
    lt_res := handle_lt(lt_vec)
    ltu_res := handle_ltu(ltu_vec)

    val ltuRes = Wire(UInt(XLEN.W))
    val ltRes = Wire(UInt(XLEN.W))
    val zero_7 = Wire(UInt(7.W)) // "b0000_000".U
    val zero_15 = Wire(UInt(15.W)) // "b0000_0000_0000_000".U
    val zero_31 = Wire(UInt(31.W))  // "b0000_0000_0000_0000_0000_0000_0000_000".U
    val zero_63 = Wire(UInt(63.W)) // "b0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_000".U
    zero_7 := 0.U
    zero_15 := 0.U
    zero_31 := 0.U
    zero_63 := 0.U

    if (XLEN == 64) {
        when (isSlt_8) {
            ltRes := Cat(zero_7, lt_res(0), zero_7, lt_res(1), zero_7, lt_res(2), zero_7, lt_res(3), zero_7, lt_res(4), zero_7, lt_res(5), zero_7, lt_res(6), zero_7, lt_res(7))
        } .elsewhen (isSlt_16) {
            ltRes := Cat(zero_15, lt_res(8), zero_15, lt_res(9), zero_15, lt_res(10), zero_15, lt_res(11))
        } .elsewhen (isSlt_32) {
            ltRes := Cat(zero_31, lt_res(12), zero_31, lt_res(13))
        } .elsewhen (isSlt_64) {
            ltRes := Cat(zero_63, lt_res(14))
        } .otherwise {
            ltRes := DontCare
        }

        when (isSltu_8) {
            ltuRes := Cat(zero_7, ltu_res(0), zero_7, ltu_res(1), zero_7, ltu_res(2), zero_7, ltu_res(3), zero_7, ltu_res(4), zero_7, ltu_res(5), zero_7, ltu_res(6), zero_7, ltu_res(7))
        } .elsewhen (isSltu_16) {
            ltuRes := Cat(zero_15, ltu_res(8), zero_15, ltu_res(9), zero_15, ltu_res(10), zero_15, ltu_res(11))
        } .elsewhen (isSltu_32) {
            ltuRes := Cat(zero_31, ltu_res(12), zero_31, ltu_res(13))
        } .elsewhen (isSltu_64) {
            ltuRes := Cat(zero_63, ltu_res(14))
        } .otherwise {
            ltuRes := DontCare
        }

    } else if (XLEN == 32) {
        when (isSlt_8) {
            ltRes := Cat(zero_7, lt_res(0), zero_7, lt_res(1), zero_7, lt_res(2), zero_7, lt_res(3), zero_7, lt_res(4), zero_7, lt_res(5), zero_7, lt_res(6), zero_7, lt_res(7))
        } .elsewhen (isSlt_16) {
            ltRes := Cat(zero_15, lt_res(8), zero_15, lt_res(9), zero_15, lt_res(10), zero_15, lt_res(11))
        } .elsewhen (isSlt_32) {
            ltRes := Cat(zero_31, lt_res(12), zero_31, lt_res(13))
        } .otherwise {
            ltRes := DontCare
        }

        when (isSltu_8) {
            ltuRes := Cat(zero_7, ltu_res(0), zero_7, ltu_res(1), zero_7, ltu_res(2), zero_7, ltu_res(3), zero_7, ltu_res(4), zero_7, ltu_res(5), zero_7, ltu_res(6), zero_7, ltu_res(7))
        } .elsewhen (isSltu_16) {
            ltuRes := Cat(zero_15, ltu_res(8), zero_15, ltu_res(9), zero_15, ltu_res(10), zero_15, ltu_res(11))
        } .elsewhen (isSltu_32) {
            ltuRes := Cat(zero_31, ltu_res(12), zero_31, ltu_res(13))
        } .otherwise {
            ltuRes := DontCare
        }

    } else {
        // problem here
        assert(cond = false, "Unsupported XLEN for VPU")
        ltuRes := DontCare
        ltRes := DontCare
    }

    val sllRes = Wire(UInt(XLEN.W))
    val srlRes = Wire(UInt(XLEN.W))
    val sraRes = Wire(UInt(XLEN.W))
    def shamt_len(len: Int) = {
        len match {
            case 8 => 3
            case 16 => 4
            case 32 => 5
            case 64 => 6
            case _ => {
                assert(false, "Unsupported shamt_len")
                0
            }
        }
    }
    val shamt_8 = Wire(Vec(XLEN / 8, UInt(shamt_len(8).W)))
    val shamt_16 = Wire(Vec(XLEN / 16, UInt(shamt_len(16).W)))
    val shamt_32 = Wire(Vec(XLEN / 32, UInt(shamt_len(32).W)))
    val shamt_64 = Wire(Vec(XLEN / 64, UInt(shamt_len(64).W)))

    def shamt_gen(width: Int) = {
        var ret = Wire(Vec(XLEN / width, UInt(shamt_len(width).W)))
        for (i <- 0 until XLEN / width) {
            ret(i) := src2(i * width + shamt_len(width) - 1, i * width)
        }
        ret.reverse
    }
    shamt_8 := shamt_gen(8)
    shamt_16 := shamt_gen(16)
    shamt_32 := shamt_gen(32)
    shamt_64 := shamt_gen(64)

    def shift_src_gen(width: Int) = {
        var ret = Wire(Vec(XLEN / width, UInt(width.W)))
        for (i <- 0 until XLEN / width) {
            ret(i) := src1((i + 1) * width - 1, i * width)
        }
        ret.reverse
    }

    val shift_src_8 = Wire(Vec(XLEN / 8, UInt(8.W)))
    val shift_src_16 = Wire(Vec(XLEN / 16, UInt(16.W)))
    val shift_src_32 = Wire(Vec(XLEN / 32, UInt(32.W)))
    val shift_src_64 = Wire(Vec(XLEN / 64, UInt(64.W)))

    shift_src_8 := shift_src_gen(8)
    shift_src_16 := shift_src_gen(16)
    shift_src_32 := shift_src_gen(32)
    shift_src_64 := shift_src_gen(64)

    val sllRes_8 = Wire(Vec(XLEN / 8, UInt(8.W)))
    val sllRes_16 = Wire(Vec(XLEN / 16, UInt(16.W)))
    val sllRes_32 = Wire(Vec(XLEN / 32, UInt(32.W)))
    val sllRes_64 = Wire(Vec(XLEN / 64, UInt(64.W)))

    val srlRes_8 = Wire(Vec(XLEN / 8, UInt(8.W)))
    val srlRes_16 = Wire(Vec(XLEN / 16, UInt(16.W)))
    val srlRes_32 = Wire(Vec(XLEN / 32, UInt(32.W)))
    val srlRes_64 = Wire(Vec(XLEN / 64, UInt(64.W)))

    val sraRes_8 = Wire(Vec(XLEN / 8, UInt(8.W)))
    val sraRes_16 = Wire(Vec(XLEN / 16, UInt(16.W)))
    val sraRes_32 = Wire(Vec(XLEN / 32, UInt(32.W)))
    val sraRes_64 = Wire(Vec(XLEN / 64, UInt(64.W)))

    for (i <- 0 until XLEN / 8) {
        sllRes_8(i) := shift_src_8(i) << shamt_8(i)
        srlRes_8(i) := shift_src_8(i) >> shamt_8(i)
        sraRes_8(i) := (shift_src_8(i).asSInt() >> shamt_8(i)).asUInt()
    }
    for (i <- 0 until XLEN / 16) {
        sllRes_16(i) := shift_src_16(i) << shamt_16(i)
        srlRes_16(i) := shift_src_16(i) >> shamt_16(i)
        sraRes_16(i) := (shift_src_16(i).asSInt() >> shamt_16(i)).asUInt()
    }
    for (i <- 0 until XLEN / 32) {
        sllRes_32(i) := shift_src_32(i) << shamt_32(i)
        srlRes_32(i) := shift_src_32(i) >> shamt_32(i)
        sraRes_32(i) := (shift_src_32(i).asSInt() >> shamt_32(i)).asUInt()
    }

    if (XLEN == 64) {
        for (i <- 0 until XLEN / 64) {
            sllRes_64(i) := shift_src_64(i) << shamt_64(i)
            srlRes_64(i) := shift_src_64(i) >> shamt_64(i)
            sraRes_64(i) := (shift_src_64(i).asSInt() >> shamt_64(i)).asUInt()
        }
    } else {
        for (i <- 0 until XLEN / 64) {
            sllRes_64(i) := DontCare
        }
    }

    if (XLEN == 64) {
        when (isSll_8) {
            sllRes := sllRes_8.reduce(Cat(_, _))
        } .elsewhen (isSll_16) {
            sllRes := sllRes_16.reduce(Cat(_, _))
        } .elsewhen (isSll_32) {
            sllRes := sllRes_32.reduce(Cat(_, _))
        } .elsewhen (isSll_64) {
            sllRes := sllRes_64.reduce(Cat(_, _))
        } .otherwise {
            sllRes := DontCare
        }
        when (isSrl_8) {
            srlRes := srlRes_8.reduce(Cat(_, _))
        } .elsewhen (isSrl_16) {
            srlRes := srlRes_16.reduce(Cat(_, _))
        } .elsewhen (isSrl_32) {
            srlRes := srlRes_32.reduce(Cat(_, _))
        } .elsewhen (isSrl_64) {
            srlRes := srlRes_64.reduce(Cat(_, _))
        } .otherwise {
            srlRes := DontCare
        }
        when (isSra_8) {
            sraRes := sraRes_8.reduce(Cat(_, _))
        } .elsewhen (isSra_16) {
            sraRes := sraRes_16.reduce(Cat(_, _))
        } .elsewhen (isSra_32) {
            sraRes := sraRes_32.reduce(Cat(_, _))
        } .elsewhen (isSra_64) {
            sraRes := sraRes_64.reduce(Cat(_, _))
        } .otherwise {
            sraRes := DontCare
        }
    } else if (XLEN == 32) {
        when (isSll_8) {
            sllRes := sllRes_8.reduce(Cat(_, _))
        } .elsewhen (isSll_16) {
            sllRes := sllRes_16.reduce(Cat(_, _))
        } .elsewhen (isSll_32) {
            sllRes := sllRes_32.reduce(Cat(_, _))
        } .otherwise {
            sllRes := DontCare
        }
        when (isSrl_8) {
            srlRes := srlRes_8.reduce(Cat(_, _))
        } .elsewhen (isSrl_16) {
            srlRes := srlRes_16.reduce(Cat(_, _))
        } .elsewhen (isSrl_32) {
            srlRes := srlRes_32.reduce(Cat(_, _))
        } .otherwise {
            srlRes := DontCare
        }
        when (isSra_8) {
            sraRes := sraRes_8.reduce(Cat(_, _))
        } .elsewhen (isSra_16) {
            sraRes := sraRes_16.reduce(Cat(_, _))
        } .elsewhen (isSra_32) {
            sraRes := sraRes_32.reduce(Cat(_, _))
        } .otherwise {
            sraRes := DontCare
        }
    } else {
        // invalid state
        sllRes := DontCare
        srlRes := DontCare
        sraRes := DontCare
    }

    when (isAdd | isSub) {
        io.out := adderRes
    } .elsewhen (isAnd) {
        io.out := andRes
    } .elsewhen (isOr) {
        io.out := orRes
    } .elsewhen (isXor) {
        io.out := xorRes
    } .elsewhen (isSlt) {
        io.out := ltRes
    } .elsewhen (isSltu) {
        io.out := ltuRes
    } .elsewhen (isSll) {
        io.out := sllRes
    } .elsewhen (isSrl) {
        io.out := srlRes
    } .elsewhen (isSra) {
        io.out := sraRes
    } .otherwise {
        io.out := DontCare
    }
}

// Debug purpose only
object VALUGen extends App {
    chisel3.Driver.execute(args, () => new VALU)
}


// unify IO port between MDU and VALU
class VALU_ClusterIO extends NutCoreModule {
    val io = IO(new ClusterIO)
    val (valid, ready, src1, src2, src3, func, vsew, maskv0) = (io.in.valid, io.out.ready, io.in.bits.src1, io.in.bits.src2, io.in.bits.src3, io.in.bits.func, io.vsew, io.in.bits.maskv0)
    def access(valid: Bool, src1: UInt, src2: UInt, src3: UInt, func: UInt, vsew: UInt, ready: Bool, maskv0: UInt) = {
        this.valid := valid
        this.src1 := Mux(VXUOpType.isReverse(func), src2, src1)
        this.src2 := Mux(VXUOpType.isReverse(func), src1, src2)
        this.src3 := src3
        this.func := func
        this.vsew := vsew
        this.ready := ready
        this.maskv0 := maskv0
        (io.in.ready, io.out.bits, io.out.valid)
    }

    val valu_valid = RegInit(0.B);
    when (io.in.fire()) {
        valu_valid := true.B
    } .elsewhen (io.out.fire()) {
        valu_valid := false.B
    }

    val valu = Module(new VALU)
    val valufunc = LookupTreeDefault(Cat(func, vsew), 0.U, List(
        Cat(VXUOpType.add, VMUOpType.byte) -> VALUOpType.add8,
        Cat(VXUOpType.add, VMUOpType.half) -> VALUOpType.add16,
        Cat(VXUOpType.add, VMUOpType.word) -> VALUOpType.add32,
        Cat(VXUOpType.add, VMUOpType.elem) -> VALUOpType.add64,
        Cat(VXUOpType.sub, VMUOpType.byte) -> VALUOpType.sub8,
        Cat(VXUOpType.sub, VMUOpType.half) -> VALUOpType.sub16,
        Cat(VXUOpType.sub, VMUOpType.word) -> VALUOpType.sub32,
        Cat(VXUOpType.sub, VMUOpType.elem) -> VALUOpType.sub64,
        Cat(VXUOpType.sll, VMUOpType.byte) -> VALUOpType.sll8,
        Cat(VXUOpType.sll, VMUOpType.half) -> VALUOpType.sll16,
        Cat(VXUOpType.sll, VMUOpType.word) -> VALUOpType.sll32,
        Cat(VXUOpType.sll, VMUOpType.elem) -> VALUOpType.sll64,
        Cat(VXUOpType.srl, VMUOpType.byte) -> VALUOpType.srl8,
        Cat(VXUOpType.srl, VMUOpType.half) -> VALUOpType.srl16,
        Cat(VXUOpType.srl, VMUOpType.word) -> VALUOpType.srl32,
        Cat(VXUOpType.srl, VMUOpType.elem) -> VALUOpType.srl64,
        Cat(VXUOpType.sra, VMUOpType.byte) -> VALUOpType.sra8,
        Cat(VXUOpType.sra, VMUOpType.half) -> VALUOpType.sra16,
        Cat(VXUOpType.sra, VMUOpType.word) -> VALUOpType.sra32,
        Cat(VXUOpType.sra, VMUOpType.elem) -> VALUOpType.sra64,
        Cat(VXUOpType.and, VMUOpType.byte) -> VALUOpType.and,
        Cat(VXUOpType.and, VMUOpType.half) -> VALUOpType.and,
        Cat(VXUOpType.and, VMUOpType.word) -> VALUOpType.and,
        Cat(VXUOpType.and, VMUOpType.elem) -> VALUOpType.and,
        Cat(VXUOpType.or, VMUOpType.byte) -> VALUOpType.or,
        Cat(VXUOpType.or, VMUOpType.half) -> VALUOpType.or,
        Cat(VXUOpType.or, VMUOpType.word) -> VALUOpType.or,
        Cat(VXUOpType.or, VMUOpType.elem) -> VALUOpType.or,
        Cat(VXUOpType.xor, VMUOpType.byte) -> VALUOpType.xor,
        Cat(VXUOpType.xor, VMUOpType.half) -> VALUOpType.xor,
        Cat(VXUOpType.xor, VMUOpType.word) -> VALUOpType.xor,
        Cat(VXUOpType.xor, VMUOpType.elem) -> VALUOpType.xor
        // Cat(VXUOpType.mslt, VMUOpType.byte) -> VALUOpType.sra8,
        // todo: rsub, compare (slt sltu etc)
    ))

    // result of ALU
    val aluRes = valu.access(RegEnable(src1, io.in.fire()), RegEnable(src2, io.in.fire()), RegEnable(valufunc, io.in.fire()))

    io.out.bits := aluRes
    io.in.ready := (!valu_valid) || io.out.fire() // always ready for new input
    io.out.valid := valid // always depends on input
}