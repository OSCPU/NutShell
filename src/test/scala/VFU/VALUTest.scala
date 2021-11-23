package nutcore
/*
import org.scalatest._
import chiseltest._
import chisel3._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation

class BasicTest extends FlatSpec with ChiselScalatestTester with Matchers {
    behavior of "Testers2"

    it should "test adds" in {
        test(new VALU) { c =>

            // 64bit add
            c.io.in.src1.poke(2333.U)
            c.io.in.src2.poke(2333.U)
            c.io.in.func.poke(VALUOpType.add64)
            c.io.out.expect(4666.U)

            // 32bit add
            c.io.in.src1.poke("h1000_0000_ffff_ffff".U)
            c.io.in.src2.poke("h1000_0000_1111_1111".U)
            c.io.in.func.poke(VALUOpType.add32)
            c.io.out.expect("h2000_0000_1111_1110".U)

            // 16bit add
            c.io.in.src1.poke("h1000_0000_0fff_ffff".U)
            c.io.in.src2.poke("hffff_0000_ffff_0001".U)
            c.io.in.func.poke(VALUOpType.add16)
            c.io.out.expect("h0fff_0000_0ffe_0000".U)

            // 8bit add
            c.io.in.src1.poke("hffff_ffff_ff00_0001".U)
            c.io.in.src2.poke("h0001_0203_a0ff_00ff".U)
            c.io.in.func.poke(VALUOpType.add8)
            c.io.out.expect("hff00_0102_9fff_0000".U)
        }
    }
    it should "test subs" in {
        test(new VALU) { c =>
            // 64bit sub
            c.io.in.src1.poke(2333.U)
            c.io.in.src2.poke(2333.U)
            c.io.in.func.poke(VALUOpType.sub64)
            c.io.out.expect(0.U)

            // 32bit sub
            c.io.in.src1.poke("hffff_ffff_0000_0000".U)
            c.io.in.src2.poke("h1111_1111_1000_0000".U)
            c.io.in.func.poke(VALUOpType.sub32)
            c.io.out.expect("heeee_eeee_f000_0000".U)

            // 16bit sub
            c.io.in.src1.poke("h1000_0000_0fff_ffff".U)
            c.io.in.src2.poke("hffff_0000_ffff_0001".U)
            c.io.in.func.poke(VALUOpType.sub16)
            c.io.out.expect("h1001_0000_1000_fffe".U)

            // 8bit sub
            c.io.in.src1.poke("hffff_ffff_ff00_0001".U)
            c.io.in.src2.poke("h0001_0203_a0ff_00ff".U)
            c.io.in.func.poke(VALUOpType.sub8)
            c.io.out.expect("hfffe_fdfc_5f01_0002".U)
        }
    }

    it should "test xor" in {
        test(new VALU) { c =>
            // 64bit sub
            c.io.in.src1.poke("hffff_ffff_ffff_ffff".U)
            c.io.in.src2.poke("hfedc_ba98_7654_3210".U)
            c.io.in.func.poke(VALUOpType.xor)
            c.io.out.expect("h0123_4567_89ab_cdef".U)
        }
    }

    it should "test or" in {
        test(new VALU) { c =>
            // 64bit sub
            c.io.in.src1.poke("hffff_0000_ffff_0000".U)
            c.io.in.src2.poke("hfedc_ba98_7654_3210".U)
            c.io.in.func.poke(VALUOpType.or)
            c.io.out.expect("hffff_ba98_ffff_3210".U)
        }
    }

    it should "test and" in {
        test(new VALU) { c =>
            // 64bit sub
            c.io.in.src1.poke("hffff_0001_ffff_0010".U)
            c.io.in.src2.poke("hfedc_ba98_7654_3210".U)
            c.io.in.func.poke(VALUOpType.and)
            c.io.out.expect("hfedc_0000_7654_0010".U)
        }
    }

    it should "test sll" in {
        test(new VALU).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
            // 64bit sll
            c.io.in.src1.poke("hfedc_ba98_7654_3210".U)
            c.io.in.src2.poke("h0000_0000_0000_0010".U)
            c.io.in.func.poke(VALUOpType.sll64)
            c.io.out.expect("hba98_7654_3210_0000".U)

            // 32bit sll
            c.io.in.src1.poke("hfedc_ba98_7654_3210".U)
            c.io.in.src2.poke("h0000_0010_0000_0008".U)
            c.io.in.func.poke(VALUOpType.sll32)
            c.io.out.expect("hba98_0000_5432_1000".U)

            // 16bit sll
            c.io.in.src1.poke("hfedc_ba98_7654_3210".U)
            c.io.in.src2.poke("h0000_0002_000a_000f".U)
            c.io.in.func.poke(VALUOpType.sll16)
            c.io.out.expect("hfedc_ea60_5000_0000".U)

            // 8bit sll
            c.io.in.src1.poke("hfedc_ba98_7654_3210".U)
            c.io.in.src2.poke("h0102_0304_0506_0700".U)
            c.io.in.func.poke(VALUOpType.sll8)
            c.io.out.expect("hfc70_d080_c000_0010".U)
        }
    }

    it should "test srl" in {
        test(new VALU).withAnnotations(Seq(WriteVcdAnnotation))  { c =>
            // 64bit srl
            c.io.in.src1.poke("hfedc_ba98_7654_3210".U)
            c.io.in.src2.poke("h0000_0000_0000_0010".U)
            c.io.in.func.poke(VALUOpType.srl64)
            c.io.out.expect("h0000_fedc_ba98_7654".U)

            // 32bit srl
            c.io.in.src1.poke("hfedc_ba98_7654_3210".U)
            c.io.in.src2.poke("h0000_0001_0000_0010".U)
            c.io.in.func.poke(VALUOpType.srl32)
            c.io.out.expect("h7f6e_5d4c_0000_7654".U)

            // 16bit srl
            c.io.in.src1.poke("hfedc_ba98_7654_3210".U)
            c.io.in.src2.poke("h0001_0001_0001_000f".U)
            c.io.in.func.poke(VALUOpType.srl16)
            c.io.out.expect("h7f6e_5d4c_3b2a_0000".U)

            // 8bit srl
            c.io.in.src1.poke("hfedc_ba98_7654_3210".U)
            c.io.in.src2.poke("h0102_0304_0506_0700".U)
            c.io.in.func.poke(VALUOpType.srl8)
            c.io.out.expect("h7f37_1709_0301_0010".U)
        }
    }

    it should "test sra" in {
        test(new VALU).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
            // 64bit sra
            c.io.in.src1.poke("hfedc_ba98_7654_3210".U)
            c.io.in.src2.poke("h0000_0000_0000_0008".U)
            c.io.in.func.poke(VALUOpType.sra64)
            c.io.out.expect("hfffe_dcba_9876_5432".U)

            // 32bit sra
            c.io.in.src1.poke("hfedc_ba98_7654_3210".U)
            c.io.in.src2.poke("h0000_0007_0000_0008".U)
            c.io.in.func.poke(VALUOpType.sra32)
            c.io.out.expect("hfffd_b975_0076_5432".U)

            // 16bit sra
            c.io.in.src1.poke("hfedc_ba98_7654_3210".U)
            c.io.in.src2.poke("h0007_0007_0007_0008".U)
            c.io.in.func.poke(VALUOpType.sra16)
            c.io.out.expect("hfffd_ff75_00ec_0032".U)

            // 8bit sra
            c.io.in.src1.poke("hfedc_ba98_7654_3210".U)
            c.io.in.src2.poke("h0002_0003_0004_0005".U)
            c.io.in.func.poke(VALUOpType.sra8)
            c.io.out.expect("hfef7_baf3_7605_3200".U)
        }
    }

    it should "test slt" in {
        test(new VALU).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
            // 64bit slt
            c.io.in.src1.poke("hfedc_ba98_7654_3210".U)
            c.io.in.src2.poke("h0000_0000_0000_0008".U)
            c.io.in.func.poke(VALUOpType.slt64)
            c.clock.step()
            c.io.out.expect("h1".U)

            // 64bit slt
            c.io.in.src1.poke("h7edc_ba98_7654_3210".U)
            c.io.in.src2.poke("h0000_0000_0000_0008".U)
            c.io.in.func.poke(VALUOpType.slt64)
            c.io.out.expect("h0".U)

            // 32bit slt
            c.io.in.src1.poke("h7edc_ba98_7654_3210".U)
            c.io.in.src2.poke("h0000_0000_7777_0008".U)
            c.io.in.func.poke(VALUOpType.slt32)
            c.clock.step()
            c.io.out.expect("h0000_0000_0000_0001".U)

            // 32bit slt
            c.io.in.src1.poke("hfedc_ba98_7a54_3210".U)
            c.io.in.src2.poke("h0000_0000_7777_0008".U)
            c.io.in.func.poke(VALUOpType.slt32)
            c.io.out.expect("h0000_0001_0000_0000".U)

            // 16bit slt
            c.io.in.src1.poke("hfedc_7a98_7a54_f210".U)
            c.io.in.src2.poke("hffff_0000_7777_0008".U)
            c.io.in.func.poke(VALUOpType.slt16)
            c.io.out.expect("h0001_0000_0000_0001".U)

            // 8bit slt
            c.io.in.src1.poke("hfedc_7a98_7a54_f210".U)
            c.io.in.src2.poke("hffff_0000_7777_0008".U)
            c.io.in.func.poke(VALUOpType.slt8)
            c.io.out.expect("h0101_0001_0001_0100".U)
        }
    }

    it should "test sltu" in {
        test(new VALU).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
            // 64bit sltu
            c.io.in.src1.poke("h0000_0000_0000_0008".U)
            c.io.in.src2.poke("hfedc_ba98_7654_3210".U)
            c.io.in.func.poke(VALUOpType.sltu64)
            c.io.out.expect("h1".U)

            // 64bit sltu
            c.io.in.src1.poke("h9edc_ba98_7654_3210".U)
            c.io.in.src2.poke("h0000_0000_0000_0008".U)
            c.io.in.func.poke(VALUOpType.sltu64)
            c.io.out.expect("h0".U)

            // 32bit sltu
            c.io.in.src1.poke("h7edc_ba98_7654_3210".U)
            c.io.in.src2.poke("h0000_0000_7777_0008".U)
            c.io.in.func.poke(VALUOpType.sltu32)
            c.clock.step()
            c.io.out.expect("h0000_0000_0000_0001".U)

            // 32bit sltu
            c.io.in.src1.poke("hfedc_ba98_7a54_3210".U)
            c.io.in.src2.poke("h0000_0000_7777_0008".U)
            c.io.in.func.poke(VALUOpType.sltu32)
            c.io.out.expect("h0000_0000_0000_0000".U)

            // 16bit sltu
            c.io.in.src1.poke("hfedc_7a98_7a54_f210".U)
            c.io.in.src2.poke("hffff_0000_7777_0008".U)
            c.io.in.func.poke(VALUOpType.sltu16)
            c.io.out.expect("h0001_0000_0000_0000".U)

            // 8bit sltu
            c.io.in.src1.poke("hfedc_7a98_7a54_f210".U)
            c.io.in.src2.poke("hffff_0000_7777_0008".U)
            c.io.in.func.poke(VALUOpType.sltu8)
            c.io.out.expect("h0101_0000_0001_0000".U)
        }
    }
}
 */