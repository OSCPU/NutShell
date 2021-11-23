package nutcore

import chisel3._
import chisel3.util.{Cat, Decoupled}

object VXUOpType {
    /* op - unsign - [mask/compare/mdu/normal] - src1 */
    def addvv     = "b0000_0_00_00".U
    def addvx     = "b0000_0_00_01".U
    def addvi     = "b0000_0_00_10".U
    def subvv     = "b0001_0_00_00".U
    def subvx     = "b0001_0_00_01".U
    def rsubvx    = "b0010_0_00_01".U
    def rsubvi    = "b0010_0_00_10".U

    def add       = "b0000_0_00".U
    def sub       = "b0001_0_00".U
    def rsub      = "b0010_0_00".U

    def sllvv     = "b0011_0_00_00".U
    def sllvx     = "b0011_0_00_01".U
    def sllvi     = "b0011_0_00_10".U
    def srlvv     = "b0100_1_00_00".U
    def srlvx     = "b0100_1_00_01".U
    def srlvi     = "b0100_1_00_10".U
    def sravv     = "b0101_0_00_00".U
    def sravx     = "b0101_0_00_01".U
    def sravi     = "b0101_0_00_10".U

    def sll       = "b0011_0_00".U
    def srl       = "b0100_1_00".U
    def sra       = "b0101_0_00".U

    def minuvv    = "b0110_1_00_00".U
    def minuvx    = "b0110_1_00_01".U
    def minvv     = "b0111_0_00_00".U
    def minvx     = "b0111_0_00_01".U
    def maxuvv    = "b1000_1_00_00".U
    def maxuvx    = "b1000_1_00_01".U
    def maxvv     = "b1001_0_00_00".U
    def maxvx     = "b1001_0_00_01".U

    def minu      = "b0110_1_00".U
    def min       = "b0111_0_00".U
    def maxu      = "b1000_1_00".U
    def max       = "b1001_0_00".U

    def andvv     = "b1010_0_00_00".U
    def andvx     = "b1010_0_00_01".U
    def andvi     = "b1010_0_00_10".U
    def orvv      = "b1011_0_00_00".U
    def orvx      = "b1011_0_00_01".U
    def orvi      = "b1011_0_00_10".U
    def xorvv     = "b1100_0_00_00".U
    def xorvx     = "b1100_0_00_01".U
    def xorvi     = "b1100_0_00_10".U
    def mergevv   = "b1101_0_00_00".U
    def mergevx   = "b1101_0_00_01".U
    def mergevi   = "b1101_0_00_10".U

    def and       = "b1010_0_00".U
    def or        = "b1011_0_00".U
    def xor       = "b1100_0_00".U
    def merge     = "b1101_0_00".U

    def mseqvv    = "b0000_0_10_00".U
    def mseqvx    = "b0000_0_10_01".U
    def mseqvi    = "b0000_0_10_10".U
    def msnevv    = "b0001_0_10_00".U
    def msnevx    = "b0001_0_10_01".U
    def msnevi    = "b0001_0_10_10".U
    def msltuvv   = "b0010_1_10_00".U
    def msltuvx   = "b0010_1_10_01".U
    def msltvv    = "b0011_0_10_00".U
    def msltvx    = "b0011_0_10_01".U
    def msleuvv   = "b0100_1_10_00".U
    def msleuvx   = "b0100_1_10_01".U
    def msleuvi   = "b0100_1_10_10".U
    def mslevv    = "b0101_0_10_00".U
    def mslevx    = "b0101_0_10_01".U
    def mslevi    = "b0101_0_10_10".U
    def msgtuvx   = "b0110_1_10_01".U
    def msgtuvi   = "b0110_1_10_10".U
    def msgtvx    = "b0111_0_10_01".U
    def msgtvi    = "b0111_0_10_10".U

    def mseq      = "b0000_0_10".U
    def msne      = "b0001_0_10".U
    def msltu     = "b0010_1_10".U
    def mslt      = "b0011_0_10".U
    def msleu     = "b0100_1_10".U
    def msle      = "b0101_0_10".U
    def msgtu     = "b0110_1_10".U
    def msgt      = "b0111_0_10".U

    def mulvv    = "b0000_0_01_00".U
    def mulvx    = "b0000_0_01_01".U
    def mulhvv   = "b0001_0_01_00".U
    def mulhvx   = "b0001_0_01_01".U
    def mulhuvv  = "b0010_1_01_00".U
    def mulhuvx  = "b0010_1_01_01".U
    def mulhsuvv = "b0011_0_01_00".U
    def mulhsuvx = "b0011_0_01_01".U
    def divvv    = "b0100_0_01_00".U
    def divvx    = "b0100_0_01_01".U
    def divuvv   = "b0101_1_01_00".U
    def divuvx   = "b0101_1_01_01".U
    def remvv    = "b0110_0_01_00".U
    def remvx    = "b0110_0_01_01".U
    def remuvv   = "b0111_1_01_00".U
    def remuvx   = "b0111_1_01_01".U
    // mul-add : mul/div : add/sub : operand : signed : futype(2) : src(2)
    def maddvv   = "b1_0_0_1_0_01_00".U
    def maddvx   = "b1_0_0_1_0_01_01".U
    def nmsubvv  = "b1_0_1_1_0_01_00".U
    def nmsubvx  = "b1_0_1_1_0_01_01".U
    def maccvv   = "b1_0_0_0_0_01_00".U
    def maccvx   = "b1_0_0_0_0_01_01".U
    def nmsacvv  = "b1_0_1_0_0_01_00".U
    def nmsacvx  = "b1_0_1_0_0_01_01".U

    def mul      = "b0000_0_01".U
    def mulh     = "b0001_0_01".U
    def mulhu    = "b0010_1_01".U
    def mulhsu   = "b0011_0_01".U
    def div      = "b0100_0_01".U
    def divu     = "b0101_1_01".U
    def rem      = "b0110_0_01".U
    def remu     = "b0111_1_01".U

    def madd     = "b1000_0_01".U
    def nmsub    = "b1010_0_01".U
    def macc     = "b1001_0_01".U
    def nmsac    = "b1011_0_01".U

    // Logical(1) - op(2) - ResNeg(1) - Src2Neg(1) - unit(2) - src(2, unuse)
    def mandmm   = "b1_00_0_0_11_10".U
    def mnandmm  = "b1_00_1_0_11_10".U
    def mandnotmm= "b1_00_0_1_11_10".U
    def mxormm   = "b1_01_0_0_11_10".U
    def mxnormm  = "b1_01_1_0_11_10".U
    def mormm    = "b1_10_0_0_11_10".U
    def mnormm   = "b1_10_1_0_11_10".U
    def mornotmm = "b1_10_0_1_11_10".U
    def mpopcm   = "b0_00_0_0_11_10".U
    def mfirstm  = "b0_01_0_0_11_10".U

    def andmm = "b00".U
    def xormm = "b01".U
    def ormm  = "b10".U
    def popcm = "b00".U
    def firstm= "b01".U

    // reduction use compare decode space
    // op - cycle - unsigned(1) - unit(2) - src(unit)(2)
    def extxv     = "b00_01_1_11_01".U
    def mvsx      = "b01_01_1_11_01".U
    def redsumvs  = "b01_11_0_11_00".U
    // def redmaxuvs = "b??_11_1_11_00".U
    // def redmaxvvs = "b??_11_0_11_00".U
    // def redminuvs = "b??_11_1_11_00".U
    // def redminvs  = "b??_11_0_11_00".U
    def redandvs  = "b00_10_0_11_00".U
    def redorvs   = "b01_10_0_11_00".U
    def redxorvs  = "b10_10_0_11_00".U

    // def ext    = "b0001".U
    // def mv   = "b0101".U
    // def sum    = "b0111".U //default for 0.U
    // def redand = "b0010".U //default for ff.U
    // def redor  = "b0110".U //default for 0.U
    // def redxor = "b1010".U //default for 0.U
    def ext    = "b00".U
    def mv   = "b01".U
    def sum    = "b01".U
    def redand = "b00".U
    def redor  = "b01".U
    def redxor = "b10".U
    // they have different cycle, so won't mix

    def opWidth   = 9
    def isSrc1Vector(func: UInt) = func(1,0)===0.U(2.W)
    def isSrc1Imm(func: UInt) = func(1,0)===2.U
    def isSrc1Scala(func: UInt) = func(1,0)===1.U
    def isCluster(func: UInt) = func(3,2)=/=3.U
    def isNeedSub(func: UInt) = func(opWidth-1,2)=/=add
    def isReverse(func: UInt) = func(opWidth-1,2)===rsub
    def isCompare(func: UInt) = func(3,2)==="b10".U
    def isReduction(func: UInt) = (func(3,2)==="b11".U && func(1,0)=/="b10".U)
    def isMerge(func: UInt) = func(opWidth-1, 2)===merge
    def getMode(func: UInt) = func(1,0)
    def opType(func: UInt) = func(opWidth-1, 2)

    def isVMDU(func: UInt) = func(3,2)===1.U
    def isSigned1(func: UInt) = ~func(4)
    def isSigned2(func: UInt) = ~func(4) && func(opWidth-1,2)=/=mulhsu
    def isMulL(func: UInt) = func(opWidth-3,opWidth-4)===0.U || func(opWidth-1)
    def isDivL(func: UInt) = func(opWidth-3,opWidth-4)===0.U || func(opWidth-3,opWidth-4)===1.U
    def isDiv(func: UInt) = func(7)
    def isDivSign(func: UInt) = func(7) && ~func(5)
    def isMultiCycle(func: UInt) = isVMDU(func)
    def isMulAdd(func: UInt) = func(opWidth-1) && isVMDU(func)
    def isMulAddReverse(func: UInt) = isMulAdd(func) && func(opWidth-4)
    def isMulAddSub(func: UInt) = isMulAdd(func) && func(opWidth-3)
    def getMulExt(func: UInt) = Cat(isMulAdd(func), func(opWidth-3,opWidth-4))

    def isMask(func: UInt) = func(3,0)==="b1110".U
    def isMaskLogical(func: UInt) = func(opWidth-1)
    def isResNeg(func: UInt) = func(opWidth-4)
    def isSrc2Neg(func: UInt) = func(opWidth-5)
    def opMaskType(func: UInt) = func(opWidth-2, opWidth-3)

    def isJustOne(func: UInt) = (isMask(func) || (isOneCycle(func) && isReduction(func)))
    def isWriteBack(func: UInt) = isMask(func) && ~isMaskLogical(func)

    // reduction
    def isOneCycle(func: UInt) = func(6,5)===1.U
    def isTwoCycle(func: UInt) = func(6,5)===2.U
    def isThrCycle(func: UInt) = func(6,5)===3.U
    def getRedOp(func: UInt) = func(opWidth-1, opWidth-2)
}


object VMUOpType {
    // bit:     5     -      4        - 3/2  -  1/0
    // bit: load/store signed/unsigned  mode   size
    def vldbu   = "b000000".U
    def vldhu   = "b000001".U
    def vldwu   = "b000010".U
    def vlde    = "b000011".U
    def vldsbu  = "b001000".U
    def vldshu  = "b001001".U
    def vldswu  = "b001010".U
    def vldse   = "b001011".U
    def vldxbu  = "b001100".U
    def vldxhu  = "b001101".U
    def vldxwu  = "b001110".U
    def vldxe   = "b001111".U
    def vldb    = "b010000".U
    def vldh    = "b010001".U
    def vldw    = "b010010".U
    // def vlde    = "b010011".U
    def vldsb   = "b011000".U
    def vldsh   = "b011001".U
    def vldsw   = "b011010".U
    // def vldse   = "b011011".U
    def vldxb   = "b011100".U
    def vldxh   = "b011101".U
    def vldxw   = "b011110".U
    // def vldxe   = "b011111".U

    def vstb    = "b110000".U
    def vsth    = "b110001".U
    def vstw    = "b110010".U
    def vste    = "b110011".U
    def vstsb   = "b111000".U
    def vstsh   = "b111001".U
    def vstsw   = "b111010".U
    def vstse   = "b111011".U
    def vstxb   = "b111100".U
    def vstxh   = "b111101".U
    def vstxw   = "b111110".U
    def vstxe   = "b111111".U

    def isLoad(func: UInt) = !func(5)
    def isStore(func: UInt) = func(5)
    def isSigned(func: UInt) = func(4)
    def isUnit(func: UInt) = func(3,2)===0.U
    def isStrd(func: UInt) = func(3,2)===2.U
    def isIdxd(func: UInt) = func(3,2)===3.U
    def getMode(func: UInt) = func(3,2)

    def byte   = "b00".U
    def half   = "b01".U
    def word   = "b10".U
    def elem   = "b11".U
}

class VCFGIO extends NutCoreBundle {
    val vsew = Output(UInt(2.W))
    val vlmul = Output(UInt(2.W))
    val vlen = Output(UInt(XLEN.W))
    // val vediv = Output(UInt(2.W))
}

class VRegRWBus extends NutCoreBundle with HasVectorParameter {
    val vs1 = Output(UInt(5.W))
    val vs2 = Output(UInt(5.W))
    val vs3 = Output(UInt(5.W))
    val vd  = Output(UInt(5.W))
    val wen = Decoupled(Output(Bool()))
    val wmask  = Output(UInt(MLEN.W))
    val wdata = Output(UInt(VLEN.W))

    val v0    = Input(UInt(VLEN.W))
    val vsrc1 = Input(UInt(VLEN.W))
    val vsrc2 = Input(UInt(VLEN.W))
    val vsrc3 = Input(UInt(VLEN.W))
}