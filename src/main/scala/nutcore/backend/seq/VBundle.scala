package nutcore

import chisel3._
import chisel3.util.Decoupled

class VFunctionUnitIO extends NutCoreBundle {
    val in = Flipped(new Bundle {
        val src1 = Output(UInt(XLEN.W))
        val src2 = Output(UInt(XLEN.W))
        val func = Output(FuOpType())
    })
    val out = Output(UInt(XLEN.W))
}

class VConfigIO extends NutCoreBundle {
    val ALUOp = Input(UInt(7.W))
}

class VRedirectIO extends NutCoreBundle {
    // mainly for branch related, should we really care?
}




