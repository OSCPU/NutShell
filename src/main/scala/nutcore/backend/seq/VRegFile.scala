package nutcore

import Chisel.{Cat, Decoupled, Log2, PriorityMux, log2Ceil}
import chisel3._
import chisel3.util.experimental.BoringUtils

import scala.language.postfixOps

trait HasVRegFileParameter {
    val NVReg = 32
    val NVReg_W = log2Ceil(NVReg)
    /*
    val NBank = 10
    val NFu = 4 // number of function units: ALU MDU LSU and SlideU
     */
    // currently only VMU
    val NRead = 3
    val NFu = 2
}

class VRegFileIO extends NutCoreBundle with HasVRegFileParameter with HasVectorParameter {
    val raddr = Output(Vec(NRead - 1, UInt(5.W)))
    val rdata = Input(Vec(NRead, UInt(XLEN.W)))
    val waddr = Output(UInt(5.W))
    val wdata = Output(UInt(XLEN.W))
    val wmask = Output(UInt((MLEN / NLane).W))
    val wen = Output(UInt(1.W))
    val debug = Input(Vec(NVReg, UInt(XLEN.W)))
}

class VRegFileSingleBankIO extends NutCoreBundle with HasVRegFileParameter with HasVectorParameter {
    val raddr = Output(Vec(NRead - 1, UInt(5.W)))
    val rdata = Input(Vec(NRead, UInt(XLEN.W)))
    val waddr = Output(UInt(5.W))
    val wdata = Output(UInt(XLEN.W))
    val wmask = Output(UInt((MLEN / NLane).W))
    val wen = Output(UInt(1.W))
    val debug = Input(Vec(NVReg, UInt(XLEN.W)))
}

class VRegFileSingleBank extends NutCoreModule with HasVRegFileParameter with HasVectorParameter {
    val io = IO(Flipped(new VRegFileSingleBankIO))
    // bank cannot be wrapped inside
    val rf = Reg(Vec(NVReg, UInt(XLEN.W)))
    // leave bank arbeiter logic outside reg file
    for (i <- 0 until NRead - 1) {
        io.rdata(i) := rf(io.raddr(i))
    }
    io.rdata(NRead - 1) := io.raddr(0)

    val rf_write_ori = Wire(UInt(XLEN.W))
    rf_write_ori := rf(io.waddr)
    val rf_write_data = Wire(Vec(XLEN / BLEN, UInt(BLEN.W)))
    for (i <- 0 until MLEN / NLane) {
        rf_write_data(i) := Mux(io.wmask(7 - i) && io.wen.asBool(), io.wdata((7 - i) * 8 + 7, (7 - i) * 8), rf_write_ori((7 - i) * 8 + 7, (7 - i) * 8))
    }
    when (io.wen.asBool()) {
        rf(io.waddr) := rf_write_data.reduce(Cat(_, _))
    }
    for (i <- 0 until NVReg) {
        io.debug(i) := rf(i)
    }
    // def read(addr: UInt) : UInt = Cat(rf(Cat(addr, "b00".U)), rf(Cat(addr, "b01".U)), rf(Cat(addr, "b10".U)), rf(Cat(addr, "b11".U)))
}

class VRegFile(implicit val p: NutCoreConfig) extends NutCoreModule with HasVRegFileParameter {
    val io = IO(Flipped(new VRegFileIO))

    def access(raddr: Vec[UInt], wen: Bool, waddr: UInt, wdata: UInt, wmask: UInt) = {
        io.wen := wen
        io.waddr := waddr
        io.wdata := wdata
        io.wmask := wmask
        for (i <- 0 until NRead - 1) {
            io.raddr(i) := raddr(i)
        }
        io.rdata
    }
    // a vector of single bank register file
    // val rf_vec = Vec(NBank, Module(new VRegFileSingleBank).io)
    val rf_vec = Module(new VRegFileSingleBank)
    rf_vec.io.waddr := io.waddr
    rf_vec.io.wdata := io.wdata
    rf_vec.io.wmask := io.wmask
    rf_vec.io.wen := io.wen
    for (i <- 0 until NRead - 1) {
        rf_vec.io.raddr(i) := io.raddr(i)
    }
    for (i <- 0 until NRead) {
        io.rdata(i) := rf_vec.io.rdata(i)
    }


    // debug port always from the first bank
    io.debug := rf_vec.io.debug
    /*
    def read(addr: UInt) = {
        // reading from the first bank
        rf_vec(0).read(addr)
    }
     */

    // todo: review the register file design
    // only design a single write port
}

class VRegReadIO extends NutCoreBundle with HasVRegFileParameter {
    val raddr = Input(Vec(NRead - 1, UInt(5.W)))
    val rdata = Output(Vec(NRead, UInt(XLEN.W)))
}

class VRegWriteIO extends NutCoreBundle with HasVRegFileParameter with HasVectorParameter {
    val wen = Vec(NFu, Flipped(Decoupled(Bool())))
    val wmask = Input(Vec(NFu, UInt((MLEN / NLane).W)))
    val wdata = Input(Vec(NFu,UInt(XLEN.W)))
    val waddr = Input(Vec(NFu, UInt(NVReg_W.W)))
}

class VRegArbiterIO extends NutCoreBundle with HasVRegFileParameter{
    val read = new VRegReadIO
    val write = new VRegWriteIO
    val debug = Output(Vec(NVReg, UInt(XLEN.W))) // for difftest debug
}

// note that this is an arbiter for only one lane

class VRegArbiter(implicit val p: NutCoreConfig) extends NutCoreModule with HasVRegFileParameter with HasVectorParameter {
    val io = IO(new VRegArbiterIO)

    // VRegFile inside arbiter
    val vrf = Module(new VRegFile)

    val wen = Wire(Bool())
    val wmask = Wire(UInt((MLEN / NLane).W))
    val wdata = Wire(UInt(XLEN.W))
    val waddr = Wire(UInt(NVReg_W.W))

    /*
    def read(addr: UInt) = {
        vrf.read(addr)
    }
     */
    // todo: rewrite with PriorityMux
    // for better mantainability

    when (io.write.wen(0).valid && io.write.wen(0).bits.asBool()) {
        wen := true.B
        wmask := io.write.wmask(0)
        wdata := io.write.wdata(0)
        waddr := io.write.waddr(0)
        io.write.wen(0).ready := true.B
        io.write.wen(1).ready := false.B
        //io.write.wen(2).ready := false.B
        //io.write.wen(3).ready := false.B
    } .elsewhen (io.write.wen(1).valid && io.write.wen(1).bits.asBool()) {
        wen := true.B
        wmask := io.write.wmask(1)
        wdata := io.write.wdata(1)
        waddr := io.write.waddr(1)
        io.write.wen(0).ready := false.B
        io.write.wen(1).ready := true.B
        //io.write.wen(2).ready := false.B
        //io.write.wen(3).ready := false.B
        /*
    } .elsewhen (io.write.wen(2).valid && io.write.wen(2).bits.asBool()) {
        wen := true.B
        wmask := io.write.wmask(2)
        wdata := io.write.wdata(2)
        waddr := io.write.waddr(2)
        io.write.wen(0).ready := false.B
        //io.write.wen(1).ready := false.B
        //io.write.wen(2).ready := true.B
        //io.write.wen(3).ready := false.B
    } .elsewhen (io.write.wen(3).valid && io.write.wen(3).bits.asBool()) {
        wen := true.B
        wmask := io.write.wmask(3)
        wdata := io.write.wdata(3)
        waddr := io.write.waddr(3)
        io.write.wen(0).ready := false.B
        //io.write.wen(1).ready := false.B
        //io.write.wen(2).ready := false.B
        //io.write.wen(3).ready := true.B
         */
    } .otherwise {
        wen := false.B
        wmask := DontCare
        wdata := DontCare
        waddr := DontCare
        io.write.wen(0).ready := false.B
        io.write.wen(1).ready := false.B
        //io.write.wen(2).ready := false.B
        //io.write.wen(3).ready := false.B
    }

    /*
    val wen_valid = Wire(Vec(NFu + 1, Bool()))
    val wen_sel = Wire(Vec(NFu + 1, Bool()))
    val wmask_sel = Wire(Vec(NFu + 1, UInt(MLEN.W)))
    val wdata_sel = Wire(Vec(NFu + 1, UInt(XLEN.W)))
    val waddr_sel = Wire(Vec(NFu + 1, UInt(NVReg_W.W)))
    for (i <- 0 until NFu) {
        wen_valid(i) := io.write.wen(i).valid & io.write.wen(i).bits.asBool()
        wen_sel(i) := 1.U
        wmask_sel(i) := io.write.wmask(i)
        wdata_sel(i) := io.write.wdata(i)
        waddr_sel(i) := io.write.waddr(i)
    }
    wen_valid(NFu) := true.B  // default value
    wen_sel(NFu) := false.B // default value
    wdata_sel(NFu) := DontCare // default value
    waddr_sel(NFu) := DontCare // default value
    wmask_sel(NFu) := DontCare // default value

    wen := PriorityMux(wen_valid, wen_sel)
    waddr := PriorityMux(wen_valid, waddr_sel)
    wdata := PriorityMux(wen_valid, wdata_sel)
    wmask := PriorityMux(wen_valid, wmask_sel)
     */

    val read_res = vrf.access(io.read.raddr, wen, waddr, wdata, wmask)
    io.read.rdata := read_res
    io.debug := vrf.io.debug
}

// Debug purpose only
/*
object VRegFileGen extends App {
    chisel3.Driver.execute(args, () => new VRegFile())
}
 */