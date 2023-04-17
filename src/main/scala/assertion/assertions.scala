package assertion

import chisel3._
import chisel3.util._

import nutcore._



class RegFileCheck extends BlackBox with HasBlackBoxInline with HasNutCoreParameter with HasRegFileParameter{
    val io = IO(new Bundle {
        val clk = Input(Clock())
        val regAddr = Input(Vec(2, UInt(log2Up(NRReg).W)))
        val regData = Input(Vec(2, UInt(XLEN.W)))
    })
    val regAddrLen = log2Up(NRReg)
    setInline("assertion_1.sv",
    s"""
      |module RegFileCheck (
      |  input clk,
      |  input [$XLEN - 1 : 0 ] regData_0,
      |  input [$XLEN - 1 : 0 ] regData_1,
      |  input [$regAddrLen - 1 : 0] regAddr_0,
      |  input [$regAddrLen - 1 : 0] regAddr_1
      |);
      |
      |
      |  property read_zero_check(reg_read_addr, reg_read_data);
      |     @(posedge clk)reg_read_addr == $regAddrLen'b0 |-> reg_read_data == $XLEN'b0;
      |  endproperty 
      |  
      |  read_zero_rf1: assert property(read_zero_check(regAddr_0, regData_0)) else $$finish; 
      |  read_zero_rf2: assert property(read_zero_check(regAddr_1, regData_1)) else $$finish;
      |endmodule
     """.stripMargin)
}

class BPUCheck extends BlackBox with HasBlackBoxInline with HasNutCoreParameter {
    val io = IO(new Bundle {
        val clk = Input(Clock())
        // check the predict of taken branch
        val phtPredictEn = Input(Bool())
        val phtPredictReadValue = Input(UInt(2.W))
        val phtPredictTaken = Input(Bool())
        // check the pht update
        val phtUpdateReadValue = Input(UInt(2.W))
        val phtUpdateValue = Input(UInt(2.W))
        val phtUpdateEn = Input(Bool())
        val taken = Input(Bool())
    })
    setInline("assertion_2.sv",
    s"""
      |module BPUCheck ( 
      |  input clk,
      |  input           phtPredictEn,
      |  input [ 1 : 0 ] phtPredictReadValue,
      |  input           phtPredictTaken,
      |  input [ 1 : 0 ] phtUpdateReadValue,
      |  input [ 1 : 0 ] phtUpdateValue,
      |  input phtUpdateEn,
      |  input taken
      |);
      |
      |  property update_pht;
      |     @(posedge clk) phtUpdateEn |-> ((phtUpdateReadValue == 2'b11 && taken &&  phtUpdateValue == 2'b11 ) or (phtUpdateReadValue == 2'b00 && !taken && phtUpdateValue == 2'b00) 
      |         or ((phtUpdateReadValue != 2'b11 ||  !taken) && (phtUpdateReadValue != 2'b00 || taken )));
      |  endproperty 
      |
      |  phtOverflow:assert property(update_pht) else $$finish;  
      |  property predict_taken;
      |     @(posedge clk)
      |       phtPredictEn && (phtPredictReadValue == 2'b11 || phtPredictReadValue ==2'b10) |=> phtPredictTaken;
      |  endproperty
      |  phtTaken: assert property(predict_taken) else $$finish;
      |
      |  property predict_not_taken;
      |     @(posedge clk)
      |       phtPredictEn && (phtPredictReadValue == 2'b00 || phtPredictReadValue ==2'b01) |=> !phtPredictTaken;
      |  endproperty 
      |  phtNotTaken: assert property(predict_not_taken) else $$finish;
      |
      |endmodule
      |""".stripMargin)
}

class CacheChecker extends BlackBox(Map("TAG_WIDTH"->19)) with HasBlackBoxResource with HasNutCoreParameter {
    val io = IO(new Bundle {
        val clk = Input(Clock())
        val mmio = Input(Bool())
        val cacheHit = Input(Bool())
        val probe = Input(Bool())
        val flush = Input(UInt(2.W))
        val stage3MainState = Input(UInt((log2Up(9)).W))
        val stage3MetaValid = Input(Bool())
        val stage3MetaDirty = Input(Bool())
        val stage3MetaTag = Input(UInt(19.W))
        val memReqValid = Input(Bool())
    })
    addResource("/assertion_cache_checker.sv")
}

class CSRChecker extends BlackBox(Map("XLEN" -> 64)) with HasBlackBoxResource with HasNutCoreParameter {
    val io = IO(new Bundle{
        val clk = Input(Clock())
        val mstatus = Input(UInt(XLEN.W))
        val mepc = Input(UInt(XLEN.W))
        val mtvec = Input(UInt(XLEN.W))
        val mie = Input(UInt(XLEN.W))
        val mideleg = Input(UInt(XLEN.W))
        val medeleg = Input(UInt(XLEN.W))
        val mcause = Input(UInt(XLEN.W))
        val causeNO = Input(UInt(XLEN.W))
        val raiseExceptionVec = Input(UInt(16.W))
        val priviledgeMode = Input(UInt(2.W))
        val raiseIntr = Input(Bool())
        val raiseTrap = Input(Bool())
        val uRet = Input(Bool())
        val mRet = Input(Bool())
        val sRet = Input(Bool())
        val instValid = Input(Bool())
    })
    addResource("/assertion_csr_checker.sv")
}


class AXIChecker extends BlackBox with HasBlackBoxResource with HasNutCoreParameter {
    val io = IO(new Bundle{
        val clk = Input(Clock())
        val axi_resetn = Input(Reset())
        val axi = new AXI4Monitor
    }) 
    addResource("/assertion_axi.sv")
}