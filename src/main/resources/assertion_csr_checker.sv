module CSRChecker #(
    parameter XLEN = 64
)(
    input logic clk,
    input logic [XLEN - 1 : 0 ] mstatus,
    input logic [XLEN - 1 : 0 ] mepc, 
    input logic [XLEN - 1 : 0 ] mtvec,
    input logic [XLEN - 1 : 0 ] mcause,
    input logic [XLEN - 1 : 0 ] mie,  
    input logic [XLEN - 1 : 0 ] mideleg,
    input logic [XLEN - 1 : 0 ] medeleg,
    input logic [XLEN - 1 : 0 ] causeNO,
    input logic [ 16  - 1 : 0 ] raiseExceptionVec,
    input logic [ 1 : 0 ]       priviledgeMode,
    input logic raiseTrap,
    input logic raiseIntr,
    input logic  uRet, 
    input logic  sRet, 
    input logic  mRet, 
    input logic  instValid

);
    typedef struct packed{
        logic m;
        logic h;
        logic s;
        logic u;
    } priv_t;
    typedef struct packed {
        logic             sd;
        logic [ 26 : 0  ] pad1;
        logic [  1 : 0  ] sxl;
        logic [  1 : 0  ] uxl;
        logic [  7 : 0  ] pad0;
        logic             tsr;
        logic             tw;
        logic             tvm;
        logic             mxr;
        logic             sum;
        logic             mprv;
        logic [  1 : 0  ] xs;
        logic [  1 : 0  ] fs;
        logic [  1 : 0  ] mpp;
        logic [  1 : 0  ] hpp;
        logic             spp;
        priv_t            pie;
        priv_t            ie;
    } mstatus_t;

    localparam umode_code = 2'b00;
    localparam mmode_code = 2'b11;
    localparam smode_code = 2'b01;
    localparam hmode_code = 2'b10; //reserved actually, is not hmode

    mstatus_t status;
    assign status = mstatus;
    logic m_mode_trap;
    logic [ XLEN - 1 : 0 ] mdeleg;
    assign mdeleg = raiseIntr ? mideleg : medeleg; 
    assign m_mode_trap = mdeleg[causeNO[3 : 0]] && (priviledgeMode < mmode_code);

    initial begin
        if(XLEN != 64)
            $error("Unsupported XLEN %d", XLEN);
    end

    // when trap flag raise 
    property raise_trap_mstatus_mie;
        @(posedge clk)
            raiseTrap & m_mode_trap |=> status.ie.m == 0;
    endproperty

    property raise_trap_mstatus_mpie;
        @(posedge clk)
            raiseTrap & m_mode_trap |=> status.pie.m == $past(status.ie.m);
    endproperty

    property raise_trap_mstatus_mpp;
        @(posedge clk)
            raiseTrap & m_mode_trap |=> status.mpp == $past(priviledgeMode);
    endproperty


    property raise_trap_smode_priviledge_mode;
        @(posedge clk)
            raiseTrap & !m_mode_trap |=> priviledgeMode == smode_code;
    endproperty

    property raise_trap_mmode_priviledge_mode;
        @(posedge clk)
            raiseTrap & m_mode_trap |=> priviledgeMode == mmode_code;
    endproperty
    
    check_mstatus_mie:
        assert property (raise_trap_mstatus_mpie);
    check_mstatus_mpie:
        assert property (raise_trap_mstatus_mie);
    check_mstatus_mpp:
        assert property (raise_trap_mstatus_mpp);

    property sret_mstatus_mie;
        @(posedge clk)
            sRet && instValid |=>  $past(status.pie.s) == status.ie.s ;
    endproperty

    check_sret_mstatus_mie:
        assert property (sret_mstatus_mie);

    property mret_mstatus_mie;
        @(posedge clk)
            mRet && instValid |=>  $past(status.pie.m) == status.ie.m ;
    endproperty

    check_mret_mstatus_mie:
        assert property (mret_mstatus_mie);

    property uret_mstatus_mie;
        @(posedge clk)
            uRet && instValid |=>  $past(status.pie.s) == status.ie.u ;
    endproperty

    check_uret_mstatus_mie:
        assert property (uret_mstatus_mie);


    property sret_mstatus_spp;
        @(posedge clk)
            sRet && instValid |=> priviledgeMode == {1'b0, $past(status.spp)};
    endproperty
    check_sret_mstatus_spp:
        assert property (sret_mstatus_spp);

    property mret_mstatus_mpp;
        @(posedge clk)
            mRet && instValid |=> priviledgeMode == $past(status.mpp);
    endproperty
    check_mret_mstatus_mpp:
        assert property (mret_mstatus_mpp);

    property uret_mstatus_mpp;
        @(posedge clk)
            uRet && instValid |=> priviledgeMode == umode_code;
    endproperty
    check_uret_mstatus_mpp:
        assert property(uret_mstatus_mpp);


    // priorty order
    property exc_priorty_break;
        @(posedge clk)
            raiseExceptionVec[3] && !raiseIntr |=> mcause == 64'd3;
    endproperty
    check_exc_priorty_break:
        assert property(exc_priorty_break);

    property exc_priorty_inst_pf;
        @(posedge clk)
            raiseExceptionVec[12] && !raiseIntr 
                && !raiseExceptionVec[3] |=> mcause == 64'd12;
    endproperty
    check_exc_priorty_inst_pf:
        assert property(exc_priorty_inst_pf);
    
    property exc_priorty_inst_af;
        @(posedge clk)
            raiseExceptionVec[1] && !raiseIntr 
                &&!raiseExceptionVec[12] && !raiseExceptionVec[3] 
            |=> mcause == 64'd1;
    endproperty
    check_exc_priorty_inst_af:
        assert property(exc_priorty_inst_af);

    property exc_priorty_illegal_inst;
        @(posedge clk)
            raiseExceptionVec[2] && !raiseIntr 
                && !raiseExceptionVec[1] &&!raiseExceptionVec[12] && !raiseExceptionVec[3] 
            |=> mcause == 64'd2;
    endproperty

    check_exc_priorty_illegal_inst:
        assert property(exc_priorty_illegal_inst);

endmodule
