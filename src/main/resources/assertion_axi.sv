module AXIChecker#(
    parameter INPUT_WIDTH = 32
)(

    input logic clk							,
    input logic axi_resetn					,

    input logic             axi_awready		,
    input logic             axi_awvalid		,
    input logic [31:0]      axi_awaddr		,
    input logic [2:0]       axi_awprot		,
    input logic [1:0]       axi_awburst		,
    input logic             axi_wready		,
    input logic             axi_wvalid		,
    input logic [63:0]      axi_wdata		,
    input logic             axi_wlast		,
    input logic             axi_bready		,
    input logic             axi_bvalid		,
    input logic [1:0]       axi_bresp		,
    input logic             axi_arready		,
    input logic             axi_arvalid		,
    input logic [31:0]      axi_araddr		,
    input logic [2:0]       axi_arprot		,
    input logic [2:0]       axi_arsize		,
    input logic [1:0]       axi_arburst		,
    input logic             axi_rready		,
    input logic             axi_rvalid		,
    input logic [1:0]       axi_rresp		,
    input logic [63:0]      axi_rdata		,
    input logic             axi_rlast		,
    input logic             axi_awid		,
    input logic             axi_awuser 		,
    input logic [7:0]       axi_awlen 		,
    input logic [2:0]       axi_awsize 		,
    input logic             axi_awlock  	,
    input logic [3:0]       axi_awcache  	,
    input logic [3:0]       axi_awqos    	,
    input logic [7:0]       axi_wstrb		,
    input logic             axi_bid			,
    input logic             axi_buser		,
    input logic             axi_arid       	,
    input logic             axi_aruser      ,
    input logic [7:0]       axi_arlen		,
    input logic             axi_arlock   	,
    input logic [3:0]       axi_arcache 	,
    input logic [3:0]       axi_arqos		,
    input logic             axi_rid       	,
    input logic             axi_ruser
);

    // 1 
    //keep awlast stable while no accepted
    property wlast_keep_stable;
        @(posedge clk)
            axi_awvalid && !axi_awready && axi_resetn 
            |=> axi_awvalid && $stable(axi_wlast);
    endproperty:wlast_keep_stable

    assert_last_keep_stable: 
        assert property (wlast_keep_stable) //////$display ("OK last_keep_stable is assert");
        else $warning ("you are  writing last_keep_stable shit ");

    // 2
    // keep stable while no accepted
    property aw_data_keep_stable;
        @(posedge clk)
            axi_awvalid && !axi_awready && axi_resetn 
            |=> axi_awvalid && $stable(axi_wdata) && $stable(axi_wlast);
    endproperty:aw_data_keep_stable

    assert_wdata: 
        assert property (aw_data_keep_stable) //$display ("OK aw_data_keep_stable is assert");
        else $warning ("you are  aw_data_keep_stable wready_reset shit ");


    // 4
    //keep axi_rlast stable while no accepted
    property r_last_keep_stable;
        @(posedge clk)
            axi_rvalid && !axi_rready && axi_resetn 
            |=> axi_rvalid && $stable(axi_rlast);
    endproperty:r_last_keep_stable

    assert_r_last_keep: 
        assert property (r_last_keep_stable) //$display ("OK r_last_keep_stable is assert");
        else $warning ("you are  writing r_last_keep_stable shit ");

    //5
    // keep stable while no accepted
    property r_data_keep_stable;
        @(posedge clk)
            axi_rvalid && !axi_rready && axi_resetn 
            |=> axi_rvalid && $stable(axi_rdata) && $stable(axi_rlast);
    endproperty:r_data_keep_stable

    assert_r_keep_stable_rdata: 
        assert property (r_data_keep_stable) //$display ("OK r_data_keep_stable is assert");
        else $warning ("you are eriting r_data_keep_stable  shit ");

     property araddr_keep_stable;
        @(posedge clk)
            axi_arvalid && !axi_arready && axi_resetn 
            |=> axi_rvalid && $stable(axi_araddr);
    endproperty:araddr_keep_stable

    assert_araddr_keep_stable: 
        assert property (araddr_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  araddr_stable shit ");
    
    property arprot_keep_stable;
        @(posedge clk)
            axi_arvalid && !axi_arready && axi_resetn 
            |=> axi_rvalid && $stable(axi_arprot);
    endproperty:arprot_keep_stable

    assert_arprot_keep_stable: 
        assert property (arprot_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  arprot_stable shit ");
    
    property arsize_keep_stable;
        @(posedge clk)
            axi_arvalid && !axi_arready && axi_resetn 
            |=> axi_rvalid && $stable(axi_arsize);
    endproperty:arsize_keep_stable

    assert_arsize_keep_stable: 
        assert property (arsize_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  arsize_stable shit ");
    
    property arburst_keep_stable;
        @(posedge clk)
            axi_arvalid && !axi_arready && axi_resetn 
            |=> axi_rvalid && $stable(axi_arburst);
    endproperty:arburst_keep_stable

    assert_arburst_keep_stable: 
        assert property (arburst_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  arburst_stable shit ");
    
    property arlock_keep_stable;
        @(posedge clk)
            axi_arvalid && !axi_arready && axi_resetn 
            |=> axi_rvalid && $stable(axi_arlock);
    endproperty:arlock_keep_stable

    assert_arlock_keep_stable: 
        assert property (arlock_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  arlock_stable shit ");
    
    property arcache_keep_stable;
        @(posedge clk)
            axi_arvalid && !axi_arready && axi_resetn 
            |=> axi_rvalid && $stable(axi_arcache);
    endproperty:arcache_keep_stable

    assert_arcache_keep_stable: 
        assert property (arcache_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  arcache_stable shit ");
    
    property arid_keep_stable;
        @(posedge clk)
            axi_arvalid && !axi_arready && axi_resetn 
            |=> axi_rvalid && $stable(axi_arid);
    endproperty:arid_keep_stable

    assert_arid_keep_stable: 
        assert property (arid_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  arid_stable shit ");
    
    property aruser_keep_stable;
        @(posedge clk)
            axi_arvalid && !axi_arready && axi_resetn 
            |=> axi_rvalid && $stable(axi_aruser);
    endproperty:aruser_keep_stable

    assert_aruser_keep_stable: 
        assert property (aruser_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  aruser_stable shit ");
    
    property arqos_keep_stable;
        @(posedge clk)
            axi_arvalid && !axi_arready && axi_resetn 
            |=> axi_rvalid && $stable(axi_arqos);
    endproperty:arqos_keep_stable

    assert_arqos_keep_stable: 
        assert property (arqos_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  arqos_stable shit ");
    
    property rdata_keep_stable;
        @(posedge clk)
            axi_rvalid && !axi_rready && axi_resetn 
            |=> axi_rvalid && $stable(axi_rdata);
    endproperty:rdata_keep_stable

    assert_rdata_keep_stable: 
        assert property (rdata_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  rdata_stable shit ");
    
    property ruser_keep_stable;
        @(posedge clk)
            axi_rvalid && !axi_rready && axi_resetn 
            |=> axi_rvalid && $stable(axi_ruser);
    endproperty:ruser_keep_stable

    assert_ruser_keep_stable: 
        assert property (ruser_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  ruser_stable shit ");
    
    property rid_keep_stable;
        @(posedge clk)
            axi_rvalid && !axi_rready && axi_resetn 
            |=> axi_rvalid && $stable(axi_rid);
    endproperty:rid_keep_stable

    assert_rid_keep_stable: 
        assert property (rid_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  rid_stable shit ");
    
    property rresp_keep_stable;
        @(posedge clk)
            axi_rvalid && !axi_rready && axi_resetn 
            |=> axi_rvalid && $stable(axi_rresp);
    endproperty:rresp_keep_stable

    assert_rresp_keep_stable: 
        assert property (rresp_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  rresp_stable shit ");
    
    property wdata_keep_stable;
        @(posedge clk)
            axi_wvalid && !axi_wready && axi_resetn 
            |=> axi_rvalid && $stable(axi_wdata);
    endproperty:wdata_keep_stable

    assert_wdata_keep_stable: 
        assert property (wdata_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  wdata_stable shit ");
    
    property awaddr_keep_stable;
        @(posedge clk)
            axi_awvalid && !axi_awready && axi_resetn 
            |=> axi_rvalid && $stable(axi_awaddr);
    endproperty:awaddr_keep_stable

    assert_awaddr_keep_stable: 
        assert property (awaddr_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  awaddr_stable shit ");
    
    property awprot_keep_stable;
        @(posedge clk)
            axi_awvalid && !axi_awready && axi_resetn 
            |=> axi_rvalid && $stable(axi_awprot);
    endproperty:awprot_keep_stable

    assert_awprot_keep_stable: 
        assert property (awprot_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  awprot_stable shit ");
    
    property awsize_keep_stable;
        @(posedge clk)
            axi_awvalid && !axi_awready && axi_resetn 
            |=> axi_rvalid && $stable(axi_awsize);
    endproperty:awsize_keep_stable

    assert_awsize_keep_stable: 
        assert property (awsize_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  awsize_stable shit ");
    
    property awburst_keep_stable;
        @(posedge clk)
            axi_awvalid && !axi_awready && axi_resetn 
            |=> axi_rvalid && $stable(axi_awburst);
    endproperty:awburst_keep_stable

    assert_awburst_keep_stable: 
        assert property (awburst_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  awburst_stable shit ");
    
    property awlock_keep_stable;
        @(posedge clk)
            axi_awvalid && !axi_awready && axi_resetn 
            |=> axi_rvalid && $stable(axi_awlock);
    endproperty:awlock_keep_stable

    assert_awlock_keep_stable: 
        assert property (awlock_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  awlock_stable shit ");
    
    property awcache_keep_stable;
        @(posedge clk)
            axi_awvalid && !axi_awready && axi_resetn 
            |=> axi_rvalid && $stable(axi_awcache);
    endproperty:awcache_keep_stable

    assert_awcache_keep_stable: 
        assert property (awcache_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  awcache_stable shit ");
    
    property awid_keep_stable;
        @(posedge clk)
            axi_awvalid && !axi_awready && axi_resetn 
            |=> axi_rvalid && $stable(axi_awid);
    endproperty:awid_keep_stable

    assert_awid_keep_stable: 
        assert property (awid_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  awid_stable shit ");
    
    property awuser_keep_stable;
        @(posedge clk)
            axi_awvalid && !axi_awready && axi_resetn 
            |=> axi_rvalid && $stable(axi_awuser);
    endproperty:awuser_keep_stable

    assert_awuser_keep_stable: 
        assert property (awuser_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  awuser_stable shit ");
    
    property awqos_keep_stable;
        @(posedge clk)
            axi_awvalid && !axi_awready && axi_resetn 
            |=> axi_rvalid && $stable(axi_awqos);
    endproperty:awqos_keep_stable

    assert_awqos_keep_stable: 
        assert property (awqos_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  awqos_stable shit ");
    

    property buser_keep_stable;
        @(posedge clk)
            axi_bvalid && !axi_bready && axi_resetn 
            |=> axi_rvalid && $stable(axi_buser);
    endproperty:buser_keep_stable

    assert_buser_keep_stable: 
        assert property (buser_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  buser_stable shit ");
    
    property bid_keep_stable;
        @(posedge clk)
            axi_bvalid && !axi_bready && axi_resetn 
            |=> axi_rvalid && $stable(axi_bid);
    endproperty:bid_keep_stable

    assert_bid_keep_stable: 
        assert property (bid_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  bid_stable shit ");
    
    property bresp_keep_stable;
        @(posedge clk)
            axi_bvalid && !axi_bready && axi_resetn 
            |=> axi_rvalid && $stable(axi_bresp);
    endproperty:bresp_keep_stable

    assert_bresp_keep_stable: 
        assert property (bresp_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  bresp_stable shit ");
    
    property wstrb_keep_stable;
        @(posedge clk)
            axi_wvalid && !axi_wready && axi_resetn 
            |=> axi_rvalid && $stable(axi_wstrb);
    endproperty:wstrb_keep_stable

    assert_wstrb_keep_stable: 
        assert property (wstrb_keep_stable) //$display ("OK r_keep_stable is assert");
        else $warning ("you are  wstrb_stable shit ");
    

/*  
    // 7
    // axi_write dependency
    property a_write_dependency;
        @(posedge clk)
            !axi_awvalid || !axi_awready || !axi_wlast 
            |=> !axi_bvalid;
    endproperty
    assert_write_dependency: 
        assert property(a_write_dependency) //$display ("OK write_dependency is assert");
        else $warning ("you are  write_dependency shit ");

    // 8
    // axi_read dependency
    property a_read_channel_dependency;
        @(posedge clk) !axi_arvalid || !axi_arready 
            |=> !axi_rvalid;
    endproperty

    always_comb_8: 
        assert property(a_read_channel_dependency) //$display ("OK a_read_channel_dependency is assert");
        else $warning ("you are  write_dependency wready_reset shit ");
*/

    //9
    // dont  wvalid when reset
    property wvalid_reset;
        @(posedge clk)
            !axi_resetn |=> !axi_wvalid; 
    endproperty 

    always_comb_9:
        assert property (wvalid_reset) //$display ("OK wvalid_reset is assert"); 
        else $warning ("you are  writing wvalid_reset shit ");
    
    // 10
    // dont awvalid when reset
    property awvalid_reset;
        @(posedge clk)
            !axi_resetn |=> !axi_awvalid; 
    endproperty 

    always_comb_10:
        assert property (awvalid_reset) //$display ("OK awvalid_reset is assert"); 
        else $warning ("you are  writing awvalid_reset shit ");

    // 11
    // dont arvalid when reset
    property arvalid_reset;
        @(posedge clk)
            !axi_resetn |=> !axi_arvalid; 
    endproperty 

    assert_arvalid_reset:
        assert property (awvalid_reset) //$display ("OK arvalid_reset is assert"); 
        else $warning ("you are  writing arvalid_reset shit ");
    
    // 12
    // dont bvalid when reset
    property bvalid_reset;
        @(posedge clk)
            !axi_resetn |=> !axi_bvalid; 
    endproperty 

    always_comb_12:
        assert property (bvalid_reset) //$display ("OK bvalid_reset is assert"); 
        else $warning ("you are  writing bvalid_reset shit ");

    // 13
    // dont rvalid when reset
    property rvalid_reset;
        @(posedge clk)
            !axi_resetn |=> !axi_rvalid; 
    endproperty 

    always_comb_13:
        assert property (rvalid_reset) //$display ("OK rvalid_reset is assert"); 
        else $warning ("you are  writing rvalid_reset shit ");

    // 14
    // dont axi_arready when reset
    property a1_ready_reset;
        @(posedge clk)
            !axi_resetn |=> !axi_arready; 
    endproperty

    always_comb_14: 
        assert property (a1_ready_reset) //$display ("OK a1_ready_reset is assert"); 
        else $warning ("you are  writing a1_ready_reset shit ");

    // 15
    // dont axi_awready axi_when axi_reset
    property a2_ready_reset;
        @(posedge clk)
            !axi_resetn |=> !axi_awready; 
    endproperty 

    always_comb_114: 
        assert property (a2_ready_reset) //$display ("OK a2_ready_reset is assert"); 
        else $warning ("you are  writing a2_ready_reset shit ");

    //16
    // dont axi_bready axi_ready axi_when axi_reset
    property b1_ready_reset;
        @(posedge clk)
            !axi_resetn |=> !axi_bready; 
    endproperty 

    always_comb_15: 
        assert property (b1_ready_reset) //$display ("OK b1_ready_reset is assert"); 
        else $warning ("you are  writing b1_ready_reset shit ");

    //17
    // dont axi_rready axi_ready axi_when axi_reset
    property rready_reset;
        @(posedge clk)
            !axi_resetn |=> !axi_rready; 
    endproperty 

    always_comb_16: 
        assert property (rready_reset) //$display ("OK rready_reset is assert"); 
        else $warning ("you are  writing rready_reset shit ");
    //18
    // dont axi_wready axi_ready axi_when axi_reset
    property wready_reset;
        @(posedge clk)
            !axi_resetn |=> !axi_wready; 
     endproperty 

    always_comb_17:  
        assert property (wready_reset) //$display ("OK wready_reset is assert"); 
        else $warning ("you are  writing wready_reset shit ");

    // reserved_burst
    property reserved_wburst;
         @(posedge clk)
             axi_awvalid |-> (axi_awburst  != 2'b11);
    endproperty

    always_comb_23: 
        assert property(reserved_wburst); //$display ("OK reserved_wburst  is assert");


    // reserved_arburst
    property reserved_arburst;
         @(posedge clk)
             axi_arvalid |-> (axi_arburst  != 2'b11);
    endproperty

    always_comb_24: 
        assert property(reserved_arburst); //$display ("OK reserved_arburst  is assert");

    /* 
    property AXI4_ERRM_AWVALID_STABLE;
        @(posedge aclk) disable iff (!resetn) 
        $rose(awvalid) |-> 
            strong(awvalid[*1:$] intersect awready[->1); // need the goto
    endproperty 
    always_comb_25: 
        assert property(AXI4_ERRM_AWVALID_STABLE) //$display ("OK AXI4_ERRM_AWVALID_STABLE is assert");

    property AXI4_ERRM_AWVALID_STABLE2;
        @(posedge aclk) disable iff (!resetn) 
            $rose(awvalid) |-> awvalid s_until_with awready;
    endproperty 
    always_comb_26: 
        assert property(AXI4_ERRM_AWVALID_STABLE2) $display ("OK AXI4_ERRM_AWVALID_STABLE2 is assert");
*/


endmodule
