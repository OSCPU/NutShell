`include "axi.vh"

module system_top (
  input [0:0]CLK_IN_D_clk_n,
  input [0:0]CLK_IN_D_clk_p,
  input [0:0]pcie_mgt_rxn,
  input [0:0]pcie_mgt_rxp,
  output [0:0]pcie_mgt_txn,
  output [0:0]pcie_mgt_txp,
  
  input  [ 31 : 0 ] cnt_data,
  output [ 31 : 0 ] cnt_addr,
  output            cnt_req,
  input             cnt_ack
  //output [7:0] led
);

  `axi_wire(AXI_MEM_MAPPED, 64, 8);
  `axi_wire(AXI_MEM, 64, 8);
  `axi_wire(AXI_MMIO, 64, 8);
  `axi_wire(AXI_DMA, 64, 16);
  // Added AXI interface for modified NutShell_peripheral block design
  `axi_wire(NutShell_io_frontend, 64, 1);
  `axi_wire(NutShell_io_mem, 64, 1);
  `axi_wire(NutShell_io_mmio, 64, 1);
  `axilite_wire(AXI_CNT);
  `axi_wire(AXI_interal, 32, 16);
  
  // Added signal wires for modified NutShell_peripheral block design
  wire NutShell_reset;
  wire [38:0] NutShell_io_ila_WBUpc;
  wire NutShell_io_ila_WBUvalid;
  wire NutShell_io_ila_WBUrfWen;
  wire [4:0] NutShell_io_ila_WBUrfDest;
  wire [63:0] NutShell_io_ila_WBUrfData;
  wire [63:0] NutShell_io_ila_InstrCnt;

  wire coreclk;
  wire corerstn;
  wire uncoreclk;
  wire uncorerstn;

  wire [4:0] intrs;

  wire nutshell_uart_tx;
  wire nutshell_uart_rx;

  zynq_soc zynq_soc_i (
    `axi_connect_if(AXI_MEM, AXI_MEM_MAPPED),
    `axi_connect_if(AXI_MMIO, AXI_MMIO),
    `axi_connect_if(AXI_DMA, AXI_DMA),
    `axilite_connect_if(AXI_CNT, AXI_CNT),

    .CLK_IN_D_clk_n(CLK_IN_D_clk_n),
    .CLK_IN_D_clk_p(CLK_IN_D_clk_p),
    .pcie_mgt_rxn(pcie_mgt_rxn),
    .pcie_mgt_rxp(pcie_mgt_rxp),
    .pcie_mgt_txn(pcie_mgt_txn),
    .pcie_mgt_txp(pcie_mgt_txp),

    .intrs(intrs),

    .coreclk(coreclk),
    .corerstn(corerstn),
    .uncoreclk(uncoreclk),
    .uncorerstn(uncorerstn)
  );

  addr_mapper addr_mapper_i(
    `axi_connect_if(s_axi, AXI_MEM),
    `axi_connect_if(m_axi, AXI_MEM_MAPPED)
  );

  reg corerstn_ff;
  always@(posedge uncoreclk) begin
    corerstn_ff <= corerstn;
  end

  reg corerstn_sync[1:0];
  always@(posedge coreclk) begin
    corerstn_sync[0] <= corerstn_ff;
    corerstn_sync[1] <= corerstn_sync[0];
  end

  NutShell nutshell_i (
    `axi_connect_if_no_id(io_frontend, NutShell_io_frontend),
    `axi_connect_if_no_id(io_mem, NutShell_io_mem),
    `axi_connect_if_no_id(io_mmio, NutShell_io_mmio),

    .clock(coreclk),
    .reset(NutShell_reset),

    .io_meip(intrs),

    .io_ila_WBUpc(NutShell_io_ila_WBUpc),
    .io_ila_WBUvalid(NutShell_io_ila_WBUvalid),
    .io_ila_WBUrfWen(NutShell_io_ila_WBUrfWen),
    .io_ila_WBUrfDest(NutShell_io_ila_WBUrfDest),
    .io_ila_WBUrfData(NutShell_io_ila_WBUrfData),
    .io_ila_InstrCnt(NutShell_io_ila_InstrCnt)
  );

  // Added instantiation for nutshell_peripheral block design
  nutshell_peripheral nutshell_peripheral_i (
    `axi_connect_if(AXI_MEM, AXI_MEM),
    `axi_connect_if(AXI_DMA, AXI_DMA),
    `axi_connect_if_no_id(AXI_MMIO, AXI_MMIO),
    `axi_connect_if_no_id(NutShell_io_frontend, NutShell_io_frontend),
    `axi_connect_if_no_id(NutShell_io_mem, NutShell_io_mem),
    `axi_connect_if_no_id(NutShell_io_mmio, NutShell_io_mmio),

    .coreclk(coreclk),
    .corerstn(corerstn_sync[1]),
    .uncoreclk(uncoreclk),
    .uncorerstn(uncorerstn),

    .NutShell_reset(NutShell_reset),
    .NutShell_io_ila_WBUpc(NutShell_io_ila_WBUpc),
    .NutShell_io_ila_WBUvalid(NutShell_io_ila_WBUvalid),
    .NutShell_io_ila_WBUrfWen(NutShell_io_ila_WBUrfWen),
    .NutShell_io_ila_WBUrfDest(NutShell_io_ila_WBUrfDest),
    .NutShell_io_ila_WBUrfData(NutShell_io_ila_WBUrfData),
    .NutShell_io_ila_InstrCnt(NutShell_io_ila_InstrCnt)
  );
  
  `define connect_external(name)\
        .io_extra``_``name(cnt``_``name)
        
  AXI4CNT axi4cnt_i(
    `axilite_connect_if(io_in, AXI_CNT),
    .connect_external(data),
    .connect_external(ack),
    .connect_external(req),
    .connect_external(addr)
  );
  
endmodule