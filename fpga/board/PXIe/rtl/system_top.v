`include "axi.vh"

module system_top (
  input [0:0]CLK_IN_D_clk_n,
  input [0:0]CLK_IN_D_clk_p,
  input [0:0]pcie_mgt_rxn,
  input [0:0]pcie_mgt_rxp,
  output [0:0]pcie_mgt_txn,
  output [0:0]pcie_mgt_txp
  //output [7:0] led
);

  `axi_wire(AXI_MEM_MAPPED, 64, 8);
  `axi_wire(AXI_MEM, 64, 8);
  `axi_wire(AXI_MMIO, 64, 8);
  `axi_wire(AXI_DMA, 64, 16);

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

  nutshell nutshell_i(
    `axi_connect_if(AXI_MEM, AXI_MEM),
    `axi_connect_if(AXI_DMA, AXI_DMA),
    `axi_connect_if_no_id(AXI_MMIO, AXI_MMIO),

    .intrs(intrs),

    .coreclk(coreclk),
    .corerstn(corerstn_sync[1]),
    .uncoreclk(uncoreclk),
    .uncorerstn(uncorerstn)
  );

endmodule
