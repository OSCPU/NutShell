`include "axi.vh"

`define HAS_HDMI

module system_top (
`ifdef HAS_HDMI
  inout  hdmi_scl,
  inout  hdmi_sda,
  output hdmi_nreset,
  output hdmi_clk,
  output hdmi_hsync,
  output hdmi_vsync,
  output hdmi_videovalid,
  output [23:0] hdmi_rgb
`endif
  //output [7:0] led
);

  `axi_wire(AXI_MEM_MAPPED, 64, 8);
  `axi_wire(AXI_MEM, 64, 8);
  `axi_wire(AXI_MMIO, 64, 8);
  `axi_wire(AXI_DMA, 64, 16);

  wire coreclk;
  wire corerstn;
  wire clk40;
  wire clk27;
  wire uncoreclk;
  wire uncorerstn;

  wire [4:0] intrs;

  zynq_soc zynq_soc_i (
    `axi_connect_if(AXI_MEM, AXI_MEM_MAPPED),
    `axi_connect_if(AXI_MMIO, AXI_MMIO),
    `axi_connect_if(AXI_DMA, AXI_DMA),

    .intrs(intrs),

`ifdef HAS_HDMI
    .vga_rgb(hdmi_rgb),
    .vga_hsync(hdmi_hsync),
    .vga_vsync(hdmi_vsync),
    .vga_valid(hdmi_videovalid),
    .clk27(clk27),
    .clk40(clk40),
`endif

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

`ifdef HAS_HDMI
  i2c_config hdmi_i2c_config(
    .rst(!uncorerstn),
    .clk(clk27),
    .i2c_scl(hdmi_scl),
    .i2c_sda(hdmi_sda)
  );

  assign hdmi_nreset = uncorerstn;
  assign hdmi_clk = clk40;
`endif

endmodule
