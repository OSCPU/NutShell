create_clock -period 10.000 -name pcie_x86_refclk -waveform {0.000 5.000} [get_ports CLK_IN_D_clk_p]
set_property PACKAGE_PIN AB35 [get_ports {CLK_IN_D_clk_n[0]}]
set_property PACKAGE_PIN AB34 [get_ports {CLK_IN_D_clk_p[0]}]

set_property PACKAGE_PIN W41 [get_ports {pcie_mgt_rxp[0]}]
set_property PACKAGE_PIN W42 [get_ports {pcie_mgt_rxn[0]}]
set_property PACKAGE_PIN Y34 [get_ports {pcie_mgt_txp[0]}]
set_property PACKAGE_PIN Y35 [get_ports {pcie_mgt_txn[0]}]
