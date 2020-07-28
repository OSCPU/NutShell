`ifdef RV32
`define RAMWIDTH 32
import "DPI-C" function void ram_helper
(
  input  int    rIdx,
  output int    rdata,
  input  int    wIdx,
  input  int    wdata,
  input  int    wmask,
  input  bit    wen
);
`else
`define RAMWIDTH 64
import "DPI-C" function void ram_helper
(
  input  longint    rIdx,
  output longint    rdata,
  input  longint    wIdx,
  input  longint    wdata,
  input  longint    wmask,
  input  bit    wen
);
`endif


module RAMHelper(
  input         clk,
  input  [`RAMWIDTH-1:0] rIdx,
  output [`RAMWIDTH-1:0] rdata,
  input  [`RAMWIDTH-1:0] wIdx,
  input  [`RAMWIDTH-1:0] wdata,
  input  [`RAMWIDTH-1:0] wmask,
  input         wen
);

  always @(posedge clk) begin
    ram_helper(rIdx, rdata, wIdx, wdata, wmask, wen);
  end

endmodule

