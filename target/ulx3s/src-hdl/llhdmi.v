// Based on https://www.fpga4fun.com/HDMI.html

`default_nettype none

module llhdmi(
  input wire CLK_TMDS,
  input wire CLK_PIX,
  input wire RESET,
  input wire [7:0] VGA_RED, VGA_GRN, VGA_BLU,
  input wire VGA_DE, VGA_HSYNC, VGA_VSYNC,
  output wire TMDS_STREAM_RED, TMDS_STREAM_GRN, TMDS_STREAM_BLU
);

  // Convert the 8-bit colours into 10-bit TMDS values
  wire [9:0] TMDS_RED, TMDS_GRN, TMDS_BLU;
  TMDS_encoder encode_R(.clk(CLK_PIX), .VDE(VGA_DE), .TMDS(TMDS_RED),
                        .VD(VGA_RED), .CD(2'b00));
  TMDS_encoder encode_G(.clk(CLK_PIX), .VDE(VGA_DE), .TMDS(TMDS_GRN),
                        .VD(VGA_GRN), .CD(2'b00));
  TMDS_encoder encode_B(.clk(CLK_PIX), .VDE(VGA_DE), .TMDS(TMDS_BLU),
                        .VD(VGA_BLU), .CD({VGA_VSYNC, VGA_HSYNC}));

  // Strobe the TMDS_shift_load once every 10 CLK_TMDS
  // i.e. at the start of new pixel data
  reg [3:0] cnt10=0;
  reg SHIFT_LOAD=0;
  always @(posedge CLK_TMDS) begin
    if (RESET) begin
      cnt10 <= 0;
      SHIFT_LOAD <= 0;
    end else begin
      cnt10 <= (cnt10==4'd9) ? 4'd0 : cnt10+4'd1;
      SHIFT_LOAD <= (cnt10==4'd9);
    end
  end

  // Latch the TMDS colour values into three shift registers
  // at the start of the pixel, then shift them one bit each CLK_TMDS.
  // We will then output the LSB on each CLK_TMDS.
  reg [9:0] SHIFT_RED = 0, SHIFT_GRN = 0, SHIFT_BLU = 0;
  always @(posedge CLK_TMDS) begin
    if (RESET) begin
      SHIFT_RED <= 0;
      SHIFT_GRN <= 0;
      SHIFT_BLU <= 0;
    end else begin
      SHIFT_RED <= SHIFT_LOAD ? TMDS_RED: {1'b0, SHIFT_RED[9:1]};
      SHIFT_GRN <= SHIFT_LOAD ? TMDS_GRN: {1'b0, SHIFT_GRN[9:1]};
      SHIFT_BLU <= SHIFT_LOAD ? TMDS_BLU: {1'b0, SHIFT_BLU[9:1]};
    end
  end

  // Finally output the LSB of each color bitstream
  assign TMDS_STREAM_RED = SHIFT_RED[0];
  assign TMDS_STREAM_GRN = SHIFT_GRN[0];
  assign TMDS_STREAM_BLU = SHIFT_BLU[0];

endmodule
