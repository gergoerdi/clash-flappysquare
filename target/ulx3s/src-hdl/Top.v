`default_nettype none

module Top (input wire        clk_25mhz,
            input wire [6:0]  btn,
            output wire [3:0] gpdi_dp, gpdi_dn,
	    output wire       wifi_gpio0);
   
   // Tie gpio0 high, this keeps the board from rebooting
   assign wifi_gpio0 = 1'b1;
   
   wire CLK_25MHZ, CLK_250MHZ;
   clock clock_instance(
       .clkin_25MHz(clk_25mhz),
       .clk_25MHz(CLK_25MHZ),
       .clk_250MHz(CLK_250MHZ)
       );

   wire o_red;
   wire o_grn;
   wire o_blu;

   OBUFDS OBUFDS_red(.I(o_red), .O(gpdi_dp[2]), .OB(gpdi_dn[2]));
   OBUFDS OBUFDS_grn(.I(o_grn), .O(gpdi_dp[1]), .OB(gpdi_dn[1]));
   OBUFDS OBUFDS_blu(.I(o_blu), .O(gpdi_dp[0]), .OB(gpdi_dn[0]));
   OBUFDS OBUFDS_clock(.I(CLK_25MHZ), .O(gpdi_dp[3]), .OB(gpdi_dn[3]));
   
   // A reset line that goes low after 16 ticks
   reg [2:0]                  reset_cnt = 0;
   wire                       reset = ~reset_cnt[2];
   always @(posedge clk_25mhz)
     if (reset) reset_cnt <= reset_cnt + 1;
   
   wire VGA_DE, VGA_HSYNC, VGA_VSYNC;
   wire [7:0] VGA_RED, VGA_GRN, VGA_BLU;

   llhdmi llhdmi_instance
     (.CLK_TMDS(CLK_250MHZ), .CLK_PIX(CLK_25MHZ),
      .RESET(reset), 
      .VGA_DE(VGA_DE), .VGA_HSYNC(VGA_HSYNC), .VGA_VSYNC(VGA_VSYNC),
      .VGA_RED(VGA_RED), .VGA_GRN(VGA_GRN), .VGA_BLU(VGA_BLU),
      .TMDS_STREAM_RED(o_red), .TMDS_STREAM_GRN(o_grn), .TMDS_STREAM_BLU(o_blu));
   
  topEntity u_topEntity
    (.CLK_25MHZ(CLK_25MHZ),
     .RESET(reset),
     .BTN(btn[3]),
     .VGA_HSYNC(VGA_HSYNC),
     .VGA_VSYNC(VGA_VSYNC),
     .VGA_DE(VGA_DE),
     .VGA_RED(VGA_RED),
     .VGA_GREEN(VGA_GRN),
     .VGA_BLUE(VGA_BLU)
     );

endmodule
