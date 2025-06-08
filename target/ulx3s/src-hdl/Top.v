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

   // A reset line that goes low after 16 ticks
   reg [2:0]                  reset_cnt = 0;
   wire                       reset = ~reset_cnt[2];
   always @(posedge clk_25mhz)
     if (reset) reset_cnt <= reset_cnt + 1;

  ulxTopEntity u_topEntity
    (.CLK_VGA(CLK_25MHZ),
     .RST_VGA(reset),
     .CLK_TMDS(CLK_250MHZ),
     .RST_TMDS(reset),
     .BTNS(btn),
     .HDMI_DP(gpdi_dp),
     .HDMI_DN(gpdi_dn)
     );

endmodule
