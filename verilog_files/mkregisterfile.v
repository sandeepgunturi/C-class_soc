//
// Generated by Bluespec Compiler (build 8d454e4)
//
// On Fri Oct  9 13:12:32 IST 2020
//
//
// Ports:
// Name                         I/O  size props
// read_rs1                       O    64
// RDY_read_rs1                   O     1
// read_rs2                       O    64
// RDY_read_rs2                   O     1
// read_rs3                       O    64
// RDY_read_rs3                   O     1
// RDY_commit_rd                  O     1
// CLK                            I     1 clock
// RST_N                          I     1 reset
// read_rs1_addr                  I     5
// read_rs1_rs1type               I     1
// read_rs2_addr                  I     5
// read_rs2_rs2type               I     1
// read_rs3_addr                  I     5
// commit_rd_c                    I    70
// EN_commit_rd                   I     1
// EN_read_rs1                    I     1 unused
// EN_read_rs2                    I     1 unused
// EN_read_rs3                    I     1 unused
//
// Combinational paths from inputs to outputs:
//   read_rs1_rs1type -> read_rs1
//   read_rs2_rs2type -> read_rs2
//
//

`ifdef BSV_ASSIGNMENT_DELAY
`else
  `define BSV_ASSIGNMENT_DELAY
`endif

`ifdef BSV_POSITIVE_RESET
  `define BSV_RESET_VALUE 1'b1
  `define BSV_RESET_EDGE posedge
`else
  `define BSV_RESET_VALUE 1'b0
  `define BSV_RESET_EDGE negedge
`endif

module mkregisterfile(CLK,
		      RST_N,

		      read_rs1_addr,
		      read_rs1_rs1type,
		      EN_read_rs1,
		      read_rs1,
		      RDY_read_rs1,

		      read_rs2_addr,
		      read_rs2_rs2type,
		      EN_read_rs2,
		      read_rs2,
		      RDY_read_rs2,

		      read_rs3_addr,
		      EN_read_rs3,
		      read_rs3,
		      RDY_read_rs3,

		      commit_rd_c,
		      EN_commit_rd,
		      RDY_commit_rd);
  parameter [63 : 0] hartid = 64'b0;
  input  CLK;
  input  RST_N;

  // actionvalue method read_rs1
  input  [4 : 0] read_rs1_addr;
  input  read_rs1_rs1type;
  input  EN_read_rs1;
  output [63 : 0] read_rs1;
  output RDY_read_rs1;

  // actionvalue method read_rs2
  input  [4 : 0] read_rs2_addr;
  input  read_rs2_rs2type;
  input  EN_read_rs2;
  output [63 : 0] read_rs2;
  output RDY_read_rs2;

  // actionvalue method read_rs3
  input  [4 : 0] read_rs3_addr;
  input  EN_read_rs3;
  output [63 : 0] read_rs3;
  output RDY_read_rs3;

  // action method commit_rd
  input  [69 : 0] commit_rd_c;
  input  EN_commit_rd;
  output RDY_commit_rd;

  // signals for module outputs
  wire [63 : 0] read_rs1, read_rs2, read_rs3;
  wire RDY_commit_rd, RDY_read_rs1, RDY_read_rs2, RDY_read_rs3;

  // register initialize
  reg initialize;
  wire initialize_D_IN, initialize_EN;

  // register rg_index
  reg [4 : 0] rg_index;
  wire [4 : 0] rg_index_D_IN;
  wire rg_index_EN;

  // ports of submodule floating_rf
  wire [63 : 0] floating_rf_D_IN,
		floating_rf_D_OUT_1,
		floating_rf_D_OUT_2,
		floating_rf_D_OUT_3;
  wire [4 : 0] floating_rf_ADDR_1,
	       floating_rf_ADDR_2,
	       floating_rf_ADDR_3,
	       floating_rf_ADDR_4,
	       floating_rf_ADDR_5,
	       floating_rf_ADDR_IN;
  wire floating_rf_WE;

  // ports of submodule integer_rf
  wire [63 : 0] integer_rf_D_IN, integer_rf_D_OUT_1, integer_rf_D_OUT_2;
  wire [4 : 0] integer_rf_ADDR_1,
	       integer_rf_ADDR_2,
	       integer_rf_ADDR_3,
	       integer_rf_ADDR_4,
	       integer_rf_ADDR_5,
	       integer_rf_ADDR_IN;
  wire integer_rf_WE;

  // rule scheduling signals
  wire CAN_FIRE_RL_initialize_regfile,
       CAN_FIRE_commit_rd,
       CAN_FIRE_read_rs1,
       CAN_FIRE_read_rs2,
       CAN_FIRE_read_rs3,
       WILL_FIRE_RL_initialize_regfile,
       WILL_FIRE_commit_rd,
       WILL_FIRE_read_rs1,
       WILL_FIRE_read_rs2,
       WILL_FIRE_read_rs3;

  // inputs to muxes for submodule ports
  wire MUX_floating_rf_upd_1__SEL_1, MUX_integer_rf_upd_1__SEL_1;

  // declarations used by system tasks
  // synopsys translate_off
  reg TASK_testplusargs___d16;
  reg TASK_testplusargs___d17;
  reg TASK_testplusargs___d18;
  reg [63 : 0] v__h890;
  reg TASK_testplusargs___d5;
  reg TASK_testplusargs___d6;
  reg TASK_testplusargs___d7;
  reg [63 : 0] v__h619;
  reg TASK_testplusargs_6_OR_TASK_testplusargs_7_AND_ETC___d25;
  reg TASK_testplusargs_6_OR_TASK_testplusargs_7_AND_ETC___d27;
  // synopsys translate_on

  // actionvalue method read_rs1
  assign read_rs1 =
	     read_rs1_rs1type ? floating_rf_D_OUT_3 : integer_rf_D_OUT_2 ;
  assign RDY_read_rs1 = !initialize ;
  assign CAN_FIRE_read_rs1 = !initialize ;
  assign WILL_FIRE_read_rs1 = EN_read_rs1 ;

  // actionvalue method read_rs2
  assign read_rs2 =
	     read_rs2_rs2type ? floating_rf_D_OUT_2 : integer_rf_D_OUT_1 ;
  assign RDY_read_rs2 = !initialize ;
  assign CAN_FIRE_read_rs2 = !initialize ;
  assign WILL_FIRE_read_rs2 = EN_read_rs2 ;

  // actionvalue method read_rs3
  assign read_rs3 = floating_rf_D_OUT_1 ;
  assign RDY_read_rs3 = !initialize ;
  assign CAN_FIRE_read_rs3 = !initialize ;
  assign WILL_FIRE_read_rs3 = EN_read_rs3 ;

  // action method commit_rd
  assign RDY_commit_rd = !initialize ;
  assign CAN_FIRE_commit_rd = !initialize ;
  assign WILL_FIRE_commit_rd = EN_commit_rd ;

  // submodule floating_rf
  RegFile #(.addr_width(32'd5),
	    .data_width(32'd64),
	    .lo(5'd0),
	    .hi(5'd31)) floating_rf(.CLK(CLK),
				    .ADDR_1(floating_rf_ADDR_1),
				    .ADDR_2(floating_rf_ADDR_2),
				    .ADDR_3(floating_rf_ADDR_3),
				    .ADDR_4(floating_rf_ADDR_4),
				    .ADDR_5(floating_rf_ADDR_5),
				    .ADDR_IN(floating_rf_ADDR_IN),
				    .D_IN(floating_rf_D_IN),
				    .WE(floating_rf_WE),
				    .D_OUT_1(floating_rf_D_OUT_1),
				    .D_OUT_2(floating_rf_D_OUT_2),
				    .D_OUT_3(floating_rf_D_OUT_3),
				    .D_OUT_4(),
				    .D_OUT_5());

  // submodule integer_rf
  RegFile #(.addr_width(32'd5),
	    .data_width(32'd64),
	    .lo(5'd0),
	    .hi(5'd31)) integer_rf(.CLK(CLK),
				   .ADDR_1(integer_rf_ADDR_1),
				   .ADDR_2(integer_rf_ADDR_2),
				   .ADDR_3(integer_rf_ADDR_3),
				   .ADDR_4(integer_rf_ADDR_4),
				   .ADDR_5(integer_rf_ADDR_5),
				   .ADDR_IN(integer_rf_ADDR_IN),
				   .D_IN(integer_rf_D_IN),
				   .WE(integer_rf_WE),
				   .D_OUT_1(integer_rf_D_OUT_1),
				   .D_OUT_2(integer_rf_D_OUT_2),
				   .D_OUT_3(),
				   .D_OUT_4(),
				   .D_OUT_5());

  // rule RL_initialize_regfile
  assign CAN_FIRE_RL_initialize_regfile = initialize ;
  assign WILL_FIRE_RL_initialize_regfile = initialize ;

  // inputs to muxes for submodule ports
  assign MUX_floating_rf_upd_1__SEL_1 = EN_commit_rd && commit_rd_c[0] ;
  assign MUX_integer_rf_upd_1__SEL_1 =
	     EN_commit_rd && !commit_rd_c[0] && commit_rd_c[69:65] != 5'd0 ;

  // register initialize
  assign initialize_D_IN = 1'd0 ;
  assign initialize_EN = initialize && rg_index == 5'd31 ;

  // register rg_index
  assign rg_index_D_IN = rg_index + 5'd1 ;
  assign rg_index_EN = initialize ;

  // submodule floating_rf
  assign floating_rf_ADDR_1 = read_rs3_addr ;
  assign floating_rf_ADDR_2 = read_rs2_addr ;
  assign floating_rf_ADDR_3 = read_rs1_addr ;
  assign floating_rf_ADDR_4 = 5'h0 ;
  assign floating_rf_ADDR_5 = 5'h0 ;
  assign floating_rf_ADDR_IN =
	     MUX_floating_rf_upd_1__SEL_1 ? commit_rd_c[69:65] : rg_index ;
  assign floating_rf_D_IN =
	     MUX_floating_rf_upd_1__SEL_1 ? commit_rd_c[64:1] : 64'd0 ;
  assign floating_rf_WE = EN_commit_rd && commit_rd_c[0] || initialize ;

  // submodule integer_rf
  assign integer_rf_ADDR_1 = read_rs2_addr ;
  assign integer_rf_ADDR_2 = read_rs1_addr ;
  assign integer_rf_ADDR_3 = 5'h0 ;
  assign integer_rf_ADDR_4 = 5'h0 ;
  assign integer_rf_ADDR_5 = 5'h0 ;
  assign integer_rf_ADDR_IN =
	     MUX_integer_rf_upd_1__SEL_1 ? commit_rd_c[69:65] : rg_index ;
  assign integer_rf_D_IN =
	     MUX_integer_rf_upd_1__SEL_1 ? commit_rd_c[64:1] : 64'd0 ;
  assign integer_rf_WE =
	     EN_commit_rd && !commit_rd_c[0] && commit_rd_c[69:65] != 5'd0 ||
	     initialize ;

  // handling of inlined registers

  always@(posedge CLK)
  begin
    if (RST_N == `BSV_RESET_VALUE)
      begin
        initialize <= `BSV_ASSIGNMENT_DELAY 1'd1;
	rg_index <= `BSV_ASSIGNMENT_DELAY 5'd0;
      end
    else
      begin
        if (initialize_EN)
	  initialize <= `BSV_ASSIGNMENT_DELAY initialize_D_IN;
	if (rg_index_EN) rg_index <= `BSV_ASSIGNMENT_DELAY rg_index_D_IN;
      end
  end

  // synopsys translate_off
  `ifdef BSV_NO_INITIAL_BLOCKS
  `else // not BSV_NO_INITIAL_BLOCKS
  initial
  begin
    initialize = 1'h0;
    rg_index = 5'h0A;
  end
  `endif // BSV_NO_INITIAL_BLOCKS
  // synopsys translate_on

  // handling of system tasks

  // synopsys translate_off
  always@(negedge CLK)
  begin
    #0;
    if (RST_N != `BSV_RESET_VALUE)
      if (EN_commit_rd)
	begin
	  TASK_testplusargs___d16 = $test$plusargs("fullverbose");
	  #0;
	end
    if (RST_N != `BSV_RESET_VALUE)
      if (EN_commit_rd)
	begin
	  TASK_testplusargs___d17 = $test$plusargs("mrf");
	  #0;
	end
    if (RST_N != `BSV_RESET_VALUE)
      if (EN_commit_rd)
	begin
	  TASK_testplusargs___d18 = $test$plusargs("l1");
	  #0;
	end
    TASK_testplusargs_6_OR_TASK_testplusargs_7_AND_ETC___d25 =
	(TASK_testplusargs___d16 ||
	 TASK_testplusargs___d17 && TASK_testplusargs___d18) &&
	commit_rd_c[0];
    TASK_testplusargs_6_OR_TASK_testplusargs_7_AND_ETC___d27 =
	(TASK_testplusargs___d16 ||
	 TASK_testplusargs___d17 && TASK_testplusargs___d18) &&
	!commit_rd_c[0];
    if (RST_N != `BSV_RESET_VALUE)
      if (EN_commit_rd)
	begin
	  v__h890 = $time;
	  #0;
	end
    if (RST_N != `BSV_RESET_VALUE)
      if (EN_commit_rd &&
	  (TASK_testplusargs___d16 ||
	   TASK_testplusargs___d17 && TASK_testplusargs___d18))
	$write("[%10d", v__h890, "] ");
    if (RST_N != `BSV_RESET_VALUE)
      if (EN_commit_rd &&
	  (TASK_testplusargs___d16 ||
	   TASK_testplusargs___d17 && TASK_testplusargs___d18))
	$write("[%2d]RF : Writing Rd: %d(%h) ",
	       hartid,
	       commit_rd_c[69:65],
	       commit_rd_c[64:1]);
    if (RST_N != `BSV_RESET_VALUE)
      if (EN_commit_rd &&
	  TASK_testplusargs_6_OR_TASK_testplusargs_7_AND_ETC___d25)
	$write("FRF");
    if (RST_N != `BSV_RESET_VALUE)
      if (EN_commit_rd &&
	  TASK_testplusargs_6_OR_TASK_testplusargs_7_AND_ETC___d27)
	$write("IRF");
    if (RST_N != `BSV_RESET_VALUE)
      if (EN_commit_rd &&
	  (TASK_testplusargs___d16 ||
	   TASK_testplusargs___d17 && TASK_testplusargs___d18))
	$write("\n");
    if (RST_N != `BSV_RESET_VALUE)
      if (initialize)
	begin
	  TASK_testplusargs___d5 = $test$plusargs("fullverbose");
	  #0;
	end
    if (RST_N != `BSV_RESET_VALUE)
      if (initialize)
	begin
	  TASK_testplusargs___d6 = $test$plusargs("mrf");
	  #0;
	end
    if (RST_N != `BSV_RESET_VALUE)
      if (initialize)
	begin
	  TASK_testplusargs___d7 = $test$plusargs("l1");
	  #0;
	end
    if (RST_N != `BSV_RESET_VALUE)
      if (initialize)
	begin
	  v__h619 = $time;
	  #0;
	end
    if (RST_N != `BSV_RESET_VALUE)
      if (initialize &&
	  (TASK_testplusargs___d5 ||
	   TASK_testplusargs___d6 && TASK_testplusargs___d7))
	$write("[%10d", v__h619, "] ");
    if (RST_N != `BSV_RESET_VALUE)
      if (initialize &&
	  (TASK_testplusargs___d5 ||
	   TASK_testplusargs___d6 && TASK_testplusargs___d7))
	$write("[%2d]RF : Initialization phase. Count: %d", hartid, rg_index);
    if (RST_N != `BSV_RESET_VALUE)
      if (initialize &&
	  (TASK_testplusargs___d5 ||
	   TASK_testplusargs___d6 && TASK_testplusargs___d7))
	$write("\n");
  end
  // synopsys translate_on
endmodule  // mkregisterfile

