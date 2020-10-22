//
// Generated by Bluespec Compiler (build 8d454e4)
//
// On Fri Oct  9 13:12:42 IST 2020
//
//
// Ports:
// Name                         I/O  size props
// mv_output                      O    70
// RDY_mv_output                  O     1 const
// CLK                            I     1 clock
// RST_N                          I     1 reset
// ma_inputs_in1                  I    64
// ma_inputs_in2                  I    64
// ma_inputs_funct3               I     3
// ma_inputs_word32               I     1
// EN_ma_inputs                   I     1
//
// No combinational paths from inputs to outputs
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

module mkrestoring_div(CLK,
		       RST_N,

		       ma_inputs_in1,
		       ma_inputs_in2,
		       ma_inputs_funct3,
		       ma_inputs_word32,
		       EN_ma_inputs,

		       mv_output,
		       RDY_mv_output);
  parameter [63 : 0] hartid = 64'b0;
  input  CLK;
  input  RST_N;

  // action method ma_inputs
  input  [63 : 0] ma_inputs_in1;
  input  [63 : 0] ma_inputs_in2;
  input  [2 : 0] ma_inputs_funct3;
  input  ma_inputs_word32;
  input  EN_ma_inputs;

  // value method mv_output
  output [69 : 0] mv_output;
  output RDY_mv_output;

  // signals for module outputs
  wire [69 : 0] mv_output;
  wire RDY_mv_output;

  // inlined wires
  wire rg_valid_1_whas;

  // register partial
  reg [128 : 0] partial;
  wire [128 : 0] partial_D_IN;
  wire partial_EN;

  // register quotient_remainder
  reg quotient_remainder;
  wire quotient_remainder_D_IN, quotient_remainder_EN;

  // register rg_complement
  reg rg_complement;
  wire rg_complement_D_IN, rg_complement_EN;

  // register rg_count
  reg [5 : 0] rg_count;
  wire [5 : 0] rg_count_D_IN;
  wire rg_count_EN;

  // register rg_in1
  reg [63 : 0] rg_in1;
  wire [63 : 0] rg_in1_D_IN;
  wire rg_in1_EN;

  // register rg_op2
  reg [63 : 0] rg_op2;
  wire [63 : 0] rg_op2_D_IN;
  wire rg_op2_EN;

  // register rg_result
  reg [63 : 0] rg_result;
  wire [63 : 0] rg_result_D_IN;
  wire rg_result_EN;

  // register rg_sign_op1
  reg rg_sign_op1;
  wire rg_sign_op1_D_IN, rg_sign_op1_EN;

  // register rg_upperbits
  reg rg_upperbits;
  wire rg_upperbits_D_IN, rg_upperbits_EN;

  // register rg_valid
  reg rg_valid;
  wire rg_valid_D_IN, rg_valid_EN;

  // register rg_word
  reg rg_word;
  wire rg_word_D_IN, rg_word_EN;

  // rule scheduling signals
  wire CAN_FIRE_RL_rg_valid__dreg_update,
       CAN_FIRE_RL_single_step_div,
       CAN_FIRE_ma_inputs,
       WILL_FIRE_RL_rg_valid__dreg_update,
       WILL_FIRE_RL_single_step_div,
       WILL_FIRE_ma_inputs;

  // inputs to muxes for submodule ports
  wire [128 : 0] MUX_partial_write_1__VAL_1, MUX_partial_write_1__VAL_2;
  wire [5 : 0] MUX_rg_count_write_1__VAL_1;

  // declarations used by system tasks
  // synopsys translate_off
  reg TASK_testplusargs___d57;
  reg TASK_testplusargs___d58;
  reg TASK_testplusargs___d59;
  reg [63 : 0] v__h1334;
  reg TASK_testplusargs___d12;
  reg TASK_testplusargs___d13;
  reg TASK_testplusargs___d14;
  reg [63 : 0] v__h695;
  reg TASK_testplusargs___d50;
  reg TASK_testplusargs___d51;
  reg TASK_testplusargs___d52;
  reg [63 : 0] v__h888;
  reg rg_op2_0_EQ_0_9_AND_TASK_testplusargs_0_OR_TAS_ETC___d55;
  // synopsys translate_on

  // remaining internal signals
  wire [128 : 0] x__h1199;
  wire [63 : 0] IF_ma_inputs_word32_THEN_IF_ma_inputs_funct3_B_ETC___d71,
		IF_ma_inputs_word32_THEN_IF_ma_inputs_funct3_B_ETC___d83,
		IF_quotient_remainder_8_THEN_rg_in1_5_ELSE_184_ETC___d26,
		IF_rg_upperbits_0_AND_rg_complement_1_2_AND_NO_ETC___d45,
		_theResult___fst__h1556,
		_theResult___snd__h1557,
		op1__h1424,
		product__h1049,
		product__h1123,
		reslt___1__h1209,
		reslt__h1121,
		t1__h1460,
		t2__h1461,
		x_BITS_63_TO_0___h1200;
  wire [31 : 0] IF_quotient_remainder_8_THEN_rg_in1_5_ELSE_184_ETC__q4,
		IF_rg_upperbits_0_AND_rg_complement_1_2_AND_NO_ETC__q3,
		ma_inputs_in1_BITS_31_TO_0__q1,
		ma_inputs_in2_BITS_31_TO_0__q2;
  wire [5 : 0] x__h817;
  wire IF_quotient_remainder_8_THEN_partial_BITS_127__ETC___d37,
       x__h1638,
       x__h1665;

  // action method ma_inputs
  assign CAN_FIRE_ma_inputs = 1'd1 ;
  assign WILL_FIRE_ma_inputs = EN_ma_inputs ;

  // value method mv_output
  assign mv_output = { rg_result, rg_valid, 5'd0 } ;
  assign RDY_mv_output = 1'd1 ;

  // rule RL_single_step_div
  assign CAN_FIRE_RL_single_step_div = rg_count != 6'd0 ;
  assign WILL_FIRE_RL_single_step_div = CAN_FIRE_RL_single_step_div ;

  // rule RL_rg_valid__dreg_update
  assign CAN_FIRE_RL_rg_valid__dreg_update = 1'd1 ;
  assign WILL_FIRE_RL_rg_valid__dreg_update = 1'd1 ;

  // inputs to muxes for submodule ports
  module_fn_single_div instance_fn_single_div_0(.fn_single_div_remainder(x__h1199[128:64]),
						.fn_single_div_quotient(x_BITS_63_TO_0___h1200),
						.fn_single_div_divisor(rg_op2),
						.fn_single_div(MUX_partial_write_1__VAL_1));
  assign MUX_partial_write_1__VAL_2 = { 65'd0, op1__h1424 } ;
  assign MUX_rg_count_write_1__VAL_1 =
	     (rg_op2 == 64'd0 || rg_count == 6'd33) ? 6'd0 : x__h817 ;

  // inlined wires
  assign rg_valid_1_whas =
	     WILL_FIRE_RL_single_step_div &&
	     (rg_op2 == 64'd0 || rg_count == 6'd33) ;

  // register partial
  assign partial_D_IN =
	     WILL_FIRE_RL_single_step_div ?
	       MUX_partial_write_1__VAL_1 :
	       MUX_partial_write_1__VAL_2 ;
  assign partial_EN = EN_ma_inputs || WILL_FIRE_RL_single_step_div ;

  // register quotient_remainder
  assign quotient_remainder_D_IN = ma_inputs_funct3[1] ;
  assign quotient_remainder_EN = EN_ma_inputs ;

  // register rg_complement
  assign rg_complement_D_IN =
	     (ma_inputs_funct3 == 3'd4) ?
	       IF_ma_inputs_word32_THEN_IF_ma_inputs_funct3_B_ETC___d71[63] ^
	       IF_ma_inputs_word32_THEN_IF_ma_inputs_funct3_B_ETC___d83[63] :
	       ma_inputs_funct3 == 3'd6 ;
  assign rg_complement_EN = EN_ma_inputs ;

  // register rg_count
  assign rg_count_D_IN =
	     WILL_FIRE_RL_single_step_div ?
	       MUX_rg_count_write_1__VAL_1 :
	       x__h817 ;
  assign rg_count_EN = EN_ma_inputs || WILL_FIRE_RL_single_step_div ;

  // register rg_in1
  assign rg_in1_D_IN = ma_inputs_in1 ;
  assign rg_in1_EN = EN_ma_inputs ;

  // register rg_op2
  assign rg_op2_D_IN =
	     (t2__h1461 ^
	      IF_ma_inputs_word32_THEN_IF_ma_inputs_funct3_B_ETC___d83) +
	     { 63'd0, x__h1665 } ;
  assign rg_op2_EN = EN_ma_inputs ;

  // register rg_result
  assign rg_result_D_IN =
	     (rg_op2 == 64'd0) ? product__h1049 : product__h1123 ;
  assign rg_result_EN = rg_valid_1_whas ;

  // register rg_sign_op1
  assign rg_sign_op1_D_IN =
	     IF_ma_inputs_word32_THEN_IF_ma_inputs_funct3_B_ETC___d71[63] ;
  assign rg_sign_op1_EN = EN_ma_inputs ;

  // register rg_upperbits
  assign rg_upperbits_D_IN = ma_inputs_funct3[1] ;
  assign rg_upperbits_EN = EN_ma_inputs ;

  // register rg_valid
  assign rg_valid_D_IN = rg_valid_1_whas ;
  assign rg_valid_EN = 1'd1 ;

  // register rg_word
  assign rg_word_D_IN = ma_inputs_word32 ;
  assign rg_word_EN = EN_ma_inputs ;

  // remaining internal signals
  assign IF_ma_inputs_word32_THEN_IF_ma_inputs_funct3_B_ETC___d71 =
	     ma_inputs_word32 ? _theResult___fst__h1556 : ma_inputs_in1 ;
  assign IF_ma_inputs_word32_THEN_IF_ma_inputs_funct3_B_ETC___d83 =
	     ma_inputs_word32 ? _theResult___snd__h1557 : ma_inputs_in2 ;
  assign IF_quotient_remainder_8_THEN_partial_BITS_127__ETC___d37 =
	     reslt__h1121[63] == rg_sign_op1 ;
  assign IF_quotient_remainder_8_THEN_rg_in1_5_ELSE_184_ETC___d26 =
	     quotient_remainder ? rg_in1 : 64'hFFFFFFFFFFFFFFFF ;
  assign IF_quotient_remainder_8_THEN_rg_in1_5_ELSE_184_ETC__q4 =
	     IF_quotient_remainder_8_THEN_rg_in1_5_ELSE_184_ETC___d26[31:0] ;
  assign IF_rg_upperbits_0_AND_rg_complement_1_2_AND_NO_ETC___d45 =
	     (rg_upperbits && rg_complement &&
	      !IF_quotient_remainder_8_THEN_partial_BITS_127__ETC___d37 ||
	      rg_complement && !rg_upperbits) ?
	       reslt___1__h1209 :
	       reslt__h1121 ;
  assign IF_rg_upperbits_0_AND_rg_complement_1_2_AND_NO_ETC__q3 =
	     IF_rg_upperbits_0_AND_rg_complement_1_2_AND_NO_ETC___d45[31:0] ;
  assign _theResult___fst__h1556 =
	     ma_inputs_funct3[0] ?
	       { 32'd0, ma_inputs_in1[31:0] } :
	       { {32{ma_inputs_in1_BITS_31_TO_0__q1[31]}},
		 ma_inputs_in1_BITS_31_TO_0__q1 } ;
  assign _theResult___snd__h1557 =
	     ma_inputs_funct3[0] ?
	       { 32'd0, ma_inputs_in2[31:0] } :
	       { {32{ma_inputs_in2_BITS_31_TO_0__q2[31]}},
		 ma_inputs_in2_BITS_31_TO_0__q2 } ;
  assign ma_inputs_in1_BITS_31_TO_0__q1 = ma_inputs_in1[31:0] ;
  assign ma_inputs_in2_BITS_31_TO_0__q2 = ma_inputs_in2[31:0] ;
  assign op1__h1424 =
	     (t1__h1460 ^
	      IF_ma_inputs_word32_THEN_IF_ma_inputs_funct3_B_ETC___d71) +
	     { 63'd0, x__h1638 } ;
  assign product__h1049 =
	     rg_word ?
	       { {32{IF_quotient_remainder_8_THEN_rg_in1_5_ELSE_184_ETC__q4[31]}},
		 IF_quotient_remainder_8_THEN_rg_in1_5_ELSE_184_ETC__q4 } :
	       IF_quotient_remainder_8_THEN_rg_in1_5_ELSE_184_ETC___d26 ;
  assign product__h1123 =
	     rg_word ?
	       { {32{IF_rg_upperbits_0_AND_rg_complement_1_2_AND_NO_ETC__q3[31]}},
		 IF_rg_upperbits_0_AND_rg_complement_1_2_AND_NO_ETC__q3 } :
	       IF_rg_upperbits_0_AND_rg_complement_1_2_AND_NO_ETC___d45 ;
  assign reslt___1__h1209 = ~reslt__h1121 + 64'd1 ;
  assign reslt__h1121 = quotient_remainder ? partial[127:64] : partial[63:0] ;
  assign t1__h1460 = {64{x__h1638}} ;
  assign t2__h1461 = {64{x__h1665}} ;
  assign x_BITS_63_TO_0___h1200 = partial[63:0] ;
  assign x__h1199 = partial ;
  assign x__h1638 =
	     ma_inputs_funct3[2] && !ma_inputs_funct3[0] &&
	     IF_ma_inputs_word32_THEN_IF_ma_inputs_funct3_B_ETC___d71[63] ;
  assign x__h1665 =
	     ma_inputs_funct3[2] && !ma_inputs_funct3[0] &&
	     IF_ma_inputs_word32_THEN_IF_ma_inputs_funct3_B_ETC___d83[63] ;
  assign x__h817 = rg_count + 6'd1 ;

  // handling of inlined registers

  always@(posedge CLK)
  begin
    if (RST_N == `BSV_RESET_VALUE)
      begin
        partial <= `BSV_ASSIGNMENT_DELAY 129'd0;
	quotient_remainder <= `BSV_ASSIGNMENT_DELAY 1'd0;
	rg_complement <= `BSV_ASSIGNMENT_DELAY 1'd0;
	rg_count <= `BSV_ASSIGNMENT_DELAY 6'd0;
	rg_in1 <= `BSV_ASSIGNMENT_DELAY 64'd0;
	rg_op2 <= `BSV_ASSIGNMENT_DELAY 64'd0;
	rg_result <= `BSV_ASSIGNMENT_DELAY 64'd0;
	rg_sign_op1 <= `BSV_ASSIGNMENT_DELAY 1'd0;
	rg_upperbits <= `BSV_ASSIGNMENT_DELAY 1'd0;
	rg_valid <= `BSV_ASSIGNMENT_DELAY 1'd0;
	rg_word <= `BSV_ASSIGNMENT_DELAY 1'd0;
      end
    else
      begin
        if (partial_EN) partial <= `BSV_ASSIGNMENT_DELAY partial_D_IN;
	if (quotient_remainder_EN)
	  quotient_remainder <= `BSV_ASSIGNMENT_DELAY quotient_remainder_D_IN;
	if (rg_complement_EN)
	  rg_complement <= `BSV_ASSIGNMENT_DELAY rg_complement_D_IN;
	if (rg_count_EN) rg_count <= `BSV_ASSIGNMENT_DELAY rg_count_D_IN;
	if (rg_in1_EN) rg_in1 <= `BSV_ASSIGNMENT_DELAY rg_in1_D_IN;
	if (rg_op2_EN) rg_op2 <= `BSV_ASSIGNMENT_DELAY rg_op2_D_IN;
	if (rg_result_EN) rg_result <= `BSV_ASSIGNMENT_DELAY rg_result_D_IN;
	if (rg_sign_op1_EN)
	  rg_sign_op1 <= `BSV_ASSIGNMENT_DELAY rg_sign_op1_D_IN;
	if (rg_upperbits_EN)
	  rg_upperbits <= `BSV_ASSIGNMENT_DELAY rg_upperbits_D_IN;
	if (rg_valid_EN) rg_valid <= `BSV_ASSIGNMENT_DELAY rg_valid_D_IN;
	if (rg_word_EN) rg_word <= `BSV_ASSIGNMENT_DELAY rg_word_D_IN;
      end
  end

  // synopsys translate_off
  `ifdef BSV_NO_INITIAL_BLOCKS
  `else // not BSV_NO_INITIAL_BLOCKS
  initial
  begin
    partial = 129'h0AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA;
    quotient_remainder = 1'h0;
    rg_complement = 1'h0;
    rg_count = 6'h2A;
    rg_in1 = 64'hAAAAAAAAAAAAAAAA;
    rg_op2 = 64'hAAAAAAAAAAAAAAAA;
    rg_result = 64'hAAAAAAAAAAAAAAAA;
    rg_sign_op1 = 1'h0;
    rg_upperbits = 1'h0;
    rg_valid = 1'h0;
    rg_word = 1'h0;
  end
  `endif // BSV_NO_INITIAL_BLOCKS
  // synopsys translate_on

  // handling of system tasks

  // synopsys translate_off
  always@(negedge CLK)
  begin
    #0;
    if (RST_N != `BSV_RESET_VALUE)
      if (EN_ma_inputs)
	begin
	  TASK_testplusargs___d57 = $test$plusargs("fullverbose");
	  #0;
	end
    if (RST_N != `BSV_RESET_VALUE)
      if (EN_ma_inputs)
	begin
	  TASK_testplusargs___d58 = $test$plusargs("mdivider");
	  #0;
	end
    if (RST_N != `BSV_RESET_VALUE)
      if (EN_ma_inputs)
	begin
	  TASK_testplusargs___d59 = $test$plusargs("l0");
	  #0;
	end
    if (RST_N != `BSV_RESET_VALUE)
      if (EN_ma_inputs)
	begin
	  v__h1334 = $time;
	  #0;
	end
    if (RST_N != `BSV_RESET_VALUE)
      if (EN_ma_inputs &&
	  (TASK_testplusargs___d57 ||
	   TASK_testplusargs___d58 && TASK_testplusargs___d59))
	$write("[%10d", v__h1334, "] ");
    if (RST_N != `BSV_RESET_VALUE)
      if (EN_ma_inputs &&
	  (TASK_testplusargs___d57 ||
	   TASK_testplusargs___d58 && TASK_testplusargs___d59))
	$write("core:%2d ", hartid, "DIV: Got inputs rg_count: %d", rg_count);
    if (RST_N != `BSV_RESET_VALUE)
      if (EN_ma_inputs &&
	  (TASK_testplusargs___d57 ||
	   TASK_testplusargs___d58 && TASK_testplusargs___d59))
	$write("\n");
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_single_step_div)
	begin
	  TASK_testplusargs___d12 = $test$plusargs("fullverbose");
	  #0;
	end
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_single_step_div)
	begin
	  TASK_testplusargs___d13 = $test$plusargs("mdivider");
	  #0;
	end
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_single_step_div)
	begin
	  TASK_testplusargs___d14 = $test$plusargs("l0");
	  #0;
	end
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_single_step_div)
	begin
	  v__h695 = $time;
	  #0;
	end
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_single_step_div &&
	  (TASK_testplusargs___d12 ||
	   TASK_testplusargs___d13 && TASK_testplusargs___d14))
	$write("[%10d", v__h695, "] ");
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_single_step_div &&
	  (TASK_testplusargs___d12 ||
	   TASK_testplusargs___d13 && TASK_testplusargs___d14))
	$write("core:%2d ",
	       hartid,
	       "DIV: RgCount:%d partial:%h QR:%b",
	       rg_count,
	       partial,
	       quotient_remainder);
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_single_step_div &&
	  (TASK_testplusargs___d12 ||
	   TASK_testplusargs___d13 && TASK_testplusargs___d14))
	$write("\n");
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_single_step_div && rg_op2 == 64'd0)
	begin
	  TASK_testplusargs___d50 = $test$plusargs("fullverbose");
	  #0;
	end
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_single_step_div && rg_op2 == 64'd0)
	begin
	  TASK_testplusargs___d51 = $test$plusargs("mdivider");
	  #0;
	end
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_single_step_div && rg_op2 == 64'd0)
	begin
	  TASK_testplusargs___d52 = $test$plusargs("l0");
	  #0;
	end
    rg_op2_0_EQ_0_9_AND_TASK_testplusargs_0_OR_TAS_ETC___d55 =
	rg_op2 == 64'd0 &&
	(TASK_testplusargs___d50 ||
	 TASK_testplusargs___d51 && TASK_testplusargs___d52);
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_single_step_div && rg_op2 == 64'd0)
	begin
	  v__h888 = $time;
	  #0;
	end
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_single_step_div &&
	  rg_op2_0_EQ_0_9_AND_TASK_testplusargs_0_OR_TAS_ETC___d55)
	$write("[%10d", v__h888, "] ");
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_single_step_div &&
	  rg_op2_0_EQ_0_9_AND_TASK_testplusargs_0_OR_TAS_ETC___d55)
	$write("core:%2d ",
	       hartid,
	       "DIV: Divide by zero detected. RgCount:%d",
	       rg_count);
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_single_step_div &&
	  rg_op2_0_EQ_0_9_AND_TASK_testplusargs_0_OR_TAS_ETC___d55)
	$write("\n");
  end
  // synopsys translate_on
endmodule  // mkrestoring_div

