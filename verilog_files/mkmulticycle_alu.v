//
// Generated by Bluespec Compiler (build 8d454e4)
//
// On Fri Oct  9 13:13:29 IST 2020
//
//
// Ports:
// Name                         I/O  size props
// RDY_ma_inputs                  O     1
// mv_delayed_output              O    70
// RDY_mv_delayed_output          O     1 const
// CLK                            I     1 clock
// RST_N                          I     1 reset
// ma_inputs_fn                   I     4 reg
// ma_inputs_funct3               I     3
// ma_inputs_op1                  I    64
// ma_inputs_op2                  I    64
// ma_inputs_op3                  I    64 reg
// ma_inputs_inst_type            I     4
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

module mkmulticycle_alu(CLK,
			RST_N,

			ma_inputs_fn,
			ma_inputs_funct3,
			ma_inputs_op1,
			ma_inputs_op2,
			ma_inputs_op3,
			ma_inputs_inst_type,
			ma_inputs_word32,
			EN_ma_inputs,
			RDY_ma_inputs,

			mv_delayed_output,
			RDY_mv_delayed_output);
  parameter [63 : 0] hartid = 64'b0;
  input  CLK;
  input  RST_N;

  // action method ma_inputs
  input  [3 : 0] ma_inputs_fn;
  input  [2 : 0] ma_inputs_funct3;
  input  [63 : 0] ma_inputs_op1;
  input  [63 : 0] ma_inputs_op2;
  input  [63 : 0] ma_inputs_op3;
  input  [3 : 0] ma_inputs_inst_type;
  input  ma_inputs_word32;
  input  EN_ma_inputs;
  output RDY_ma_inputs;

  // value method mv_delayed_output
  output [69 : 0] mv_delayed_output;
  output RDY_mv_delayed_output;

  // signals for module outputs
  wire [69 : 0] mv_delayed_output;
  wire RDY_ma_inputs, RDY_mv_delayed_output;

  // ports of submodule fpu
  wire [69 : 0] fpu_get_result;
  wire [63 : 0] fpu__start_operand1, fpu__start_operand2, fpu__start_operand3;
  wire [6 : 0] fpu__start_funct7;
  wire [3 : 0] fpu__start_opcode;
  wire [2 : 0] fpu__start_funct3;
  wire [1 : 0] fpu__start_imm;
  wire fpu_EN__start, fpu_EN_flush, fpu_RDY__start, fpu__start_issp;

  // ports of submodule mbox
  wire [69 : 0] mbox_mv_output;
  wire [63 : 0] mbox_ma_inputs_in1, mbox_ma_inputs_in2;
  wire [2 : 0] mbox_ma_inputs_funct3;
  wire mbox_EN_ma_inputs, mbox_RDY_mv_output, mbox_ma_inputs_word32;

  // rule scheduling signals
  wire CAN_FIRE_ma_inputs, WILL_FIRE_ma_inputs;

  // action method ma_inputs
  assign RDY_ma_inputs = fpu_RDY__start ;
  assign CAN_FIRE_ma_inputs = fpu_RDY__start ;
  assign WILL_FIRE_ma_inputs = EN_ma_inputs ;

  // value method mv_delayed_output
  assign mv_delayed_output =
	     { fpu_get_result[5] ?
		 fpu_get_result[69:6] :
		 mbox_mv_output[69:6],
	       fpu_get_result[5] || mbox_mv_output[5],
	       fpu_get_result[5] ?
		 fpu_get_result[4:0] :
		 mbox_mv_output[4:0] } ;
  assign RDY_mv_delayed_output = mbox_RDY_mv_output ;

  // submodule fpu
  mkfpu fpu(.CLK(CLK),
	    .RST_N(RST_N),
	    ._start_funct3(fpu__start_funct3),
	    ._start_funct7(fpu__start_funct7),
	    ._start_imm(fpu__start_imm),
	    ._start_issp(fpu__start_issp),
	    ._start_opcode(fpu__start_opcode),
	    ._start_operand1(fpu__start_operand1),
	    ._start_operand2(fpu__start_operand2),
	    ._start_operand3(fpu__start_operand3),
	    .EN__start(fpu_EN__start),
	    .EN_flush(fpu_EN_flush),
	    .RDY__start(fpu_RDY__start),
	    .get_result(fpu_get_result),
	    .RDY_get_result(),
	    .RDY_flush());

  // submodule mbox
  mkmbox #(.hartid(hartid)) mbox(.CLK(CLK),
				 .RST_N(RST_N),
				 .ma_inputs_funct3(mbox_ma_inputs_funct3),
				 .ma_inputs_in1(mbox_ma_inputs_in1),
				 .ma_inputs_in2(mbox_ma_inputs_in2),
				 .ma_inputs_word32(mbox_ma_inputs_word32),
				 .EN_ma_inputs(mbox_EN_ma_inputs),
				 .mv_output(mbox_mv_output),
				 .RDY_mv_output(mbox_RDY_mv_output));

  // submodule fpu
  assign fpu__start_funct3 = ma_inputs_funct3 ;
  assign fpu__start_funct7 = ma_inputs_op3[11:5] ;
  assign fpu__start_imm = ma_inputs_op3[1:0] ;
  assign fpu__start_issp = ma_inputs_word32 ;
  assign fpu__start_opcode = ma_inputs_fn ;
  assign fpu__start_operand1 = ma_inputs_op1 ;
  assign fpu__start_operand2 = ma_inputs_op2 ;
  assign fpu__start_operand3 = ma_inputs_op3 ;
  assign fpu_EN__start = EN_ma_inputs && ma_inputs_inst_type == 4'd8 ;
  assign fpu_EN_flush = 1'b0 ;

  // submodule mbox
  assign mbox_ma_inputs_funct3 = ma_inputs_funct3 ;
  assign mbox_ma_inputs_in1 = ma_inputs_op1 ;
  assign mbox_ma_inputs_in2 = ma_inputs_op2 ;
  assign mbox_ma_inputs_word32 = ma_inputs_word32 ;
  assign mbox_EN_ma_inputs = EN_ma_inputs && ma_inputs_inst_type == 4'd9 ;
endmodule  // mkmulticycle_alu

