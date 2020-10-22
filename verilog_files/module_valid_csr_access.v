//
// Generated by Bluespec Compiler (build 8d454e4)
//
// On Fri Oct  9 13:12:35 IST 2020
//
//
// Ports:
// Name                         I/O  size props
// valid_csr_access               O     1
// valid_csr_access_csr_addr      I    12
// valid_csr_access_operand       I     5
// valid_csr_access_operation     I     2
// valid_csr_access_tvm           I     1
// valid_csr_access_prv           I     2
//
// Combinational paths from inputs to outputs:
//   (valid_csr_access_csr_addr,
//    valid_csr_access_operand,
//    valid_csr_access_operation,
//    valid_csr_access_tvm,
//    valid_csr_access_prv) -> valid_csr_access
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

module module_valid_csr_access(valid_csr_access_csr_addr,
			       valid_csr_access_operand,
			       valid_csr_access_operation,
			       valid_csr_access_tvm,
			       valid_csr_access_prv,
			       valid_csr_access);
  // value method valid_csr_access
  input  [11 : 0] valid_csr_access_csr_addr;
  input  [4 : 0] valid_csr_access_operand;
  input  [1 : 0] valid_csr_access_operation;
  input  valid_csr_access_tvm;
  input  [1 : 0] valid_csr_access_prv;
  output valid_csr_access;

  // signals for module outputs
  wire valid_csr_access;

  // remaining internal signals
  reg [1 : 0] CASE_valid_csr_access_prv_1_valid_csr_access_p_ETC__q1;
  wire hasCSRPermission___d16;

  // value method valid_csr_access
  assign valid_csr_access =
	     (valid_csr_access_csr_addr != 12'h180 || !valid_csr_access_tvm ||
	      valid_csr_access_prv != 2'd1) &&
	     hasCSRPermission___d16 ;

  // remaining internal signals
  module_hasCSRPermission instance_hasCSRPermission_0(.hasCSRPermission_address(valid_csr_access_csr_addr),
						      .hasCSRPermission_write(valid_csr_access_operand !=
									      5'd0 ||
									      valid_csr_access_operation ==
									      2'b01),
						      .hasCSRPermission_prv(CASE_valid_csr_access_prv_1_valid_csr_access_p_ETC__q1),
						      .hasCSRPermission(hasCSRPermission___d16));
  always@(valid_csr_access_prv)
  begin
    case (valid_csr_access_prv)
      2'd1, 2'd3:
	  CASE_valid_csr_access_prv_1_valid_csr_access_p_ETC__q1 =
	      valid_csr_access_prv;
      default: CASE_valid_csr_access_prv_1_valid_csr_access_p_ETC__q1 = 2'd0;
    endcase
  end
endmodule  // module_valid_csr_access
