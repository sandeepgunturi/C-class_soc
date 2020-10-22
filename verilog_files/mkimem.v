//
// Generated by Bluespec Compiler (build 8d454e4)
//
// On Fri Oct  9 13:15:02 IST 2020
//
//
// Ports:
// Name                         I/O  size props
// RDY_put_core_req_put           O     1
// get_core_resp_get              O    41
// RDY_get_core_resp_get          O     1
// RDY_ma_cache_enable            O     1 const
// get_read_mem_req_get           O    44 reg
// RDY_get_read_mem_req_get       O     1 reg
// RDY_put_read_mem_resp_put      O     1
// mv_cache_available             O     1
// RDY_mv_cache_available         O     1 const
// RDY_ma_curr_priv               O     1 const
// get_request_to_ptw_get         O    66 reg
// RDY_get_request_to_ptw_get     O     1 reg
// RDY_put_response_frm_ptw_put   O     1
// RDY_ma_satp_from_csr           O     1 const
// mv_icache_perf_counters        O     5
// RDY_mv_icache_perf_counters    O     1 const
// mv_itlb_perf_counters          O     1
// RDY_mv_itlb_perf_counters      O     1 const
// pmp_cfg_0                      I     8
// pmp_cfg_1                      I     8
// pmp_cfg_2                      I     8
// pmp_cfg_3                      I     8
// pmp_addr_0                     I    29
// pmp_addr_1                     I    29
// pmp_addr_2                     I    29
// pmp_addr_3                     I    29
// CLK                            I     1 clock
// RST_N                          I     1 reset
// put_core_req_put               I    68
// ma_cache_enable_c              I     1
// put_read_mem_resp_put          I    66
// ma_curr_priv_c                 I     2
// put_response_frm_ptw_put       I    63
// ma_satp_from_csr_s             I    64
// EN_put_core_req_put            I     1
// EN_ma_cache_enable             I     1
// EN_put_read_mem_resp_put       I     1
// EN_ma_curr_priv                I     1
// EN_put_response_frm_ptw_put    I     1
// EN_ma_satp_from_csr            I     1
// EN_get_core_resp_get           I     1
// EN_get_read_mem_req_get        I     1
// EN_get_request_to_ptw_get      I     1
//
// Combinational paths from inputs to outputs:
//   (put_core_req_put,
//    ma_cache_enable_c,
//    put_read_mem_resp_put,
//    ma_curr_priv_c,
//    EN_put_core_req_put,
//    EN_ma_cache_enable,
//    EN_put_read_mem_resp_put,
//    EN_ma_curr_priv,
//    EN_ma_satp_from_csr,
//    pmp_cfg_0,
//    pmp_cfg_1,
//    pmp_cfg_2,
//    pmp_cfg_3,
//    pmp_addr_0,
//    pmp_addr_1,
//    pmp_addr_2,
//    pmp_addr_3) -> mv_icache_perf_counters
//   (put_core_req_put,
//    ma_curr_priv_c,
//    ma_satp_from_csr_s,
//    EN_put_core_req_put,
//    EN_ma_cache_enable,
//    EN_ma_curr_priv,
//    EN_ma_satp_from_csr,
//    pmp_cfg_0,
//    pmp_cfg_1,
//    pmp_cfg_2,
//    pmp_cfg_3,
//    pmp_addr_0,
//    pmp_addr_1,
//    pmp_addr_2,
//    pmp_addr_3) -> mv_itlb_perf_counters
//   (ma_cache_enable_c,
//    put_read_mem_resp_put,
//    ma_curr_priv_c,
//    EN_ma_cache_enable,
//    EN_put_read_mem_resp_put,
//    EN_ma_curr_priv,
//    pmp_cfg_0,
//    pmp_cfg_1,
//    pmp_cfg_2,
//    pmp_cfg_3,
//    pmp_addr_0,
//    pmp_addr_1,
//    pmp_addr_2,
//    pmp_addr_3) -> RDY_get_core_resp_get
//   (ma_cache_enable_c,
//    put_read_mem_resp_put,
//    ma_curr_priv_c,
//    EN_ma_cache_enable,
//    EN_put_read_mem_resp_put,
//    EN_ma_curr_priv,
//    pmp_cfg_0,
//    pmp_cfg_1,
//    pmp_cfg_2,
//    pmp_cfg_3,
//    pmp_addr_0,
//    pmp_addr_1,
//    pmp_addr_2,
//    pmp_addr_3) -> get_core_resp_get
//   (EN_ma_cache_enable,
//    EN_ma_curr_priv,
//    EN_ma_satp_from_csr,
//    pmp_cfg_0,
//    pmp_cfg_1,
//    pmp_cfg_2,
//    pmp_cfg_3,
//    pmp_addr_0,
//    pmp_addr_1,
//    pmp_addr_2,
//    pmp_addr_3) -> RDY_put_core_req_put
//   EN_ma_satp_from_csr -> RDY_put_response_frm_ptw_put
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

module mkimem(pmp_cfg_0,
	      pmp_cfg_1,
	      pmp_cfg_2,
	      pmp_cfg_3,
	      pmp_addr_0,
	      pmp_addr_1,
	      pmp_addr_2,
	      pmp_addr_3,
	      CLK,
	      RST_N,

	      put_core_req_put,
	      EN_put_core_req_put,
	      RDY_put_core_req_put,

	      EN_get_core_resp_get,
	      get_core_resp_get,
	      RDY_get_core_resp_get,

	      ma_cache_enable_c,
	      EN_ma_cache_enable,
	      RDY_ma_cache_enable,

	      EN_get_read_mem_req_get,
	      get_read_mem_req_get,
	      RDY_get_read_mem_req_get,

	      put_read_mem_resp_put,
	      EN_put_read_mem_resp_put,
	      RDY_put_read_mem_resp_put,

	      mv_cache_available,
	      RDY_mv_cache_available,

	      ma_curr_priv_c,
	      EN_ma_curr_priv,
	      RDY_ma_curr_priv,

	      EN_get_request_to_ptw_get,
	      get_request_to_ptw_get,
	      RDY_get_request_to_ptw_get,

	      put_response_frm_ptw_put,
	      EN_put_response_frm_ptw_put,
	      RDY_put_response_frm_ptw_put,

	      ma_satp_from_csr_s,
	      EN_ma_satp_from_csr,
	      RDY_ma_satp_from_csr,

	      mv_icache_perf_counters,
	      RDY_mv_icache_perf_counters,

	      mv_itlb_perf_counters,
	      RDY_mv_itlb_perf_counters);
  parameter [31 : 0] id = 32'b0;
  input  [7 : 0] pmp_cfg_0;
  input  [7 : 0] pmp_cfg_1;
  input  [7 : 0] pmp_cfg_2;
  input  [7 : 0] pmp_cfg_3;
  input  [28 : 0] pmp_addr_0;
  input  [28 : 0] pmp_addr_1;
  input  [28 : 0] pmp_addr_2;
  input  [28 : 0] pmp_addr_3;
  input  CLK;
  input  RST_N;

  // action method put_core_req_put
  input  [67 : 0] put_core_req_put;
  input  EN_put_core_req_put;
  output RDY_put_core_req_put;

  // actionvalue method get_core_resp_get
  input  EN_get_core_resp_get;
  output [40 : 0] get_core_resp_get;
  output RDY_get_core_resp_get;

  // action method ma_cache_enable
  input  ma_cache_enable_c;
  input  EN_ma_cache_enable;
  output RDY_ma_cache_enable;

  // actionvalue method get_read_mem_req_get
  input  EN_get_read_mem_req_get;
  output [43 : 0] get_read_mem_req_get;
  output RDY_get_read_mem_req_get;

  // action method put_read_mem_resp_put
  input  [65 : 0] put_read_mem_resp_put;
  input  EN_put_read_mem_resp_put;
  output RDY_put_read_mem_resp_put;

  // value method mv_cache_available
  output mv_cache_available;
  output RDY_mv_cache_available;

  // action method ma_curr_priv
  input  [1 : 0] ma_curr_priv_c;
  input  EN_ma_curr_priv;
  output RDY_ma_curr_priv;

  // actionvalue method get_request_to_ptw_get
  input  EN_get_request_to_ptw_get;
  output [65 : 0] get_request_to_ptw_get;
  output RDY_get_request_to_ptw_get;

  // action method put_response_frm_ptw_put
  input  [62 : 0] put_response_frm_ptw_put;
  input  EN_put_response_frm_ptw_put;
  output RDY_put_response_frm_ptw_put;

  // action method ma_satp_from_csr
  input  [63 : 0] ma_satp_from_csr_s;
  input  EN_ma_satp_from_csr;
  output RDY_ma_satp_from_csr;

  // value method mv_icache_perf_counters
  output [4 : 0] mv_icache_perf_counters;
  output RDY_mv_icache_perf_counters;

  // value method mv_itlb_perf_counters
  output mv_itlb_perf_counters;
  output RDY_mv_itlb_perf_counters;

  // signals for module outputs
  wire [65 : 0] get_request_to_ptw_get;
  wire [43 : 0] get_read_mem_req_get;
  wire [40 : 0] get_core_resp_get;
  wire [4 : 0] mv_icache_perf_counters;
  wire RDY_get_core_resp_get,
       RDY_get_read_mem_req_get,
       RDY_get_request_to_ptw_get,
       RDY_ma_cache_enable,
       RDY_ma_curr_priv,
       RDY_ma_satp_from_csr,
       RDY_mv_cache_available,
       RDY_mv_icache_perf_counters,
       RDY_mv_itlb_perf_counters,
       RDY_put_core_req_put,
       RDY_put_read_mem_resp_put,
       RDY_put_response_frm_ptw_put,
       mv_cache_available,
       mv_itlb_perf_counters;

  // ports of submodule icache
  wire [66 : 0] icache_put_core_req_put;
  wire [65 : 0] icache_put_read_mem_resp_put;
  wire [43 : 0] icache_get_read_mem_req_get;
  wire [40 : 0] icache_get_core_resp_get;
  wire [38 : 0] icache_put_pa_from_tlb_put;
  wire [4 : 0] icache_mv_perf_counters;
  wire [1 : 0] icache_ma_curr_priv_c;
  wire icache_EN_get_core_resp_get,
       icache_EN_get_read_mem_req_get,
       icache_EN_ma_cache_enable,
       icache_EN_ma_curr_priv,
       icache_EN_put_core_req_put,
       icache_EN_put_pa_from_tlb_put,
       icache_EN_put_read_mem_resp_put,
       icache_RDY_get_core_resp_get,
       icache_RDY_get_read_mem_req_get,
       icache_RDY_put_core_req_put,
       icache_RDY_put_pa_from_tlb_put,
       icache_RDY_put_read_mem_resp_put,
       icache_ma_cache_enable_c,
       icache_mv_cache_available;

  // ports of submodule itlb
  wire [65 : 0] itlb_get_request_to_ptw_get;
  wire [64 : 0] itlb_put_core_request_put;
  wire [63 : 0] itlb_ma_satp_from_csr_s;
  wire [62 : 0] itlb_put_response_frm_ptw_put;
  wire [38 : 0] itlb_get_core_response_get;
  wire [1 : 0] itlb_ma_curr_priv_c;
  wire itlb_EN_get_core_response_get,
       itlb_EN_get_request_to_ptw_get,
       itlb_EN_ma_curr_priv,
       itlb_EN_ma_satp_from_csr,
       itlb_EN_put_core_request_put,
       itlb_EN_put_response_frm_ptw_put,
       itlb_RDY_get_core_response_get,
       itlb_RDY_get_request_to_ptw_get,
       itlb_RDY_put_core_request_put,
       itlb_RDY_put_response_frm_ptw_put,
       itlb_mv_perf_counters;

  // rule scheduling signals
  wire CAN_FIRE_RL_mkConnectionGetPut,
       CAN_FIRE_get_core_resp_get,
       CAN_FIRE_get_read_mem_req_get,
       CAN_FIRE_get_request_to_ptw_get,
       CAN_FIRE_ma_cache_enable,
       CAN_FIRE_ma_curr_priv,
       CAN_FIRE_ma_satp_from_csr,
       CAN_FIRE_put_core_req_put,
       CAN_FIRE_put_read_mem_resp_put,
       CAN_FIRE_put_response_frm_ptw_put,
       WILL_FIRE_RL_mkConnectionGetPut,
       WILL_FIRE_get_core_resp_get,
       WILL_FIRE_get_read_mem_req_get,
       WILL_FIRE_get_request_to_ptw_get,
       WILL_FIRE_ma_cache_enable,
       WILL_FIRE_ma_curr_priv,
       WILL_FIRE_ma_satp_from_csr,
       WILL_FIRE_put_core_req_put,
       WILL_FIRE_put_read_mem_resp_put,
       WILL_FIRE_put_response_frm_ptw_put;

  // action method put_core_req_put
  assign RDY_put_core_req_put =
	     icache_RDY_put_core_req_put && itlb_RDY_put_core_request_put ;
  assign CAN_FIRE_put_core_req_put =
	     icache_RDY_put_core_req_put && itlb_RDY_put_core_request_put ;
  assign WILL_FIRE_put_core_req_put = EN_put_core_req_put ;

  // actionvalue method get_core_resp_get
  assign get_core_resp_get = icache_get_core_resp_get ;
  assign RDY_get_core_resp_get = icache_RDY_get_core_resp_get ;
  assign CAN_FIRE_get_core_resp_get = icache_RDY_get_core_resp_get ;
  assign WILL_FIRE_get_core_resp_get = EN_get_core_resp_get ;

  // action method ma_cache_enable
  assign RDY_ma_cache_enable = 1'd1 ;
  assign CAN_FIRE_ma_cache_enable = 1'd1 ;
  assign WILL_FIRE_ma_cache_enable = EN_ma_cache_enable ;

  // actionvalue method get_read_mem_req_get
  assign get_read_mem_req_get = icache_get_read_mem_req_get ;
  assign RDY_get_read_mem_req_get = icache_RDY_get_read_mem_req_get ;
  assign CAN_FIRE_get_read_mem_req_get = icache_RDY_get_read_mem_req_get ;
  assign WILL_FIRE_get_read_mem_req_get = EN_get_read_mem_req_get ;

  // action method put_read_mem_resp_put
  assign RDY_put_read_mem_resp_put = icache_RDY_put_read_mem_resp_put ;
  assign CAN_FIRE_put_read_mem_resp_put = icache_RDY_put_read_mem_resp_put ;
  assign WILL_FIRE_put_read_mem_resp_put = EN_put_read_mem_resp_put ;

  // value method mv_cache_available
  assign mv_cache_available = icache_mv_cache_available ;
  assign RDY_mv_cache_available = 1'd1 ;

  // action method ma_curr_priv
  assign RDY_ma_curr_priv = 1'd1 ;
  assign CAN_FIRE_ma_curr_priv = 1'd1 ;
  assign WILL_FIRE_ma_curr_priv = EN_ma_curr_priv ;

  // actionvalue method get_request_to_ptw_get
  assign get_request_to_ptw_get = itlb_get_request_to_ptw_get ;
  assign RDY_get_request_to_ptw_get = itlb_RDY_get_request_to_ptw_get ;
  assign CAN_FIRE_get_request_to_ptw_get = itlb_RDY_get_request_to_ptw_get ;
  assign WILL_FIRE_get_request_to_ptw_get = EN_get_request_to_ptw_get ;

  // action method put_response_frm_ptw_put
  assign RDY_put_response_frm_ptw_put = itlb_RDY_put_response_frm_ptw_put ;
  assign CAN_FIRE_put_response_frm_ptw_put =
	     itlb_RDY_put_response_frm_ptw_put ;
  assign WILL_FIRE_put_response_frm_ptw_put = EN_put_response_frm_ptw_put ;

  // action method ma_satp_from_csr
  assign RDY_ma_satp_from_csr = 1'd1 ;
  assign CAN_FIRE_ma_satp_from_csr = 1'd1 ;
  assign WILL_FIRE_ma_satp_from_csr = EN_ma_satp_from_csr ;

  // value method mv_icache_perf_counters
  assign mv_icache_perf_counters = icache_mv_perf_counters ;
  assign RDY_mv_icache_perf_counters = 1'd1 ;

  // value method mv_itlb_perf_counters
  assign mv_itlb_perf_counters = itlb_mv_perf_counters ;
  assign RDY_mv_itlb_perf_counters = 1'd1 ;

  // submodule icache
  mkicache #(.id(id)) icache(.pmp_cfg_0(pmp_cfg_0),
			     .pmp_cfg_1(pmp_cfg_1),
			     .pmp_cfg_2(pmp_cfg_2),
			     .pmp_cfg_3(pmp_cfg_3),
			     .pmp_addr_0(pmp_addr_0),
			     .pmp_addr_1(pmp_addr_1),
			     .pmp_addr_2(pmp_addr_2),
			     .pmp_addr_3(pmp_addr_3),
			     .CLK(CLK),
			     .RST_N(RST_N),
			     .ma_cache_enable_c(icache_ma_cache_enable_c),
			     .ma_curr_priv_c(icache_ma_curr_priv_c),
			     .put_core_req_put(icache_put_core_req_put),
			     .put_pa_from_tlb_put(icache_put_pa_from_tlb_put),
			     .put_read_mem_resp_put(icache_put_read_mem_resp_put),
			     .EN_put_core_req_put(icache_EN_put_core_req_put),
			     .EN_get_core_resp_get(icache_EN_get_core_resp_get),
			     .EN_get_read_mem_req_get(icache_EN_get_read_mem_req_get),
			     .EN_put_read_mem_resp_put(icache_EN_put_read_mem_resp_put),
			     .EN_put_pa_from_tlb_put(icache_EN_put_pa_from_tlb_put),
			     .EN_ma_cache_enable(icache_EN_ma_cache_enable),
			     .EN_ma_curr_priv(icache_EN_ma_curr_priv),
			     .RDY_put_core_req_put(icache_RDY_put_core_req_put),
			     .get_core_resp_get(icache_get_core_resp_get),
			     .RDY_get_core_resp_get(icache_RDY_get_core_resp_get),
			     .get_read_mem_req_get(icache_get_read_mem_req_get),
			     .RDY_get_read_mem_req_get(icache_RDY_get_read_mem_req_get),
			     .RDY_put_read_mem_resp_put(icache_RDY_put_read_mem_resp_put),
			     .RDY_put_pa_from_tlb_put(icache_RDY_put_pa_from_tlb_put),
			     .mv_perf_counters(icache_mv_perf_counters),
			     .RDY_mv_perf_counters(),
			     .RDY_ma_cache_enable(),
			     .RDY_ma_curr_priv(),
			     .mv_cache_available(icache_mv_cache_available),
			     .RDY_mv_cache_available());

  // submodule itlb
  mkfa_itlb #(.hartid(id)) itlb(.CLK(CLK),
				.RST_N(RST_N),
				.ma_curr_priv_c(itlb_ma_curr_priv_c),
				.ma_satp_from_csr_s(itlb_ma_satp_from_csr_s),
				.put_core_request_put(itlb_put_core_request_put),
				.put_response_frm_ptw_put(itlb_put_response_frm_ptw_put),
				.EN_put_core_request_put(itlb_EN_put_core_request_put),
				.EN_get_core_response_get(itlb_EN_get_core_response_get),
				.EN_get_request_to_ptw_get(itlb_EN_get_request_to_ptw_get),
				.EN_put_response_frm_ptw_put(itlb_EN_put_response_frm_ptw_put),
				.EN_ma_satp_from_csr(itlb_EN_ma_satp_from_csr),
				.EN_ma_curr_priv(itlb_EN_ma_curr_priv),
				.RDY_put_core_request_put(itlb_RDY_put_core_request_put),
				.get_core_response_get(itlb_get_core_response_get),
				.RDY_get_core_response_get(itlb_RDY_get_core_response_get),
				.get_request_to_ptw_get(itlb_get_request_to_ptw_get),
				.RDY_get_request_to_ptw_get(itlb_RDY_get_request_to_ptw_get),
				.RDY_put_response_frm_ptw_put(itlb_RDY_put_response_frm_ptw_put),
				.RDY_ma_satp_from_csr(),
				.RDY_ma_curr_priv(),
				.mv_perf_counters(itlb_mv_perf_counters),
				.RDY_mv_perf_counters());

  // rule RL_mkConnectionGetPut
  assign CAN_FIRE_RL_mkConnectionGetPut =
	     icache_RDY_put_pa_from_tlb_put &&
	     itlb_RDY_get_core_response_get ;
  assign WILL_FIRE_RL_mkConnectionGetPut = CAN_FIRE_RL_mkConnectionGetPut ;

  // submodule icache
  assign icache_ma_cache_enable_c = ma_cache_enable_c ;
  assign icache_ma_curr_priv_c = ma_curr_priv_c ;
  assign icache_put_core_req_put =
	     { put_core_req_put[67:4],
	       put_core_req_put[1],
	       put_core_req_put[3:2] } ;
  assign icache_put_pa_from_tlb_put = itlb_get_core_response_get ;
  assign icache_put_read_mem_resp_put = put_read_mem_resp_put ;
  assign icache_EN_put_core_req_put =
	     EN_put_core_req_put && !put_core_req_put[0] ;
  assign icache_EN_get_core_resp_get = EN_get_core_resp_get ;
  assign icache_EN_get_read_mem_req_get = EN_get_read_mem_req_get ;
  assign icache_EN_put_read_mem_resp_put = EN_put_read_mem_resp_put ;
  assign icache_EN_put_pa_from_tlb_put = CAN_FIRE_RL_mkConnectionGetPut ;
  assign icache_EN_ma_cache_enable = EN_ma_cache_enable ;
  assign icache_EN_ma_curr_priv = EN_ma_curr_priv ;

  // submodule itlb
  assign itlb_ma_curr_priv_c = ma_curr_priv_c ;
  assign itlb_ma_satp_from_csr_s = ma_satp_from_csr_s ;
  assign itlb_put_core_request_put =
	     { put_core_req_put[67:4], put_core_req_put[0] } ;
  assign itlb_put_response_frm_ptw_put = put_response_frm_ptw_put ;
  assign itlb_EN_put_core_request_put =
	     EN_put_core_req_put && !put_core_req_put[1] ;
  assign itlb_EN_get_core_response_get = CAN_FIRE_RL_mkConnectionGetPut ;
  assign itlb_EN_get_request_to_ptw_get = EN_get_request_to_ptw_get ;
  assign itlb_EN_put_response_frm_ptw_put = EN_put_response_frm_ptw_put ;
  assign itlb_EN_ma_satp_from_csr = EN_ma_satp_from_csr ;
  assign itlb_EN_ma_curr_priv = EN_ma_curr_priv ;
endmodule  // mkimem
