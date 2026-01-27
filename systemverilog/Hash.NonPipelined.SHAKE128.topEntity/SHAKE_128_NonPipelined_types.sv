package SHAKE_128_NonPipelined_types;
  typedef struct packed {
    logic [8:0] State_sel0;
    logic [1599:0] State_sel1;
  } State;
  typedef logic [0:0] array_of_64_logic_vector_1 [0:63];
  typedef logic [0:0] array_of_1600_logic_vector_1 [0:1599];
  typedef struct packed {
    logic [10:0] Tuple3_sel0;
    logic [10:0] Tuple3_sel1;
    logic [10:0] Tuple3_sel2;
  } Tuple3;
  typedef Tuple3  array_of_1600_Tuple3 [0:1599];
  typedef struct packed {
    logic [10:0] Tuple2_0_sel0;
    logic [1599:0] Tuple2_0_sel1;
  } Tuple2_0;
  typedef logic [10:0] array_of_1600_logic_vector_11 [0:1599];
  typedef struct packed {
    logic [63:0] AXI4Stream_sel0;
    logic AXI4Stream_sel1;
    logic AXI4Stream_sel2;
  } AXI4Stream;
  typedef struct packed {
    AXI4Stream Tuple3_0_sel0;
    logic Tuple3_0_sel1;
    logic Tuple3_0_sel2;
  } Tuple3_0;
  typedef struct packed {
    AXI4Stream Tuple2_1_sel0;
    logic Tuple2_1_sel1;
  } Tuple2_1;
  typedef struct packed {
    State Tuple2_sel0;
    Tuple2_1 Tuple2_sel1;
  } Tuple2;
  typedef logic  array_of_1600_logic [0:1599];
  typedef logic  array_of_64_logic [0:63];
  typedef logic [0:63] array_of_25_array_of_64_logic [0:24];
  typedef logic [0:63] array_of_24_array_of_64_logic [0:23];
  function automatic logic [0:63][0:0] array_of_64_logic_vector_1_to_lv(array_of_64_logic_vector_1 i);
    for (int n = 0; n < 64; n=n+1)
      array_of_64_logic_vector_1_to_lv[n] = i[n];
  endfunction
  function automatic array_of_64_logic_vector_1 array_of_64_logic_vector_1_from_lv(logic [0:63][0:0] i);
    for (int n = 0; n < 64; n=n+1)
      array_of_64_logic_vector_1_from_lv[n] = i[n];
  endfunction
  function automatic array_of_64_logic_vector_1 array_of_64_logic_vector_1_cons(logic [0:0] x,logic [0:0] xs [0:62]);
    array_of_64_logic_vector_1_cons[0] = x;
    array_of_64_logic_vector_1_cons[1:63] = xs;
  endfunction
  function automatic logic [0:1599][0:0] array_of_1600_logic_vector_1_to_lv(array_of_1600_logic_vector_1 i);
    for (int n = 0; n < 1600; n=n+1)
      array_of_1600_logic_vector_1_to_lv[n] = i[n];
  endfunction
  function automatic array_of_1600_logic_vector_1 array_of_1600_logic_vector_1_from_lv(logic [0:1599][0:0] i);
    for (int n = 0; n < 1600; n=n+1)
      array_of_1600_logic_vector_1_from_lv[n] = i[n];
  endfunction
  function automatic array_of_1600_logic_vector_1 array_of_1600_logic_vector_1_cons(logic [0:0] x,logic [0:0] xs [0:1598]);
    array_of_1600_logic_vector_1_cons[0] = x;
    array_of_1600_logic_vector_1_cons[1:1599] = xs;
  endfunction
  function automatic logic [0:1599][32:0] array_of_1600_Tuple3_to_lv(array_of_1600_Tuple3 i);
    for (int n = 0; n < 1600; n=n+1)
      array_of_1600_Tuple3_to_lv[n] = i[n];
  endfunction
  function automatic array_of_1600_Tuple3 array_of_1600_Tuple3_from_lv(logic [0:1599][32:0] i);
    for (int n = 0; n < 1600; n=n+1)
      array_of_1600_Tuple3_from_lv[n] = i[n];
  endfunction
  function automatic array_of_1600_Tuple3 array_of_1600_Tuple3_cons(Tuple3 x,Tuple3  xs [0:1598]);
    array_of_1600_Tuple3_cons[0] = x;
    array_of_1600_Tuple3_cons[1:1599] = xs;
  endfunction
  function automatic logic [0:1599][10:0] array_of_1600_logic_vector_11_to_lv(array_of_1600_logic_vector_11 i);
    for (int n = 0; n < 1600; n=n+1)
      array_of_1600_logic_vector_11_to_lv[n] = i[n];
  endfunction
  function automatic array_of_1600_logic_vector_11 array_of_1600_logic_vector_11_from_lv(logic [0:1599][10:0] i);
    for (int n = 0; n < 1600; n=n+1)
      array_of_1600_logic_vector_11_from_lv[n] = i[n];
  endfunction
  function automatic array_of_1600_logic_vector_11 array_of_1600_logic_vector_11_cons(logic [10:0] x,logic [10:0] xs [0:1598]);
    array_of_1600_logic_vector_11_cons[0] = x;
    array_of_1600_logic_vector_11_cons[1:1599] = xs;
  endfunction
  function automatic logic [0:1599][0:0] array_of_1600_logic_to_lv(array_of_1600_logic i);
    for (int n = 0; n < 1600; n=n+1)
      array_of_1600_logic_to_lv[n] = i[n];
  endfunction
  function automatic array_of_1600_logic array_of_1600_logic_from_lv(logic [0:1599][0:0] i);
    for (int n = 0; n < 1600; n=n+1)
      array_of_1600_logic_from_lv[n] = i[n];
  endfunction
  function automatic array_of_1600_logic array_of_1600_logic_cons(logic x,logic  xs [0:1598]);
    array_of_1600_logic_cons[0] = x;
    array_of_1600_logic_cons[1:1599] = xs;
  endfunction
  function automatic logic [0:63][0:0] array_of_64_logic_to_lv(array_of_64_logic i);
    for (int n = 0; n < 64; n=n+1)
      array_of_64_logic_to_lv[n] = i[n];
  endfunction
  function automatic array_of_64_logic array_of_64_logic_from_lv(logic [0:63][0:0] i);
    for (int n = 0; n < 64; n=n+1)
      array_of_64_logic_from_lv[n] = i[n];
  endfunction
  function automatic array_of_64_logic array_of_64_logic_cons(logic x,logic  xs [0:62]);
    array_of_64_logic_cons[0] = x;
    array_of_64_logic_cons[1:63] = xs;
  endfunction
  function automatic logic [0:24][63:0] array_of_25_array_of_64_logic_to_lv(array_of_25_array_of_64_logic i);
    for (int n = 0; n < 25; n=n+1)
      array_of_25_array_of_64_logic_to_lv[n] = i[n];
  endfunction
  function automatic array_of_25_array_of_64_logic array_of_25_array_of_64_logic_from_lv(logic [0:24][63:0] i);
    for (int n = 0; n < 25; n=n+1)
      array_of_25_array_of_64_logic_from_lv[n] = i[n];
  endfunction
  function automatic array_of_25_array_of_64_logic array_of_25_array_of_64_logic_cons(array_of_64_logic x,logic [0:63] xs [0:23]);
    array_of_25_array_of_64_logic_cons[0] = {array_of_64_logic_to_lv(x)};
    array_of_25_array_of_64_logic_cons[1:24] = xs;
  endfunction
  function automatic logic [0:23][63:0] array_of_24_array_of_64_logic_to_lv(array_of_24_array_of_64_logic i);
    for (int n = 0; n < 24; n=n+1)
      array_of_24_array_of_64_logic_to_lv[n] = i[n];
  endfunction
  function automatic array_of_24_array_of_64_logic array_of_24_array_of_64_logic_from_lv(logic [0:23][63:0] i);
    for (int n = 0; n < 24; n=n+1)
      array_of_24_array_of_64_logic_from_lv[n] = i[n];
  endfunction
  function automatic array_of_24_array_of_64_logic array_of_24_array_of_64_logic_cons(array_of_64_logic x,logic [0:63] xs [0:22]);
    array_of_24_array_of_64_logic_cons[0] = {array_of_64_logic_to_lv(x)};
    array_of_24_array_of_64_logic_cons[1:23] = xs;
  endfunction
endpackage : SHAKE_128_NonPipelined_types

