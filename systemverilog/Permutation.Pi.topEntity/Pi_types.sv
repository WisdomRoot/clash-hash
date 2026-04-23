package Pi_types;
  typedef logic [0:0] array_of_1600_logic_vector_1 [0:1599];
  typedef logic [10:0] array_of_1600_logic_vector_11 [0:1599];
  typedef logic  array_of_1600_logic [0:1599];
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
endpackage : Pi_types

