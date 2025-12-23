-- The Keccak sponge function, designed by Guido Bertoni, Joan Daemen,
-- Michaël Peeters and Gilles Van Assche. For more information, feedback or
-- questions, please refer to: http://keccak.noekeon.org/
--
-- This variant keeps only the theta step with pre-computed parity pairs.

library work;
  use work.keccak_globals.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity keccak_round_theta_improved is
  port (
    round_in  : in  k_state;
    round_constant_signal : in std_logic_vector(63 downto 0);
    round_out : out k_state
  );
end keccak_round_theta_improved;

architecture rtl of keccak_round_theta_improved is
  signal theta_in  : k_state;
  signal theta_out : k_state;
  signal sum_sheet : k_plane;
  -- Pre-computed parity pairs for each column
  signal d0 : k_lane;
  signal d1 : k_lane;
  signal d2 : k_lane;
  signal d3 : k_lane;
  signal d4 : k_lane;
begin
  -- Theta is the only retained primitive; round_constant_signal is unused.
  theta_in <= round_in;
  round_out <= theta_out;

  -- Stage 1: Column parities
  i0101: for x in 0 to 4 generate
    i0102: for i in 0 to 63 generate
      sum_sheet(x)(i) <= theta_in(0)(x)(i) xor theta_in(1)(x)(i) xor theta_in(2)(x)(i) xor theta_in(3)(x)(i) xor theta_in(4)(x)(i);
    end generate;
  end generate;

  -- Stage 2: Pre-compute parity pairs (d(x) = sum_sheet(x-1) XOR rotated(sum_sheet(x+1)))
  -- d0: uses sum_sheet(4) and rotated sum_sheet(1)
  d0(0) <= sum_sheet(4)(0) xor sum_sheet(1)(63);
  i_d0: for i in 1 to 63 generate
    d0(i) <= sum_sheet(4)(i) xor sum_sheet(1)(i-1);
  end generate;

  -- d1: uses sum_sheet(0) and rotated sum_sheet(2)
  d1(0) <= sum_sheet(0)(0) xor sum_sheet(2)(63);
  i_d1: for i in 1 to 63 generate
    d1(i) <= sum_sheet(0)(i) xor sum_sheet(2)(i-1);
  end generate;

  -- d2: uses sum_sheet(1) and rotated sum_sheet(3)
  d2(0) <= sum_sheet(1)(0) xor sum_sheet(3)(63);
  i_d2: for i in 1 to 63 generate
    d2(i) <= sum_sheet(1)(i) xor sum_sheet(3)(i-1);
  end generate;

  -- d3: uses sum_sheet(2) and rotated sum_sheet(4)
  d3(0) <= sum_sheet(2)(0) xor sum_sheet(4)(63);
  i_d3: for i in 1 to 63 generate
    d3(i) <= sum_sheet(2)(i) xor sum_sheet(4)(i-1);
  end generate;

  -- d4: uses sum_sheet(3) and rotated sum_sheet(0)
  d4(0) <= sum_sheet(3)(0) xor sum_sheet(0)(63);
  i_d4: for i in 1 to 63 generate
    d4(i) <= sum_sheet(3)(i) xor sum_sheet(0)(i-1);
  end generate;

  -- Stage 3: Apply pre-computed pairs to each lane
  i_apply: for y in 0 to 4 generate
    i_x0: for i in 0 to 63 generate
      theta_out(y)(0)(i) <= theta_in(y)(0)(i) xor d0(i);
    end generate;
    i_x1: for i in 0 to 63 generate
      theta_out(y)(1)(i) <= theta_in(y)(1)(i) xor d1(i);
    end generate;
    i_x2: for i in 0 to 63 generate
      theta_out(y)(2)(i) <= theta_in(y)(2)(i) xor d2(i);
    end generate;
    i_x3: for i in 0 to 63 generate
      theta_out(y)(3)(i) <= theta_in(y)(3)(i) xor d3(i);
    end generate;
    i_x4: for i in 0 to 63 generate
      theta_out(y)(4)(i) <= theta_in(y)(4)(i) xor d4(i);
    end generate;
  end generate;

end rtl;
