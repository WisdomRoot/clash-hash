{-# LANGUAGE TemplateHaskell #-}

-- | Scaffold component for the ML-DSA (Dilithium) NTT work.
--
-- This module is intentionally trivial: it adds two ML-DSA coefficients modulo
-- the Dilithium prime @q = 8380417@. It is the intern's starting point.
--
-- Its whole purpose is to make the /plumbing/ work end-to-end __before__ any
-- real NTT logic exists:
--
--   * it is a proper 'Synthesize'-annotated @topEntity@, so
--     @synth NTT@ and @bench NTT@ both run (see @clash.json@);
--   * it computes something real (@a + b mod q@), so it is __not__ 'undefined'
--     and actually synthesizes to gates.
--
-- Suggested growth path:
--
--   1. Keep this module compiling and synthesizing at every step.
--   2. Replace 'addModQ' with a real NTT butterfly (Montgomery multiply + add/sub).
--   3. Grow the port shape from a single @(a, b)@ pair toward a full
--      length-256 transform.
module Component.NTT
  ( topEntity,
  )
where

import Clash.Prelude

-- | The ML-DSA (Dilithium) modulus: @q = 2^23 - 2^13 + 1 = 8380417@.
-- Held in 24 bits so the sum of two reduced coefficients cannot overflow.
q :: Unsigned 24
q = 8380417

-- | Add two coefficients modulo 'q'.
--
-- Both inputs are assumed already reduced (in @[0, q)@), so their sum lies in
-- @[0, 2q)@ and a single conditional subtraction brings it back into @[0, q)@.
addModQ :: BitVector 23 -> BitVector 23 -> BitVector 23
addModQ a b =
  let s = extend (unpack a :: Unsigned 23) + extend (unpack b :: Unsigned 23) :: Unsigned 24
      reduced = if s >= q then s - q else s
   in pack (resize reduced :: Unsigned 23)

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "dut",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortProduct "" [PortName "A", PortName "B"]
          ],
        t_output = PortName "SUM"
      }
  )
  #-}
{-# NOINLINE topEntity #-}
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector 23, BitVector 23) ->
  Signal System (BitVector 23)
topEntity clk rst en =
  withClockResetEnable clk rst en (fmap (uncurry addModQ))
