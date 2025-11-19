{-# LANGUAGE TypeApplications #-}

module KeccakF400.Permutation
  ( -- * Round primitives
    thetaF400,
    rhoF400,
    piF400,
    chiF400,
    iotaF400,
    -- * Permutation
    keccakF400Round,
    keccakF400,
    -- * Top entity
    topEntity,
  )
where

import Clash.Prelude
import qualified Constants

--------------------------------------------------------------------------------
-- Round primitives
--------------------------------------------------------------------------------

thetaF400 :: BitVector 400 -> BitVector 400
thetaF400 bv = bitCoerce $ map (fold xor . map (bv !)) $(Constants.theta 4)

chiF400 :: BitVector 400 -> BitVector 400
chiF400 bv = bitCoerce $ map (\(i0, i1, i2) -> bv ! i0 `xor` (complement (bv ! i1) .&. bv ! i2)) $(Constants.chi 4)

piF400 :: BitVector 400 -> BitVector 400
piF400 bv = bitCoerce $ map (bv !) $(Constants.pi 4)

rhoF400 :: BitVector 400 -> BitVector 400
rhoF400 bv = bitCoerce $ map (bv !) $(Constants.rho 4)

iotaF400 :: Index 24 -> BitVector 400 -> BitVector 400
iotaF400 roundIdx bv =
  let lane0 = slice d15 d0 bv
      lane0' = lane0 `xor` truncateB ($(Constants.iota) !! roundIdx)
   in slice d399 d16 bv ++# lane0'

--------------------------------------------------------------------------------
-- Permutation
--------------------------------------------------------------------------------

keccakF400Round :: Index 24 -> BitVector 400 -> BitVector 400
keccakF400Round roundIdx =
  iotaF400 roundIdx . chiF400 . piF400 . rhoF400 . thetaF400

keccakF400 :: BitVector 400 -> BitVector 400
keccakF400 initialState =
  foldl applyRound initialState (indicesI @20)
  where
    applyRound state roundIdx = keccakF400Round (resize roundIdx) state

--------------------------------------------------------------------------------
-- Top entity for hardware synthesis
--------------------------------------------------------------------------------

-- | Top entity for single Keccak-f[400] round
--
-- This module generates a standalone combinational block for a single round,
-- which can be instantiated by the FSM to execute one round per cycle.
--
-- = Ports
--
-- * ROUND_IDX - 5-bit round index (0-23, though only 0-19 are used)
-- * STATE_IN - 400-bit input state
-- * STATE_OUT - 400-bit output state after one round
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "KeccakF400_Round",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "ROUND_IDX",
            PortName "STATE_IN"
          ],
        t_output = PortName "STATE_OUT"
      }
  )
  #-}
{-# NOINLINE topEntity #-}
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (Index 24, BitVector 400) ->
  Signal System (BitVector 400)
topEntity _clk _rst _en input = fmap (uncurry keccakF400Round) input
