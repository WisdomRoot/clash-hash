{-# LANGUAGE TypeApplications #-}

module KeccakF800.Permutation
  ( -- * Round primitives
    thetaF800,
    rhoF800,
    piF800,
    chiF800,
    iotaF800,
    -- * Permutation
    keccakF800Round,
    keccakF800,
    -- * Top entity
    topEntity,
  )
where

import Clash.Prelude
import qualified Constants

--------------------------------------------------------------------------------
-- Round primitives
--------------------------------------------------------------------------------

thetaF800 :: BitVector 800 -> BitVector 800
thetaF800 bv = bitCoerce $ map (fold xor . map (bv !)) $(Constants.theta 5)

chiF800 :: BitVector 800 -> BitVector 800
chiF800 bv = bitCoerce $ map (\(i0, i1, i2) -> bv ! i0 `xor` (complement (bv ! i1) .&. bv ! i2)) $(Constants.chi 5)

piF800 :: BitVector 800 -> BitVector 800
piF800 bv = bitCoerce $ map (bv !) $(Constants.pi 5)

rhoF800 :: BitVector 800 -> BitVector 800
rhoF800 bv = bitCoerce $ map (bv !) $(Constants.rho 5)

iotaF800 :: Index 24 -> BitVector 800 -> BitVector 800
iotaF800 roundIdx bv =
  let lane0 = slice d31 d0 bv
      lane0' = lane0 `xor` truncateB ($(Constants.iota) !! roundIdx)
   in slice d799 d32 bv ++# lane0'

--------------------------------------------------------------------------------
-- Permutation
--------------------------------------------------------------------------------

keccakF800Round :: Index 24 -> BitVector 800 -> BitVector 800
keccakF800Round roundIdx =
  iotaF800 roundIdx . chiF800 . piF800 . rhoF800 . thetaF800

keccakF800 :: BitVector 800 -> BitVector 800
keccakF800 initialState =
  foldl applyRound initialState (indicesI @22)
  where
    applyRound state roundIdx = keccakF800Round (resize roundIdx) state

--------------------------------------------------------------------------------
-- Top entity for hardware synthesis
--------------------------------------------------------------------------------

-- | Top entity for single Keccak-f[800] round
--
-- This module generates a standalone combinational block for a single round,
-- which can be instantiated by the FSM to execute one round per cycle.
--
-- = Ports
--
-- * ROUND_IDX - 5-bit round index (0-23, though only 0-21 are used)
-- * STATE_IN - 800-bit input state
-- * STATE_OUT - 800-bit output state after one round
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "KeccakF800_Round",
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
  Signal System (Index 24, BitVector 800) ->
  Signal System (BitVector 800)
topEntity _clk _rst _en input = fmap (uncurry keccakF800Round) input
