{-# LANGUAGE TypeApplications #-}

module KeccakF1600.Permutation
  ( -- * Round primitives
    thetaF1600,
    rhoF1600,
    piF1600,
    chiF1600,
    iotaF1600,
    -- * Permutation
    keccakF1600Round,
    keccakF1600,
    -- * Top entity
    topEntity,
  )
where

import Clash.Prelude
import qualified Constants

--------------------------------------------------------------------------------
-- Round primitives
--------------------------------------------------------------------------------

thetaF1600 :: BitVector 1600 -> BitVector 1600
thetaF1600 bv =
  ifoldl
    ( \acc idx indices11 ->
        let bitOut = fold xor (map (bv !) indices11)
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.theta 6)

chiF1600 :: BitVector 1600 -> BitVector 1600
chiF1600 bv =
  ifoldl
    ( \acc idx (i0, i1, i2) ->
        let bitOut = bv ! i0 `xor` (complement (bv ! i1) .&. bv ! i2)
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.chi 6)

piF1600 :: BitVector 1600 -> BitVector 1600
piF1600 bv =
  ifoldl
    ( \acc idx srcIdx ->
        let bitOut = bv ! srcIdx
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.pi 6)

rhoF1600 :: BitVector 1600 -> BitVector 1600
rhoF1600 bv =
  ifoldl
    ( \acc idx srcIdx ->
        let bitOut = bv ! srcIdx
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.rho 6)

iotaF1600 :: Index 24 -> BitVector 1600 -> BitVector 1600
iotaF1600 roundIdx bv =
  let lane0 = slice d63 d0 bv
      lane0' = lane0 `xor` ($(Constants.iota) !! roundIdx)
      (upper, _) = split bv :: (BitVector 1536, BitVector 64)
   in upper ++# lane0'

--------------------------------------------------------------------------------
-- Permutation
--------------------------------------------------------------------------------

keccakF1600Round :: Index 24 -> BitVector 1600 -> BitVector 1600
keccakF1600Round roundIdx =
  iotaF1600 roundIdx . chiF1600 . piF1600 . rhoF1600 . thetaF1600

keccakF1600 :: BitVector 1600 -> BitVector 1600
keccakF1600 initialState =
  foldl applyRound initialState (indicesI @24)
  where
    applyRound state roundIdx = keccakF1600Round roundIdx state

--------------------------------------------------------------------------------
-- Top entity for hardware synthesis
--------------------------------------------------------------------------------

-- | Top entity for single Keccak-f[1600] round
--
-- This module generates a standalone combinational block for a single round,
-- which can be instantiated by the FSM to execute one round per cycle.
--
-- = Ports
--
-- * ROUND_IDX - 5-bit round index (0-23)
-- * STATE_IN - 1600-bit input state
-- * STATE_OUT - 1600-bit output state after one round
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "KeccakF1600_Round",
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
  Signal System (Index 24, BitVector 1600) ->
  Signal System (BitVector 1600)
topEntity _clk _rst _en input = fmap (uncurry keccakF1600Round) input
