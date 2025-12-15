{-# LANGUAGE TypeApplications #-}

module Permutation.P2
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
import Permutation.Constants qualified as Constants

--------------------------------------------------------------------------------
-- Round primitives
--------------------------------------------------------------------------------

-- Helper: reverse index for bit ordering
rev :: Index 1600 -> Index 1600
rev i = 1599 - i

-- Theta transformation: XOR with column parities
thetaF1600 :: Vec 1600 Bit -> Vec 1600 Bit
thetaF1600 bv = map (fold xor . map (bv !)) $(Constants.theta 6)

-- Chi transformation
chiF1600 :: Vec 1600 Bit -> Vec 1600 Bit
chiF1600 bv = map (\(i0, i1, i2) -> bv ! rev i0 `xor` (complement (bv ! rev i1) .&. bv ! rev i2)) $(Constants.chi 6)

-- Pi transformation: bit permutation
piF1600 :: Vec 1600 Bit -> Vec 1600 Bit
piF1600 bv = map (bv !) Constants.pi6

-- Rho transformation: bit permutation (lane rotation)
rhoF1600 :: Vec 1600 Bit -> Vec 1600 Bit
rhoF1600 bv = map (bv !) Constants.rho6

-- Iota transformation: XOR lane 0 with round constant
-- Matches SHA3internal.iota implementation exactly
iotaF1600 :: Index 24 -> Vec 1600 Bit -> Vec 1600 Bit
iotaF1600 roundIdx v =
  let lanes :: Vec 25 (Vec 64 Bit)
      lanes = unconcatI v
      rc :: Vec 64 Bit
      rc = $(Constants.iota) !! roundIdx
      lane0' = zipWith xor (head lanes) rc
   in concat (lane0' :> tail lanes)

--------------------------------------------------------------------------------
-- Permutation
--------------------------------------------------------------------------------

-- Complete Keccak-f[1600] round: Theta, Rho, Pi, Chi, Iota
-- OPAQUE ensures Clash treats this as a black box:
--   - No inlining or specialization (keeps single definition)
--   - Emits separate component once, wired to all callers
--   - Enforces module boundary for potential blackbox override
{-# OPAQUE keccakF1600Round #-}
keccakF1600Round :: Index 24 -> BitVector 1600 -> BitVector 1600
keccakF1600Round roundIdx = pack . iotaF1600 roundIdx . chiF1600 . piF1600 . rhoF1600 . thetaF1600 . unpack

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
      { t_name = "KeccakF1600_P2",
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
topEntity _clk _rst _en = fmap (uncurry keccakF1600Round)
