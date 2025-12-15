{-# LANGUAGE TypeApplications #-}

module Permutation.P3
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
-- Step 1: Just compute column parities (5 lanes of 64 bits = 320 bits)
thetaF1600 :: Vec 1600 Bit -> Vec 1600 Bit
thetaF1600 bv =
  let -- Unpack to 5×5×64: Vec 5 (Vec 5 (Vec 64 Bit))
      state :: Vec 25 (Vec 64 Bit)
      state = unconcatI bv

      -- Extract the 5 lanes for each column (x=0,1,2,3,4)
      -- Each column has 5 lanes at positions: y*5+x where y=0..4
      col0 = state !! 0 :> state !! 5 :> state !! 10 :> state !! 15 :> state !! 20 :> Nil
      col1 = state !! 1 :> state !! 6 :> state !! 11 :> state !! 16 :> state !! 21 :> Nil
      col2 = state !! 2 :> state !! 7 :> state !! 12 :> state !! 17 :> state !! 22 :> Nil
      col3 = state !! 3 :> state !! 8 :> state !! 13 :> state !! 18 :> state !! 23 :> Nil
      col4 = state !! 4 :> state !! 9 :> state !! 14 :> state !! 19 :> state !! 24 :> Nil

      -- Stage 1: Compute column parity for each column: XOR all 5 lanes
      parity0 = fold (zipWith xor) col0
      parity1 = fold (zipWith xor) col1
      parity2 = fold (zipWith xor) col2
      parity3 = fold (zipWith xor) col3
      parity4 = fold (zipWith xor) col4

      -- Stage 2: Apply theta to each lane
      -- For each lane, we need to XOR with two column parities
      -- output[y][x][z] = state[y][x][z] XOR parity[(x-1) mod 5][z] XOR parity[(x+1) mod 5][(z-1) mod 64]

      -- Helper: rotate a 64-bit lane right by 1 position (z-1 mod 64)
      rotateRight1 :: Vec 64 Bit -> Vec 64 Bit
      rotateRight1 v = last v :> init v

      -- Pre-compute the rotated parities (only 5 of them)
      parity0Rot = rotateRight1 parity0
      parity1Rot = rotateRight1 parity1
      parity2Rot = rotateRight1 parity2
      parity3Rot = rotateRight1 parity3
      parity4Rot = rotateRight1 parity4

      -- Apply theta for x=0: uses parity4 and rotated parity1
      applyX0 lane = zipWith xor lane (zipWith xor parity4 parity1Rot)
      -- Apply theta for x=1: uses parity0 and rotated parity2
      applyX1 lane = zipWith xor lane (zipWith xor parity0 parity2Rot)
      -- Apply theta for x=2: uses parity1 and rotated parity3
      applyX2 lane = zipWith xor lane (zipWith xor parity1 parity3Rot)
      -- Apply theta for x=3: uses parity2 and rotated parity4
      applyX3 lane = zipWith xor lane (zipWith xor parity2 parity4Rot)
      -- Apply theta for x=4: uses parity3 and rotated parity0
      applyX4 lane = zipWith xor lane (zipWith xor parity3 parity0Rot)

      -- Apply to all 25 lanes based on their column (i mod 5)
      outputState :: Vec 25 (Vec 64 Bit)
      outputState = imap (\i lane ->
        case resize (i `mod` 5) :: Index 5 of
          0 -> applyX0 lane
          1 -> applyX1 lane
          2 -> applyX2 lane
          3 -> applyX3 lane
          _ -> applyX4 lane
        ) state

   in concat outputState

-- Chi transformation
chiF1600 :: Vec 1600 Bit -> Vec 1600 Bit
chiF1600 bv = map (\(i0, i1, i2) -> bv ! rev i0 `xor` (complement (bv ! rev i1) .&. bv ! rev i2)) $(Constants.chiReversed 6)

-- Pi transformation: bit permutation
piF1600 :: Vec 1600 Bit -> Vec 1600 Bit
piF1600 bv = map ((bv !) . rev) Constants.pi6Reversed

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
      { t_name = "KeccakF1600_P3",
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
