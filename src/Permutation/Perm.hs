{-# LANGUAGE TypeApplications #-}

module Permutation.Perm
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

-- Element-wise XOR for bit vectors
-- NOTE: defining it as an left-associative operator would generate inefficient verilog!!
(^^^) :: Vec n Bit -> Vec n Bit -> Vec n Bit
(^^^) = zipWith xor

infixr 7 ^^^ -- infixl 7 would generate inefficient verilog!!


rotateRight1 :: KnownNat n => Vec n a -> Vec n a
rotateRight1 x = rotateRightS x d1

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

      -- Helper: get lane at index i
      lane :: Index 25 -> Vec 64 Bit
      lane i = state !! i

      -- Stage 1: Compute column parity for each column: XOR all 5 lanes
      parity0 = lane 0 ^^^ lane 5 ^^^ lane 10 ^^^ lane 15 ^^^ lane 20
      parity1 = lane 1 ^^^ lane 6 ^^^ lane 11 ^^^ lane 16 ^^^ lane 21
      parity2 = lane 2 ^^^ lane 7 ^^^ lane 12 ^^^ lane 17 ^^^ lane 22
      parity3 = lane 3 ^^^ lane 8 ^^^ lane 13 ^^^ lane 18 ^^^ lane 23
      parity4 = lane 4 ^^^ lane 9 ^^^ lane 14 ^^^ lane 19 ^^^ lane 24

      -- Stage 2: Apply theta to each lane
      -- For each lane, we need to XOR with two column parities
      -- output[y][x][z] = state[y][x][z] XOR parity[(x-1) mod 5][z] XOR parity[(x+1) mod 5][(z-1) mod 64]
      applyX :: Index 5 -> Vec 64 Bit -> Vec 64 Bit
      applyX 0 x = x ^^^ parity4 ^^^ rotateRight1 parity1
      applyX 1 x = x ^^^ parity0 ^^^ rotateRight1 parity2
      applyX 2 x = x ^^^ parity1 ^^^ rotateRight1 parity3
      applyX 3 x = x ^^^ parity2 ^^^ rotateRight1 parity4
      applyX _ x = x ^^^ parity3 ^^^ rotateRight1 parity0

      -- Apply to all 25 lanes based on their column (i mod 5)
      outputState :: Vec 25 (Vec 64 Bit)
      outputState = imap (applyX . resize . (`mod` 5)) state

   in concat outputState

-- Chi transformation
chiF1600 :: Vec 1600 Bit -> Vec 1600 Bit
chiF1600 bv = map (\(i0, i1, i2) -> bv ! rev i0 `xor` (complement (bv ! rev i1) .&. bv ! rev i2)) $(Constants.chiReversed 6)

-- Pi transformation: bit permutation
piF1600 :: Vec 1600 Bit -> Vec 1600 Bit
piF1600 bv = map ((bv !) . rev) Constants.pi6

-- Rho transformation: bit permutation (lane rotation)
rhoF1600 :: Vec 1600 Bit -> Vec 1600 Bit
rhoF1600 bv = map (bv !) Constants.rho6Reversed

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
      { t_name = "KeccakF1600_Perm",
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
