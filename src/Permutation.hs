{-# LANGUAGE TypeApplications #-}

module Permutation
  ( -- * Round primitives
    thetaF1600Reversed,
    rhoF1600Reversed,
    piF1600Reversed,
    chiF1600Reversed,
    iotaF1600Reversed,

    -- * Permutation
    keccakF1600,
    keccakF160024,

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

rotateRight1 :: (KnownNat n) => Vec n a -> Vec n a
rotateRight1 x = rotateRightS x d1

--------------------------------------------------------------------------------
-- Round primitives
--------------------------------------------------------------------------------

-- Theta transformation: XOR with column parities
-- Step 1: Just compute column parities (5 lanes of 64 bits = 320 bits)
thetaF1600Reversed :: Vec 1600 Bit -> Vec 1600 Bit
thetaF1600Reversed bv =
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
chiF1600Reversed :: Vec 1600 Bit -> Vec 1600 Bit
chiF1600Reversed bv = map (\(i0, i1, i2) -> bv ! i0 `xor` (complement (bv ! i1) .&. bv ! i2)) Constants.chi6Reversed

-- Pi transformation: bit permutation
piF1600Reversed :: Vec 1600 Bit -> Vec 1600 Bit
piF1600Reversed bv = map (bv !) Constants.pi6Reversed

-- Rho transformation: bit permutation (lane rotation)
rhoF1600Reversed :: Vec 1600 Bit -> Vec 1600 Bit
rhoF1600Reversed bv = map (bv !) Constants.rho6Reversed

-- Iota transformation: XOR lane 0 with round constant (pure BitVector version)
iotaF1600Reversed :: Index 24 -> BitVector 1600 -> BitVector 1600
iotaF1600Reversed roundIdx bv =
  let lane0 :: BitVector 64
      lane0 = slice (SNat @1599) (SNat @1536) bv
      rc :: BitVector 64
      rc = pack ($(Constants.iota) !! roundIdx)
      lane0' = lane0 `xor` rc
   in setSlice (SNat @1599) (SNat @1536) lane0' bv

--------------------------------------------------------------------------------
-- Permutation
--------------------------------------------------------------------------------

-- Complete Keccak-f[1600] round: Theta, Rho, Pi, Chi, Iota
-- OPAQUE ensures Clash treats this as a black box:
--   - No inlining or specialization (keeps single definition)
--   - Emits separate component once, wired to all callers
--   - Enforces module boundary for potential blackbox override
{-# OPAQUE keccakF1600 #-}
keccakF1600 :: Index 24 -> BitVector 1600 -> BitVector 1600
keccakF1600 roundIdx =
  iotaF1600Reversed roundIdx
    . pack
    . chiF1600Reversed
    . piF1600Reversed
    . rhoF1600Reversed
    . thetaF1600Reversed
    . unpack

keccakF160024 :: BitVector 1600 -> BitVector 1600
keccakF160024 initialState =
  foldl applyRound initialState (indicesI @24)
  where
    applyRound state roundIdx = keccakF1600 roundIdx state

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
topEntity _clk _rst _en = fmap (uncurry keccakF1600)
