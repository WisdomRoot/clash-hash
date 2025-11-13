{-# LANGUAGE TypeApplications #-}

module KeccakF200.Permutation
  ( -- * Round primitives
    thetaF200,
    rhoF200,
    piF200,
    chiF200,
    iotaF200,
    -- * Permutation
    keccakF200Round,
    keccakF200,
    -- * Top entity
    topEntity,
  )
where

import Clash.Prelude
import qualified Constants

-- Theta transformation: XOR with column parities
thetaF200 :: BitVector 200 -> BitVector 200
thetaF200 bv =
  ifoldl
    ( \acc idx indices11 ->
        let bitOut = fold xor (map (bv !) indices11)
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.theta 3)

-- Chi transformation expressed directly on BitVector
chiF200 :: BitVector 200 -> BitVector 200
chiF200 bv =
  ifoldl
    ( \acc idx (i0, i1, i2) ->
        let bitOut = bv ! i0 `xor` (complement (bv ! i1) .&. bv ! i2)
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.chi 3)

-- Pi transformation: bit permutation on BitVector
piF200 :: BitVector 200 -> BitVector 200
piF200 bv =
  ifoldl
    ( \acc idx srcIdx ->
        let bitOut = bv ! srcIdx
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.pi 3)

-- Rho transformation: bit permutation on BitVector (lane rotation)
rhoF200 :: BitVector 200 -> BitVector 200
rhoF200 bv =
  ifoldl
    ( \acc idx srcIdx ->
        let bitOut = bv ! srcIdx
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.rho 3)

iotaF200 :: Index 24 -> BitVector 200 -> BitVector 200
iotaF200 roundIdx bv =
  let lane0 = slice d7 d0 bv -- Extract first 8 bits (lane 0)
      lane0' = lane0 `xor` truncateB ($(Constants.iota) !! roundIdx) -- XOR with selected round constant
   in slice d199 d8 bv ++# lane0' -- Replace bits 0-7 with result

-- Complete Keccak-f[200] round: Theta, Rho, Pi, Chi, Iota
keccakF200Round :: Index 24 -> BitVector 200 -> BitVector 200
keccakF200Round roundIdx =
  iotaF200 roundIdx . chiF200 . piF200 . rhoF200 . thetaF200

-- | Full Keccak-f[200] permutation: 18 rounds (12 + 2*l where l=3)
-- Applies all rounds in sequence using the round constants
keccakF200 :: BitVector 200 -> BitVector 200
keccakF200 initialState =
  foldl applyRound initialState (indicesI @18)
  where
    applyRound state roundIdx = keccakF200Round (resize roundIdx) state

--------------------------------------------------------------------------------
-- Top entity for hardware synthesis
--------------------------------------------------------------------------------

-- | Top entity for single Keccak-f[200] round
--
-- This module generates a standalone combinational block for a single round,
-- which can be instantiated by the FSM to execute one round per cycle.
--
-- = Ports
--
-- * ROUND_IDX - 5-bit round index (0-23, though only 0-17 are used)
-- * STATE_IN - 200-bit input state
-- * STATE_OUT - 200-bit output state after one round
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "KeccakF200_Round",
        t_inputs =
          [ PortName "ROUND_IDX",
            PortName "STATE_IN"
          ],
        t_output = PortName "STATE_OUT"
      }
  )
  #-}
{-# OPAQUE topEntity #-}
topEntity :: (Index 24, BitVector 200) -> BitVector 200
topEntity (roundIdx, state) = keccakF200Round roundIdx state
