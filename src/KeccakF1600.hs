{-# LANGUAGE TypeApplications #-}

module KeccakF1600
  ( thetaF1600,
    rhoF1600,
    piF1600,
    chiF1600,
    iotaF1600,
    keccakF1600Round,
    keccakF1600,
    topEntity,
  )
where

import Clash.Prelude
import qualified Constants

-- Theta transformation: XOR with column parities
thetaF1600 :: BitVector 1600 -> BitVector 1600
thetaF1600 bv =
  ifoldl
    ( \acc idx indices11 ->
        let bitOut = fold xor (map (bv !) indices11)
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.theta 6)

-- Chi transformation expressed directly on BitVector
chiF1600 :: BitVector 1600 -> BitVector 1600
chiF1600 bv =
  ifoldl
    ( \acc idx (i0, i1, i2) ->
        let bitOut = bv ! i0 `xor` (complement (bv ! i1) .&. bv ! i2)
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.chi 6)

-- Pi transformation: bit permutation on BitVector
piF1600 :: BitVector 1600 -> BitVector 1600
piF1600 bv =
  ifoldl
    ( \acc idx srcIdx ->
        let bitOut = bv ! srcIdx
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.pi 6)

-- Rho transformation: bit permutation on BitVector (lane rotation)
rhoF1600 :: BitVector 1600 -> BitVector 1600
rhoF1600 bv =
  ifoldl
    ( \acc idx srcIdx ->
        let bitOut = bv ! srcIdx
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.rho 6)

-- Iota transformation: XOR round constant into first lane
iotaF1600 :: Index 24 -> BitVector 1600 -> BitVector 1600
iotaF1600 roundIdx bv =
  let (upperBits :: BitVector 1536, lane0 :: BitVector 64) = split bv -- Split into bits[1599:64] and bits[63:0]
      lane0' = lane0 `xor` ($(Constants.iota) !! roundIdx) -- XOR with selected round constant (no truncation needed)
   in upperBits ++# lane0' -- Concatenate: bits[1599:64] ++ bits[63:0]

-- Complete Keccak-f[1600] round: Theta, Rho, Pi, Chi, Iota
keccakF1600Round :: Index 24 -> BitVector 1600 -> BitVector 1600
keccakF1600Round roundIdx =
  iotaF1600 roundIdx . chiF1600 . piF1600 . rhoF1600 . thetaF1600

-- | Full Keccak-f[1600] permutation: 24 rounds (12 + 2*l where l=6)
-- Applies all rounds in sequence using the round constants
keccakF1600 :: BitVector 1600 -> BitVector 1600
keccakF1600 initialState =
  foldl applyRound initialState (indicesI @24)
  where
    applyRound state roundIdx = keccakF1600Round roundIdx state

-- | Top entity for synthesis: combinational Keccak-f[1600] permutation
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "KeccakF1600_AllRounds",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "DIN"
          ],
        t_output = PortName "DOUT"
      }
  )
  #-}
{-# OPAQUE topEntity #-}
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector 1600) ->
  Signal System (BitVector 1600)
topEntity = exposeClockResetEnable $ fmap keccakF1600
