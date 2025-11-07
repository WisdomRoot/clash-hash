module KeccakF200
  ( thetaF200
  , rhoF200
  , piF200
  , chiF200
  , iotaF200
  , topEntity
  ) where

import Clash.Prelude
import qualified Constants

type State200 = BitVector 200

-- Pre-computed constants for Theta transformation
thetaIndices :: Vec 200 (Vec 11 (Index 200))
thetaIndices = Constants.theta

-- Pre-computed constants for Chi transformation
chiTriples :: Vec 200 (Index 200, Index 200, Index 200)
chiTriples = Constants.chi

-- Pre-computed constants for Pi permutation
piIndices :: Vec 200 (Index 200)
piIndices = Constants.pi

-- Pre-computed constants for Rho permutation
rhoIndices :: Vec 200 (Index 200)
rhoIndices = Constants.rho

-- Pre-computed round constant for Iota (round 0, w=8 bits)
iotaRC0 :: BitVector 8
iotaRC0 = resize $ head Constants.iota

-- Theta transformation: XOR with column parities
thetaF200 :: BitVector 200 -> BitVector 200
thetaF200 bv =
  ifoldl
    (\acc idx indices11 ->
       let bitOut = fold xor (map (bv !) indices11)
       in  replaceBit idx bitOut acc)
    0
    thetaIndices

-- Chi transformation expressed directly on BitVector
chiF200 :: BitVector 200 -> BitVector 200
chiF200 bv =
  ifoldl
    (\acc idx (i0, i1, i2) ->
       let bitOut = bv ! i0 `xor` (complement (bv ! i1) .&. bv ! i2)
       in  replaceBit idx bitOut acc)
    0
    chiTriples

-- Pi transformation: bit permutation on BitVector
piF200 :: BitVector 200 -> BitVector 200
piF200 bv =
  ifoldl
    (\acc idx srcIdx ->
       let bitOut = bv ! srcIdx
       in  replaceBit idx bitOut acc)
    0
    piIndices

-- Rho transformation: bit permutation on BitVector (lane rotation)
rhoF200 :: BitVector 200 -> BitVector 200
rhoF200 bv =
  ifoldl
    (\acc idx srcIdx ->
       let bitOut = bv ! srcIdx
       in  replaceBit idx bitOut acc)
    0
    rhoIndices

-- Iota transformation: XOR round constant into first lane (bits 0-7)
iotaF200 :: BitVector 200 -> BitVector 200
iotaF200 bv =
  let lane0 = slice d7 d0 bv       -- Extract first 8 bits (lane 0)
      lane0' = lane0 `xor` iotaRC0  -- Single 8-bit XOR operation
  in  slice d199 d8 bv ++# lane0' -- Replace bits 0-7 with result

-- Complete Keccak-f[200] round: Theta, Rho, Pi, Chi, Iota
keccakF200Round :: BitVector 200 -> BitVector 200
keccakF200Round = iotaF200 . chiF200 . piF200 . rhoF200 . thetaF200

{-# ANN topEntity
  (Synthesize
    { t_name = "KeccakF200_OneRound"
    , t_inputs = [ PortName "CLK"
                 , PortName "RST"
                 , PortName "EN"
                 , PortName "DIN"
                 ]
    , t_output = PortName "DOUT"
    }) #-}
{-# OPAQUE topEntity #-}

topEntity :: Clock System
          -> Reset System
          -> Enable System
          -> Signal System State200
          -> Signal System State200
topEntity = exposeClockResetEnable $ fmap keccakF200Round
