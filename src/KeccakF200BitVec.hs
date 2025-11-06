module KeccakF200BitVec
  ( chiF200BitVec
  , piF200BitVec
  , iotaF200BitVec
  , topEntity
  ) where

import Clash.Prelude
import Clash.Sized.Internal.BitVector (replaceBit#)
import SHA3internal (sha3_constants, SHA3Constants(..), chi_constants, pi_constants)

type State200 = BitVector 200

-- Pre-computed constants for Keccak-f[200]
chiTriples :: Vec 200 (Index 200, Index 200, Index 200)
chiTriples = chi_constants (sha3_constants @3 @8 @200)

-- Pre-computed constants for Pi permutation
piIndices :: Vec 200 (Index 200)
piIndices = pi_constants (sha3_constants @3 @8 @200)

-- Pre-computed round constant for Iota (round 0, w=8 bits)
iotaRC0 :: Vec 8 Bit
iotaRC0 = takeI $ head $ iota_constants (sha3_constants @3 @8 @200)

-- Chi transformation expressed directly on BitVector
chiF200BitVec :: BitVector 200 -> BitVector 200
chiF200BitVec bv =
  ifoldl
    (\acc idx (i0, i1, i2) ->
       let bitOut = bv ! fromIntegral i0 `xor` (complement (bv ! fromIntegral i1) .&. bv ! fromIntegral i2)
       in  replaceBit# acc (fromIntegral idx) bitOut)
    0
    chiTriples

-- Pi transformation: bit permutation on BitVector
piF200BitVec :: BitVector 200 -> BitVector 200
piF200BitVec bv =
  ifoldl
    (\acc idx srcIdx ->
       let bitOut = bv ! fromIntegral srcIdx
       in  replaceBit# acc (fromIntegral idx) bitOut)
    0
    piIndices

-- Iota transformation: XOR round constant into first lane (bits 0-7)
iotaF200BitVec :: BitVector 200 -> BitVector 200
iotaF200BitVec bv =
  ifoldl
    (\acc bitIdx rcBit ->
       let oldBit = bv ! fromIntegral bitIdx
           newBit = oldBit `xor` rcBit
       in  replaceBit# acc (fromIntegral bitIdx) newBit)
    bv
    iotaRC0

-- Partial Keccak round: Pi followed by Chi followed by Iota
piChiIotaF200BitVec :: BitVector 200 -> BitVector 200
piChiIotaF200BitVec = iotaF200BitVec . chiF200BitVec . piF200BitVec

{-# ANN topEntity
  (Synthesize
    { t_name = "PiChiIota_F200BitVec_OneRound"
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
topEntity = exposeClockResetEnable $ fmap piChiIotaF200BitVec
