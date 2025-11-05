module ChiF200BitVec
  ( chiF200BitVec
  , topEntity
  ) where

import Clash.Prelude
import Clash.Sized.Internal.BitVector (replaceBit#)
import SHA3internal (sha3_constants, chi_constants)

type State200 = BitVector 200

-- Pre-computed constants for Keccak-f[200]
chiTriples :: Vec 200 (Index 200, Index 200, Index 200)
chiTriples = chi_constants (sha3_constants @3 @8 @200)

-- Chi transformation expressed directly on BitVector
chiF200BitVec :: BitVector 200 -> BitVector 200
chiF200BitVec bv =
  foldl
    (\acc (idx, (i0, i1, i2)) ->
       let bitOut = bv ! fromIntegral i0 `xor` (complement (bv ! fromIntegral i1) .&. bv ! fromIntegral i2)
       in  replaceBit# acc (fromIntegral idx) bitOut)
    0
    (zip indicesI chiTriples)

{-# ANN topEntity
  (Synthesize
    { t_name = "ChiF200BitVec_OneRound"
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
topEntity = exposeClockResetEnable $ fmap chiF200BitVec
