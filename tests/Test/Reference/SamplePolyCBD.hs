{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Reference.SamplePolyCBD
  ( run,
  )
where

import Clash.Prelude (BitVector, Unsigned, pack)
import Component.PRF.Common (Eta (..))
import Data.Bits (setBit, testBit, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word16, Word8)
import System.FilePath ((</>))
import Test.TestHarness.ExternalReference (callPythonReference)
import Prelude qualified as P

bv264ToBS :: BitVector 264 -> ByteString
bv264ToBS bv = BS.pack [byteAt i | i <- [0 .. 32]]
  where
    byteAt :: P.Int -> Word8
    byteAt byteIdx =
      let base = byteIdx P.* 8
       in P.foldl
            (\acc bitIdx -> if testBit bv (base P.+ bitIdx) then setBit acc bitIdx else acc)
            (0 :: Word8)
            [0 .. 7]

run :: Eta -> BitVector 264 -> [BitVector 12]
run eta msg =
  let bs = bv264ToBS msg
      padded = BS.take 33 (bs P.<> BS.replicate 33 0)
      seed = BS.take 32 padded
      b = BS.last padded
      etaByte = case eta of
        Eta2 -> 2
        Eta3 -> 3
      input = BS.concat [BS.singleton etaByte, seed, BS.singleton b]
      output = callPythonReference ("reference" </> "kyber" </> "prf_cbd.py") input
      coeffs = unpackPython512Bytes output
      coeffsMasked = P.map (.&. 0x0FFF) coeffs
   in P.map (\w -> pack (P.fromIntegral w :: Unsigned 12)) coeffsMasked

unpackPython512Bytes :: ByteString -> [Word16]
unpackPython512Bytes bs = go (BS.unpack bs)
  where
    go (lo : hi : rest) =
      let val = P.fromIntegral lo P.+ 256 P.* P.fromIntegral hi
       in val : go rest
    go [] = []
    go _ = P.error "unpackPython512Bytes: Expected 512 bytes (little-endian uint16)"
