{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Reference.SamplePolyCBD
  ( run,
  )
where

import Clash.Prelude (BitVector, Unsigned, pack)
import Component.PRF.Common (Eta (..))
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word16)
import System.FilePath ((</>))
import Stream (bvToBS)
import Test.TestHarness.ExternalReference (callPythonReference)
import Prelude qualified as P

run :: Eta -> BitVector 264 -> [BitVector 12]
run eta msg =
  let bs = bvToBS 33 msg
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
