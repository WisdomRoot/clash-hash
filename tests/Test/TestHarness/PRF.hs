{-# LANGUAGE OverloadedStrings #-}

module Test.TestHarness.PRF
  ( prfReference,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import System.FilePath ((</>))
import Test.TestHarness.ExternalReference (callPythonReference)
import Prelude

-- | Call the external PRF reference implementation (FIPS 203, 4.3).
-- Input: eta (2 or 3), 32-byte seed s, and 1-byte b.
-- Output: 64 * eta bytes.
prfReference :: Word8 -> ByteString -> Word8 -> ByteString
prfReference eta s b
  | eta /= 2 && eta /= 3 = error "prfReference: eta must be 2 or 3"
  | BS.length s /= 32 = error "prfReference: s must be 32 bytes"
  | otherwise =
      let input = BS.concat [BS.singleton eta, s, BS.singleton b]
       in callPythonReference ("reference" </> "kyber" </> "prf.py") input
