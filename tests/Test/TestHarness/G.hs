{-# LANGUAGE DataKinds #-}

module Test.TestHarness.G
  ( gReference,
    gReferenceK,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import System.FilePath ((</>))
import Test.TestHarness.ExternalReference (callPythonReference)
import Prelude

-- | Reference implementation of G (SHA3-512 split into two 32-byte outputs)
--
-- G: B* → B^32 × B^32
--
-- Input: Variable-length bytes
-- Output: (rho, sigma) where each is 32 bytes
gReference :: ByteString -> (ByteString, ByteString)
gReference = gReferenceK 2

-- | Reference implementation of G with explicit k (2/3/4).
gReferenceK :: Word8 -> ByteString -> (ByteString, ByteString)
gReferenceK k input =
  let output = callPythonReference ("reference" </> "kyber" </> "g.py") (input <> BS.pack [k])
   in (BS.take 32 output, BS.drop 32 output)
