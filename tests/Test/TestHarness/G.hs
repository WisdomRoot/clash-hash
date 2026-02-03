{-# LANGUAGE DataKinds #-}

module Test.TestHarness.G
  ( gReference,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
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
gReference input =
  let output = callPythonReference ("reference" </> "kyber" </> "g.py") input
   in (BS.take 32 output, BS.drop 32 output)
