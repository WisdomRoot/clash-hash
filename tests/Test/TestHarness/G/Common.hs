module Test.TestHarness.G.Common
  ( gReference,
    gReferenceK,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import System.FilePath ((</>))
import Test.TestHarness.ExternalReference (callPythonReference)
import Prelude qualified as P

gReference :: ByteString -> (ByteString, ByteString)
gReference = gReferenceK 2

gReferenceK :: Word8 -> ByteString -> (ByteString, ByteString)
gReferenceK k input =
  let output = callPythonReference ("reference" </> "kyber" </> "g.py") (input P.<> BS.pack [k])
   in (BS.take 32 output, BS.drop 32 output)
