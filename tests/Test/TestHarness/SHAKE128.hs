{-# LANGUAGE TypeApplications #-}

module Test.TestHarness.SHAKE128
  ( ShakeTest,
    UpstreamStall (..),
    DownstreamBackpressure (..),
    shake128Params,
    referenceShake128BS,
    runTest,
    runHardware,
    testLabel
  )
where

import Data.ByteString (ByteString)
import Hash.NonPipelined.SHAKE128 qualified as SHAKE128
import Reference.Hash qualified as Hash
import Reference.SHA3 qualified as SHA3
import Test.Hspec (Expectation)
import Test.TestHarness.SHAKECommon
  ( DownstreamBackpressure (..),
    ShakeParams (..),
    ShakeTest,
    UpstreamStall (..),
    runShakeHardware,
    runShakeTest,
    shake128GenConfig
  )
import Test.TestHarness.SHAKECommon qualified as Common
import Prelude

shake128Params :: ShakeParams
shake128Params =
  ShakeParams
    { spBeatsPerBlock = 21,
      spReference = referenceShake128BS,
      spTopEntity = SHAKE128.topEntity
    }

referenceShake128BS :: Int -> ByteString -> ByteString
referenceShake128BS outputBytes input =
  let inputBits = Hash.bsToBitList input
      outputBits = outputBytes * 8
      domain = [1, 1, 1, 1]
      resultBits = Hash.sponge @1600 @1344 SHA3.keccakf outputBits (inputBits ++ domain)
   in Hash.bitListToBS resultBits

runTest :: ShakeTest -> Expectation
runTest = runShakeTest shake128Params

runHardware :: ShakeTest -> ByteString
runHardware = runShakeHardware shake128Params

testLabel :: ShakeTest -> String
testLabel = Common.testLabel
