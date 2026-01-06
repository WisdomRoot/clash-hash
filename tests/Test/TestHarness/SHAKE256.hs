module Test.TestHarness.SHAKE256
  ( ShakeTest,
    UpstreamStall (..),
    DownstreamBackpressure (..),
    shake256Params,
    runTest,
    runHardware,
    testLabel
  )
where

import Data.ByteString (ByteString)
import Hash.NonPipelined.SHAKE256 qualified as SHAKE256
import Reference.Hash qualified as Hash
import Test.Hspec (Expectation)
import Test.TestHarness.SHAKECommon
  ( DownstreamBackpressure (..),
    ShakeParams (..),
    ShakeTest,
    UpstreamStall (..),
    runShakeHardware,
    runShakeTest,
    shake256GenConfig
  )
import Test.TestHarness.SHAKECommon qualified as Common
import Prelude

shake256Params :: ShakeParams
shake256Params =
  ShakeParams
    { spBeatsPerBlock = 17,
      spReference = Hash.shake256BS,
      spTopEntity = SHAKE256.topEntity
    }

runTest :: ShakeTest -> Expectation
runTest = runShakeTest shake256Params

runHardware :: ShakeTest -> ByteString
runHardware = runShakeHardware shake256Params

testLabel :: ShakeTest -> String
testLabel = Common.testLabel
