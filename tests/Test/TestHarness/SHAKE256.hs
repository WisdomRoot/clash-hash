module Test.TestHarness.SHAKE256
  ( ShakeTest,
    runTest,
    runHardware,
    testLabel
  )
where

import Data.ByteString (ByteString)
import Hash.NonPipelined.SHAKE256 qualified as SHAKE256
import Reference.Crypton qualified as Crypton
import Test.Hspec (Expectation)
import Test.TestHarness.SHAKECommon
  ( ShakeParams (..),
    ShakeTest,
    runShakeHardware,
    runShakeTest
  )
import Test.TestHarness.SHAKECommon qualified as Common
import Prelude

shake256Params :: ShakeParams
shake256Params =
  ShakeParams
    { spBeatsPerBlock = 17,
      spReference = Crypton.shake256,
      spTopEntity = SHAKE256.topEntity
    }

runTest :: ShakeTest -> Expectation
runTest = runShakeTest shake256Params

runHardware :: ShakeTest -> ByteString
runHardware = runShakeHardware shake256Params

testLabel :: ShakeTest -> String
testLabel = Common.testLabel
