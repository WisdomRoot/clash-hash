module Test.TestHarness.SHAKE128
  ( ShakeTest,
    runTest,
    runHardware,
    testLabel
  )
where

import Data.ByteString (ByteString)
import Hash.NonPipelined.SHAKE128 qualified as SHAKE128
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

shake128Params :: ShakeParams
shake128Params =
  ShakeParams
    { spBeatsPerBlock = 21,
      spReference = Crypton.shake128,
      spTopEntity = SHAKE128.topEntity
    }

runTest :: ShakeTest -> Expectation
runTest = runShakeTest shake128Params

runHardware :: ShakeTest -> ByteString
runHardware = runShakeHardware shake128Params

testLabel :: ShakeTest -> String
testLabel = Common.testLabel
