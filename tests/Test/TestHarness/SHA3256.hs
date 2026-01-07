module Test.TestHarness.SHA3256
  ( SHA3256Test,
    sha3256Gen,
    runTest,
    runHardware,
    testLabel
  )
where

import Data.ByteString (ByteString)
import Hash.NonPipelined.SHA3256 qualified as SHA3256
import Reference.Crypton qualified as Crypton
import Test.Hspec (Expectation)
import Test.QuickCheck (Gen)
import Test.TestHarness.SHAKECommon
  ( ShakeGenConfig (..),
    ShakeParams (..),
    ShakeTest (..),
    defaultShakeGenConfig,
    genShakeTest,
    runShakeHardware,
    runShakeTest
  )
import Test.TestHarness.SHAKECommon qualified as Common
import Prelude

type SHA3256Test = ShakeTest

sha3256Params :: ShakeParams
sha3256Params =
  ShakeParams
    { spBeatsPerBlock = 17,
      spReference = const Crypton.sha3,
      spTopEntity = SHA3256.topEntity
    }

sha3256GenConfig :: ShakeGenConfig
sha3256GenConfig =
  defaultShakeGenConfig
    { sgBeatOptions =
        [ (2, 1),
          (1, 2),
          (1, 16),
          (2, 17),
          (1, 18),
          (2, 25),
          (1, 34),
          (1, 51)
        ],
      sgOutputOptions = [(1, 32)]
    }

sha3256Gen :: Gen SHA3256Test
sha3256Gen = genShakeTest sha3256GenConfig

runTest :: SHA3256Test -> Expectation
runTest = runShakeTest sha3256Params

runHardware :: SHA3256Test -> ByteString
runHardware = runShakeHardware sha3256Params

testLabel :: SHA3256Test -> String
testLabel = Common.testLabel
