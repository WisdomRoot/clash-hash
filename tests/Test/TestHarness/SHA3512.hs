module Test.TestHarness.SHA3512
  ( SHA3512Test,
    sha3512Gen,
    runTest,
    runHardware,
    testLabel
  )
where

import Data.ByteString (ByteString)
import Hash.NonPipelined.SHA3512 qualified as SHA3512
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

type SHA3512Test = ShakeTest

sha3512Params :: ShakeParams
sha3512Params =
  ShakeParams
    { spBeatsPerBlock = 9,
      spReference = const Crypton.sha3_512,
      spTopEntity = SHA3512.topEntity
    }

sha3512GenConfig :: ShakeGenConfig
sha3512GenConfig =
  defaultShakeGenConfig
    { sgBeatOptions =
        [ (2, 1),
          (1, 2),
          (1, 8),
          (2, 9),
          (1, 10),
          (2, 17),
          (1, 18),
          (1, 25)
        ],
      sgOutputOptions = [(1, 64)]
    }

sha3512Gen :: Gen SHA3512Test
sha3512Gen = genShakeTest sha3512GenConfig

runTest :: SHA3512Test -> Expectation
runTest = runShakeTest sha3512Params

runHardware :: SHA3512Test -> ByteString
runHardware = runShakeHardware sha3512Params

testLabel :: SHA3512Test -> String
testLabel = Common.testLabel
