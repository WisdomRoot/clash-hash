module Test.TestHarness.SHA3256Normal
  ( SHA3256NormalTest,
    sha3256NormalGen,
    runTest,
    runHardware,
    testLabel
  )
where

import Data.ByteString (ByteString)
import Hash.NonPipelined.SHA3256Normal qualified as SHA3256Normal
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
import Test.TestHarness.StreamCommon (bitListToBSHW, bsToBitListHW)
import Prelude

type SHA3256NormalTest = ShakeTest

sha3256NormalParams :: ShakeParams
sha3256NormalParams =
  ShakeParams
    { spBeatsPerBlock = 17,
      spReference = \_ msg -> reverseBitsAll (Crypton.sha3 msg),
      spTopEntity = SHA3256Normal.topEntity
    }

sha3256NormalGenConfig :: ShakeGenConfig
sha3256NormalGenConfig =
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

sha3256NormalGen :: Gen SHA3256NormalTest
sha3256NormalGen = genShakeTest sha3256NormalGenConfig

runTest :: SHA3256NormalTest -> Expectation
runTest = runShakeTest sha3256NormalParams

runHardware :: SHA3256NormalTest -> ByteString
runHardware = runShakeHardware sha3256NormalParams

testLabel :: SHA3256NormalTest -> String
testLabel = Common.testLabel

reverseBitsAll :: ByteString -> ByteString
reverseBitsAll bs =
  let bits = bsToBitListHW bs
   in bitListToBSHW (reverse bits)
