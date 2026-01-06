{-# LANGUAGE TypeApplications #-}

module Test.TestHarness.SHA3256
  ( SHA3256Test,
    UpstreamStall (..),
    DownstreamBackpressure (..),
    sha3256Params,
    sha3256Gen,
    runTest,
    runHardware,
    testLabel,
    msg1024,
    msg1088,
    msg1600,
    msg3200,
    stallPatternSimple,
    stallPatternModerate,
    stallPatternAggressive,
    backpressurePatternSimple,
    backpressurePatternModerate,
    backpressurePatternAggressive,
    makeBasicTest,
    makeStallTest,
    makeBackpressureTest,
    makeCombinedTest
  )
where

import Data.ByteString (ByteString)
import Hash.NonPipelined.SHA3256 qualified as SHA3256
import Reference.Hash qualified as Hash
import Test.Hspec (Expectation)
import Test.QuickCheck (Gen)
import Test.TestHarness.SHAKECommon
  ( DownstreamBackpressure (..),
    ShakeGenConfig (..),
    ShakeParams (..),
    ShakeTest (..),
    UpstreamStall (..),
    defaultShakeGenConfig,
    genShakeTest,
    runShakeHardware,
    runShakeTest
  )
import Test.TestHarness.SHAKECommon qualified as Common
import Test.TestHarness.SHAKESamples qualified as Samples
import Prelude

type SHA3256Test = ShakeTest

sha3256Params :: ShakeParams
sha3256Params =
  ShakeParams
    { spBeatsPerBlock = 17,
      spReference = \_ -> Hash.sha3_256BS,
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

msg1024, msg1088, msg1600, msg3200 :: ByteString
msg1024 = Samples.msg1024
msg1088 = Samples.msg1088
msg1600 = Samples.msg1600
msg3200 = Samples.msg3200

stallPatternSimple, stallPatternModerate, stallPatternAggressive :: [Bool]
stallPatternSimple = Samples.stallPatternSimple
stallPatternModerate = Samples.stallPatternModerate
stallPatternAggressive = Samples.stallPatternAggressive

backpressurePatternSimple, backpressurePatternModerate, backpressurePatternAggressive :: [Bool]
backpressurePatternSimple = Samples.backpressurePatternSimple
backpressurePatternModerate = Samples.backpressurePatternModerate
backpressurePatternAggressive = Samples.backpressurePatternAggressive

makeBasicTest :: ByteString -> SHA3256Test
makeBasicTest input = Common.makeBasicTest input 32

makeStallTest :: ByteString -> [Bool] -> SHA3256Test
makeStallTest input pattern = Common.makeStallTest input 32 pattern

makeBackpressureTest :: ByteString -> [Bool] -> SHA3256Test
makeBackpressureTest input pattern = Common.makeBackpressureTest input 32 pattern

makeCombinedTest :: ByteString -> [Bool] -> [Bool] -> SHA3256Test
makeCombinedTest input stallPattern backpressurePattern =
  Common.makeCombinedTest input 32 stallPattern backpressurePattern
