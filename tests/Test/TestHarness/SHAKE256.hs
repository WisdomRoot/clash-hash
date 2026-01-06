module Test.TestHarness.SHAKE256
  ( SHAKE256Test (..),
    UpstreamStall (..),
    DownstreamBackpressure (..),
    runTest,
    runHardware,
    testLabel,
    makeBasicTest,
    makeVariableOutputTest,
    makeStallTest,
    makeBackpressureTest,
    makeCombinedTest
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Hash.NonPipelined.SHAKE256 qualified as SHAKE256
import Reference.Hash qualified as Hash
import Prelude
import Test.Hspec (Expectation)
import Test.QuickCheck (Arbitrary (..), frequency, vector)
import Test.TestHarness.SHAKECommon
  ( DownstreamBackpressure (..),
    ShakeParams (..),
    ShakeTest (..),
    UpstreamStall (..),
    runShakeHardware,
    runShakeTest
  )
import Test.TestHarness.SHAKECommon qualified as Common

newtype SHAKE256Test = SHAKE256Test { getShake256Test :: ShakeTest }
  deriving stock (Show)

toShakeTest256 :: SHAKE256Test -> ShakeTest
toShakeTest256 (SHAKE256Test t) = t

fromShakeTest256 :: ShakeTest -> SHAKE256Test
fromShakeTest256 = SHAKE256Test
shake256Params :: ShakeParams
shake256Params =
  ShakeParams
    { spBeatsPerBlock = 17,
      spReference = Hash.shake256BS,
      spTopEntity = SHAKE256.topEntity
    }

runTest :: SHAKE256Test -> Expectation
runTest = runShakeTest shake256Params . toShakeTest256

runHardware :: SHAKE256Test -> ByteString
runHardware = runShakeHardware shake256Params . toShakeTest256

testLabel :: SHAKE256Test -> String
testLabel = Common.testLabel . toShakeTest256

instance Arbitrary SHAKE256Test where
  arbitrary = do
    beatCount <-
      frequency
        [ (1, pure 0),
          (2, pure 1),
          (1, pure 2),
          (1, pure 16),
          (2, pure 17),
          (1, pure 18),
          (2, pure 25),
          (1, pure 34),
          (1, pure 51)
        ]
    messageBytes <- BS.pack <$> vector (beatCount * 8)
    outputBytes <-
      frequency
        [ (2, pure 8),
          (1, pure 16),
          (2, pure 32),
          (1, pure 64),
          (1, pure 128),
          (1, pure 256)
        ]
    upstreamStall <- arbitrary
    downstreamBackpressure <- arbitrary
    pure . fromShakeTest256 $
      ShakeTest
        messageBytes
        outputBytes
        upstreamStall
        downstreamBackpressure

makeBasicTest :: ByteString -> Int -> SHAKE256Test
makeBasicTest input outputBytes =
  fromShakeTest256 (Common.makeBasicTest input outputBytes)

makeVariableOutputTest :: ByteString -> Int -> SHAKE256Test
makeVariableOutputTest input outputBytes =
  fromShakeTest256 (Common.makeVariableOutputTest input outputBytes)

makeStallTest :: ByteString -> Int -> [Bool] -> SHAKE256Test
makeStallTest input outputBytes pattern =
  fromShakeTest256 (Common.makeStallTest input outputBytes pattern)

makeBackpressureTest :: ByteString -> Int -> [Bool] -> SHAKE256Test
makeBackpressureTest input outputBytes pattern =
  fromShakeTest256 (Common.makeBackpressureTest input outputBytes pattern)

makeCombinedTest :: ByteString -> Int -> [Bool] -> [Bool] -> SHAKE256Test
makeCombinedTest input outputBytes stallPattern backpressurePattern =
  fromShakeTest256
    ( Common.makeCombinedTest
        input
        outputBytes
        stallPattern
        backpressurePattern
    )
