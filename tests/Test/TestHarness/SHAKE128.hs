{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.TestHarness.SHAKE128
  ( SHAKE128Test (..),
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
import Hash.NonPipelined.SHAKE128 qualified as SHAKE128
import Reference.Hash qualified as Hash
import Reference.SHA3 qualified as SHA3
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

newtype SHAKE128Test = SHAKE128Test { getShake128Test :: ShakeTest }
  deriving stock (Show)

toShakeTest128 :: SHAKE128Test -> ShakeTest
toShakeTest128 (SHAKE128Test t) = t

fromShakeTest128 :: ShakeTest -> SHAKE128Test
fromShakeTest128 = SHAKE128Test

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

runTest :: SHAKE128Test -> Expectation
runTest = runShakeTest shake128Params . toShakeTest128

runHardware :: SHAKE128Test -> ByteString
runHardware = runShakeHardware shake128Params . toShakeTest128

testLabel :: SHAKE128Test -> String
testLabel = Common.testLabel . toShakeTest128

instance Arbitrary SHAKE128Test where
  arbitrary = do
    beatCount <-
      frequency
        [ (1, pure 0),
          (2, pure 1),
          (1, pure 2),
          (1, pure 20),
          (2, pure 21),
          (1, pure 22),
          (2, pure 25),
          (1, pure 30),
          (1, pure 42)
        ]
    messageBytes <- BS.pack <$> vector (beatCount * 8)
    outputBytes <-
      frequency
        [ (2, pure 8),
          (1, pure 16),
          (2, pure 32),
          (1, pure 64),
          (1, pure 96),
          (1, pure 128)
        ]
    upstreamStall <- arbitrary
    downstreamBackpressure <- arbitrary
    pure . fromShakeTest128 $
      ShakeTest
        messageBytes
        outputBytes
        upstreamStall
        downstreamBackpressure

makeBasicTest :: ByteString -> Int -> SHAKE128Test
makeBasicTest input outputBytes =
  fromShakeTest128 (Common.makeBasicTest input outputBytes)

makeVariableOutputTest :: ByteString -> Int -> SHAKE128Test
makeVariableOutputTest input outputBytes =
  fromShakeTest128 (Common.makeVariableOutputTest input outputBytes)

makeStallTest :: ByteString -> Int -> [Bool] -> SHAKE128Test
makeStallTest input outputBytes pattern =
  fromShakeTest128 (Common.makeStallTest input outputBytes pattern)

makeBackpressureTest :: ByteString -> Int -> [Bool] -> SHAKE128Test
makeBackpressureTest input outputBytes pattern =
  fromShakeTest128 (Common.makeBackpressureTest input outputBytes pattern)

makeCombinedTest :: ByteString -> Int -> [Bool] -> [Bool] -> SHAKE128Test
makeCombinedTest input outputBytes stallPattern backpressurePattern =
  fromShakeTest128
    (Common.makeCombinedTest input outputBytes stallPattern backpressurePattern)
