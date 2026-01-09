{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.TestHarness.SHAKECommon
  ( UpstreamStall (..),
    DownstreamBackpressure (..),
    ShakeTest (..),
    ShakeParams (..),
    ShakeTopEntity,
    ShakeGenConfig (..),
    defaultShakeGenConfig,
    shake128GenConfig,
    shake256GenConfig,
    genShakeTest,
    testLabel,
    runShakeTest,
    runShakeHardware,
    makeBasicTest,
    makeVariableOutputTest,
    makeStallTest,
    makeBackpressureTest,
    makeCombinedTest,
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Test.Hspec (Expectation, shouldBe)
import Test.QuickCheck (Arbitrary (..), Gen, chooseInt, frequency, vector)
import Test.TestHarness.StreamCommon
  ( DownstreamBackpressure (..),
    UpstreamStall (..),
    bitListToBSHW,
    bitListToWords,
    bsToBitListHW,
    feedInput,
    makeBackpressureSignal,
    wordToBits,
  )
import Prelude qualified as P

--------------------------------------------------------------------------------
-- SHAKE-specific data types
--------------------------------------------------------------------------------

data ShakeTest = ShakeTest
  { testMessage :: ByteString,
    testOutputBytes :: Int,
    testUpstreamStall :: UpstreamStall,
    testDownstreamBackpressure :: DownstreamBackpressure
  }
  deriving (Show)

data ShakeGenConfig = ShakeGenConfig
  { sgBeatOptions :: [(Int, Int)],
    sgBeatRanges :: [(Int, Int, Int)],
    sgOutputOptions :: [(Int, Int)]
  }

defaultShakeGenConfig :: ShakeGenConfig
defaultShakeGenConfig =
  ShakeGenConfig
    { sgBeatOptions =
        [ (1, 0),
          (2, 1),
          (1, 2),
          (1, 16),
          (2, 17),
          (1, 18),
          (1, 20),
          (2, 21),
          (1, 22),
          (1, 25),
          (1, 34),
          (1, 42),
          (1, 50)
        ],
      sgBeatRanges = [(1, 0, 99)],
      sgOutputOptions =
        [ (2, 8),
          (1, 16),
          (2, 32),
          (1, 64),
          (1, 96),
          (1, 128),
          (1, 256)
        ]
    }

shake128GenConfig :: ShakeGenConfig
shake128GenConfig =
  defaultShakeGenConfig
    { sgBeatOptions =
        [ (1, 0),
          (2, 1),
          (1, 2),
          (1, 20),
          (4, 21),
          (2, 22),
          (1, 25),
          (2, 30),
          (2, 42),
          (1, 50)
        ]
    }

shake256GenConfig :: ShakeGenConfig
shake256GenConfig =
  defaultShakeGenConfig
    { sgBeatOptions =
        [ (1, 0),
          (2, 1),
          (1, 2),
          (1, 16),
          (4, 17),
          (2, 18),
          (1, 25),
          (2, 34),
          (2, 51)
        ]
    }

genShakeTest :: ShakeGenConfig -> Gen ShakeTest
genShakeTest config = do
  beatCount <- frequency (optionFreqs P.++ rangeFreqs)
  messageBytes <- BS.pack <$> vector (beatCount P.* 8)
  outputBytes <- frequency (toFreq <$> sgOutputOptions config)
  upstreamStall <- arbitrary
  ShakeTest
    messageBytes
    outputBytes
    upstreamStall
    <$> arbitrary
  where
    toFreq (weight, value) = (weight, pure value)
    optionFreqs = toFreq <$> sgBeatOptions config
    rangeFreqs =
      [ (weight, chooseInt (lo, hi))
        | (weight, lo, hi) <- sgBeatRanges config
      ]

instance Arbitrary ShakeTest where
  arbitrary = genShakeTest defaultShakeGenConfig

testLabel :: ShakeTest -> String
testLabel test =
  show inputBits
    <> "-bit input, "
    <> show outputBits
    <> "-bit output"
    <> stallInfo
    <> backpressureInfo
  where
    inputBits = BS.length (testMessage test) P.* 8
    outputBits = testOutputBytes test P.* 8
    stallInfo = case testUpstreamStall test of
      NoUpstreamStall -> ""
      UpstreamStall _ -> " [with stalls]"
    backpressureInfo = case testDownstreamBackpressure test of
      NoDownstreamBackpressure -> ""
      DownstreamBackpressure _ -> " [with backpressure]"

--------------------------------------------------------------------------------
-- Harness configuration
--------------------------------------------------------------------------------

type ShakeTopEntity =
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool ->
  Signal System (AXI4Stream 64, Bool) ->
  Signal System (AXI4Stream 64, Bool)

data ShakeParams = ShakeParams
  { spBeatsPerBlock :: Int,
    spReference :: Int -> ByteString -> ByteString,
    spTopEntity :: ShakeTopEntity
  }

--------------------------------------------------------------------------------
-- Running tests
--------------------------------------------------------------------------------

runShakeTest :: ShakeParams -> ShakeTest -> Expectation
runShakeTest params test = do
  let expected = spReference params (testOutputBytes test) (testMessage test)
      actual = runShakeHardware params test
  actual `shouldBe` expected

runShakeHardware :: ShakeParams -> ShakeTest -> ByteString
runShakeHardware params test =
  let beatsPerBlock = spBeatsPerBlock params
      inputBytes = BS.length (testMessage test)
      beats = (inputBytes P.+ 7) `P.div` 8
   in case fromJust (someNatVal (P.fromIntegral beats)) of
        SomeNat (_ :: Proxy beats') ->
          runHardwareKnown @beats' params test beats beatsPerBlock

runHardwareKnown ::
  forall beats.
  (KnownNat beats) =>
  ShakeParams ->
  ShakeTest ->
  Int ->
  Int ->
  ByteString
runHardwareKnown params test beats beatsPerBlock =
  let inputBS = testMessage test
      inputBits = bsToBitListHW inputBS
      paddedBits = P.take (beats P.* 64) (inputBits P.++ P.repeat 0)
      messageWords = bitListToWords @beats beats paddedBits
      inputStream =
        withClockResetEnable clockGen resetGen enableGen
          $ feedInput @beats beatsPerBlock (testUpstreamStall test) messageWords
      treadySignal = makeBackpressureSignal (testDownstreamBackpressure test)
      output =
        spTopEntity
          params
          clockGen
          resetGen
          enableGen
          treadySignal
          inputStream
      outputBits = testOutputBytes test P.* 8
      outputBeats = (outputBits P.+ 63) `P.div` 64
      squeezesNeeded = (outputBeats P.+ beatsPerBlock - 1) `P.div` beatsPerBlock
      sampleCount =
        beats P.* 2
          P.+ 24
          P.+ squeezesNeeded P.* (beatsPerBlock P.+ 24)
          P.+ 200
      samples = sampleN @System sampleCount output
      validOutputs = [tdata stream | (stream, _) <- samples, tvalid stream]
      outputWordBits = P.concatMap wordToBits (P.take outputBeats validOutputs)
      resultBits = P.take outputBits outputWordBits
   in bitListToBSHW resultBits

--------------------------------------------------------------------------------
-- Builder helpers
--------------------------------------------------------------------------------

makeBasicTest :: ByteString -> Int -> ShakeTest
makeBasicTest input outputBytes =
  ShakeTest input outputBytes NoUpstreamStall NoDownstreamBackpressure

makeVariableOutputTest :: ByteString -> Int -> ShakeTest
makeVariableOutputTest = makeBasicTest

makeStallTest :: ByteString -> Int -> [Bool] -> ShakeTest
makeStallTest input outputBytes pattern =
  ShakeTest input outputBytes (UpstreamStall pattern) NoDownstreamBackpressure

makeBackpressureTest :: ByteString -> Int -> [Bool] -> ShakeTest
makeBackpressureTest input outputBytes pattern =
  ShakeTest input outputBytes NoUpstreamStall (DownstreamBackpressure pattern)

makeCombinedTest :: ByteString -> Int -> [Bool] -> [Bool] -> ShakeTest
makeCombinedTest input outputBytes stallPattern backpressurePattern =
  ShakeTest
    input
    outputBytes
    (UpstreamStall stallPattern)
    (DownstreamBackpressure backpressurePattern)
