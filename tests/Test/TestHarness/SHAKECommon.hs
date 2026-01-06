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
import Clash.Sized.Vector qualified as V
import Data.Bits qualified as Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import Test.Hspec (Expectation, shouldBe)
import Test.QuickCheck (Arbitrary (..), Gen, frequency, listOf1, vector)
import Prelude qualified as P

--------------------------------------------------------------------------------
-- Shared data types
--------------------------------------------------------------------------------

data UpstreamStall
  = NoUpstreamStall
  | UpstreamStall [Bool]
  deriving (Show)

data DownstreamBackpressure
  = NoDownstreamBackpressure
  | DownstreamBackpressure [Bool]
  deriving (Show)

instance Arbitrary UpstreamStall where
  arbitrary =
    frequency
      [ (3, pure NoUpstreamStall),
        (1, UpstreamStall <$> listOf1 (frequency [(3, pure True), (1, pure False)]))
      ]

instance Arbitrary DownstreamBackpressure where
  arbitrary =
    frequency
      [ (3, pure NoDownstreamBackpressure),
        (1, DownstreamBackpressure <$> listOf1 (frequency [(3, pure True), (1, pure False)]))
      ]

data ShakeTest = ShakeTest
  { testMessage :: ByteString,
    testOutputBytes :: Int,
    testUpstreamStall :: UpstreamStall,
    testDownstreamBackpressure :: DownstreamBackpressure
  }
  deriving (Show)

data ShakeGenConfig = ShakeGenConfig
  { sgBeatOptions :: [(Int, Int)],
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
  beatCount <- frequency (toFreq <$> sgBeatOptions config)
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
-- Input / backpressure helpers
--------------------------------------------------------------------------------

feedInput ::
  forall beats dom.
  ( KnownNat beats,
    HiddenClockResetEnable dom
  ) =>
  Int ->
  UpstreamStall ->
  Vec beats (BitVector 64) ->
  Signal dom (AXI4Stream 64, Bool)
feedInput beatsPerBlock control messageWords =
  let isEmpty = V.length messageWords == 0
   in mealy step (toList messageWords, 0 :: Int, 0 :: Int, stallPattern, isEmpty) (pure ())
  where
    stallPattern = case control of
      NoUpstreamStall -> []
      UpstreamStall xs -> xs
    permuteLatency = 24 :: Int
    step (xs, waitCount, emittedInBlock, ctrl, wasEmpty) _ =
      if waitCount P.> 0
        then ((xs, waitCount P.- 1, emittedInBlock, ctrl, wasEmpty), (idleBeat, False))
        else
          let (canSend, ctrl') = case ctrl of
                [] -> (True, [])
                b : bs -> (b, bs)
           in if P.not canSend
                then ((xs, waitCount, emittedInBlock, ctrl', wasEmpty), (idleBeat, False))
                else case xs of
                  []
                    | wasEmpty ->
                        (([], 0, 0, ctrl', False), (idleBeat, True))
                  [] -> (([], 0, 0, ctrl', False), (idleBeat, False))
                  y : ys ->
                    let isLast = P.null ys
                        emittedNow = emittedInBlock P.+ 1
                        blockCompleted = emittedNow == beatsPerBlock
                        needGap = blockCompleted P.&& P.not isLast
                        nextWait = if needGap then permuteLatency else 0
                        nextEmitted = if blockCompleted then 0 else emittedNow
                        nextState = (ys, nextWait, nextEmitted, ctrl', False)
                        outBeat =
                          AXI4Stream
                            { tdata = y,
                              tvalid = True,
                              tlast = isLast
                            }
                     in (nextState, (outBeat, False))
    idleBeat =
      AXI4Stream
        { tdata = 0,
          tvalid = False,
          tlast = False
        }

makeBackpressureSignal :: DownstreamBackpressure -> Signal System Bool
makeBackpressureSignal NoDownstreamBackpressure = pure True
makeBackpressureSignal (DownstreamBackpressure pattern) =
  fromList (pattern P.++ P.repeat True)

--------------------------------------------------------------------------------
-- Conversion helpers
--------------------------------------------------------------------------------

bsToBitListHW :: ByteString -> [Bit]
bsToBitListHW bs =
  P.concatMap word8ToBits (BS.unpack bs)
  where
    word8ToBits :: Word8 -> [Bit]
    word8ToBits w = [if Bits.testBit w i then 1 else 0 | i <- [0 .. 7]]

bitListToBSHW :: [Bit] -> ByteString
bitListToBSHW bits =
  BS.pack (packBytes bits)
  where
    packBytes [] = []
    packBytes bs =
      let (chunk, rest) = P.splitAt 8 bs
          byte = P.foldl setBit' 0 (P.zip [0 ..] chunk)
       in byte : packBytes rest
    setBit' acc (i, b) = if b == 1 then Bits.setBit acc i else acc

bitListToWords :: forall beats. (KnownNat beats) => Int -> [Bit] -> Vec beats (BitVector 64)
bitListToWords n bits =
  let chunks = chunksOf 64 bits
      wordsList = P.map bitsToWord (P.take n chunks)
   in V.unsafeFromList wordsList
  where
    chunksOf _ [] = []
    chunksOf m xs = P.take m xs : chunksOf m (P.drop m xs)
    bitsToWord bs =
      let paddedBits = P.take 64 (bs P.++ P.repeat 0)
          word = P.foldl accumBit 0 (P.zip [63, 62 .. 0] paddedBits)
       in word
    accumBit acc (i, b) = if b == 1 then Bits.setBit acc i else acc

wordToBits :: BitVector 64 -> [Bit]
wordToBits w = [if Bits.testBit w i then 1 else 0 | i <- [63, 62 .. 0]]

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
