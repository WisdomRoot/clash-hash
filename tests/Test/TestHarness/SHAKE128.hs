{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Self-contained test harness infrastructure for SHAKE128 hardware testing.
--   Mirrors the SHAKE256 harness but uses the SHAKE128 rate (1344 bits / 21 beats).
module Test.TestHarness.SHAKE128
  ( -- * Test data types
    SHAKE128Test (..),
    UpstreamStall (..),
    DownstreamBackpressure (..),
    -- * Test execution
    runTest,
    testLabel,
    -- * Common test messages
    msg1344,
    msg1408,
    msg2016,
    msg2688,
    -- * Stall patterns
    stallPatternSimple,
    stallPatternModerate,
    stallPatternAggressive,
    -- * Backpressure patterns
    backpressurePatternSimple,
    backpressurePatternModerate,
    backpressurePatternAggressive,
    -- * Test case builders
    makeBasicTest,
    makeVariableOutputTest,
    makeStallTest,
    makeBackpressureTest,
    makeCombinedTest
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Clash.Sized.Vector qualified as V
import Data.Bits qualified as Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import Hash.NonPipelined.SHAKE128 qualified as SHAKE128
import Prelude qualified as P
import Reference.Hash qualified as Hash
import Reference.SHA3 qualified as SHA3
import Test.Hspec (Expectation, shouldBe)
import Test.QuickCheck
  ( Arbitrary (..),
    frequency,
    listOf1,
    vector
  )

--------------------------------------------------------------------------------
-- Data types
--------------------------------------------------------------------------------

data UpstreamStall
  = NoUpstreamStall
  | UpstreamStall [Bool]
  deriving (Show)

data DownstreamBackpressure
  = NoDownstreamBackpressure
  | DownstreamBackpressure [Bool]
  deriving (Show)

data SHAKE128Test = SHAKE128Test
  { testMessage :: ByteString,
    testOutputBytes :: Int,
    testUpstreamStall :: UpstreamStall,
    testDownstreamBackpressure :: DownstreamBackpressure
  }
  deriving (Show)

testLabel :: SHAKE128Test -> String
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
-- QuickCheck helpers
--------------------------------------------------------------------------------

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

instance Arbitrary SHAKE128Test where
  arbitrary = do
    -- beat counts (1 beat = 64 bits)
    beatCount <-
      frequency
        [ (1, pure 0),
          (2, pure 1),
          (1, pure 2),
          (1, pure 20),
          (2, pure 21), -- exact 1344-bit block
          (1, pure 22),
          (2, pure 25),
          (1, pure 30),
          (1, pure 42)
        ]
    let byteCount = beatCount P.* 8
    messageBytes <- BS.pack <$> vector byteCount
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
    pure $
      SHAKE128Test
        messageBytes
        outputBytes
        upstreamStall
        downstreamBackpressure

--------------------------------------------------------------------------------
-- Reference SHAKE128 (ByteString)
--------------------------------------------------------------------------------

referenceShake128BS :: Int -> ByteString -> ByteString
referenceShake128BS outputBytes input =
  let inputBits = Hash.bsToBitList input
      outputBits = outputBytes P.* 8
      domain = [1, 1, 1, 1] -- SHAKE domain separator
      resultBits = Hash.sponge @1600 @1344 SHA3.keccakf outputBits (inputBits P.++ domain)
   in Hash.bitListToBS resultBits

--------------------------------------------------------------------------------
-- Running tests
--------------------------------------------------------------------------------

runTest :: SHAKE128Test -> Expectation
runTest test = do
  let expectedBS = referenceShake128BS (testOutputBytes test) (testMessage test)
      actualBS = runHardware test
  actualBS `shouldBe` expectedBS

runHardware :: SHAKE128Test -> ByteString
runHardware test =
  let inputBytes = BS.length (testMessage test)
      beats = (inputBytes P.+ 7) `P.div` 8
   in case fromJust (someNatVal (P.fromIntegral beats)) of
        SomeNat (_ :: Proxy beats') ->
          runHardware' @beats' test beats

runHardware' ::
  forall beats.
  KnownNat beats =>
  SHAKE128Test ->
  Int ->
  ByteString
runHardware' test beats =
  let inputBS = testMessage test
      outputBytes = testOutputBytes test
      inputBits = bsToBitListHW inputBS
      paddedBits = P.take (beats P.* 64) (inputBits P.++ P.repeat 0)
      messageWords = bitListToWords @beats beats paddedBits
      inputStream =
        withClockResetEnable clockGen resetGen enableGen $
          feedInput @beats (testUpstreamStall test) messageWords
      treadySignal = makeBackpressureSignal (testDownstreamBackpressure test)
      output =
        SHAKE128.topEntity
          clockGen
          resetGen
          enableGen
          treadySignal
          inputStream
      outputBits = testOutputBytes test P.* 8
      outputBeats = (outputBits P.+ 63) `P.div` 64
      squeezesNeeded = (outputBeats P.+ 20) `P.div` 21
      sampleCount = beats P.* 2 P.+ 24 P.+ squeezesNeeded P.* (21 P.+ 24) P.+ 200
      samples = sampleN @System sampleCount output
      validOutputs = [tdata stream | (stream, _) <- samples, tvalid stream]
      outputWordBits = P.concatMap wordToBits (P.take outputBeats validOutputs)
      resultBits = P.take outputBits outputWordBits
   in bitListToBSHW resultBits

--------------------------------------------------------------------------------
-- Input helper (with flush)
--------------------------------------------------------------------------------

feedInput ::
  forall beats dom.
  ( KnownNat beats,
    HiddenClockResetEnable dom
  ) =>
  UpstreamStall ->
  Vec beats (BitVector 64) ->
  Signal dom (AXI4Stream 64, Bool)
feedInput control messageWords =
  let isEmpty = V.length messageWords == 0
   in mealy step (toList messageWords, 0 :: Int, 0 :: Int, stallPattern, isEmpty) (pure ())
  where
    stallPattern = case control of
      NoUpstreamStall -> []
      UpstreamStall xs -> xs
    beatsPerBlock = 21 :: Int
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
                  [] | wasEmpty ->
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
-- Bit/word conversion utilities
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

bitListToWords :: forall beats. KnownNat beats => Int -> [Bit] -> Vec beats (BitVector 64)
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
-- Sample messages
--------------------------------------------------------------------------------

msg1344 :: ByteString
msg1344 = BS8.replicate (21 * 8) 'a'

msg1408 :: ByteString
msg1408 = BS8.replicate (22 * 8) 'b'

msg2016 :: ByteString
msg2016 = BS8.pack $ P.take 252 (P.cycle "0123456789abcdef")

msg2688 :: ByteString
msg2688 = BS8.pack $ P.take 336 (P.cycle "qwertyuiopasdfgh")

--------------------------------------------------------------------------------
-- Stall / backpressure patterns
--------------------------------------------------------------------------------

stallPatternSimple :: [Bool]
stallPatternSimple =
  [True, True, False, True, True, True, False, True, True, True]

stallPatternModerate :: [Bool]
stallPatternModerate =
  [True, False, True, False, True, False, True, False, True, False]

stallPatternAggressive :: [Bool]
stallPatternAggressive =
  [True, False, False, True, False, True, False, False, False, True]

backpressurePatternSimple :: [Bool]
backpressurePatternSimple =
  [True, True, True, False, True, True, True, False, True, True]

backpressurePatternModerate :: [Bool]
backpressurePatternModerate =
  [True, False, True, False, True, False, True, False, True, False]

backpressurePatternAggressive :: [Bool]
backpressurePatternAggressive =
  [True, False, False, False, True, False, False, True, False, False]

--------------------------------------------------------------------------------
-- Builders
--------------------------------------------------------------------------------

makeBasicTest :: ByteString -> Int -> SHAKE128Test
makeBasicTest input outputBytes =
  SHAKE128Test input outputBytes NoUpstreamStall NoDownstreamBackpressure

makeVariableOutputTest :: ByteString -> Int -> SHAKE128Test
makeVariableOutputTest = makeBasicTest

makeStallTest :: ByteString -> Int -> [Bool] -> SHAKE128Test
makeStallTest input outputBytes pattern =
  SHAKE128Test input outputBytes (UpstreamStall pattern) NoDownstreamBackpressure

makeBackpressureTest :: ByteString -> Int -> [Bool] -> SHAKE128Test
makeBackpressureTest input outputBytes pattern =
  SHAKE128Test input outputBytes NoUpstreamStall (DownstreamBackpressure pattern)

makeCombinedTest :: ByteString -> Int -> [Bool] -> [Bool] -> SHAKE128Test
makeCombinedTest input outputBytes stallPattern backpressurePattern =
  SHAKE128Test
    input
    outputBytes
    (UpstreamStall stallPattern)
    (DownstreamBackpressure backpressurePattern)
