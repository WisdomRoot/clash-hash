{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.TestCase
  ( TestCase (TestCase),
    testCaseLabel,
    SomeMessage (..),
    UpstreamStall (..),
    Segment (..),
    Result (..),
    toActualResult,
    toExpectedResult,
    runTestCase,
    expectedCycles,
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Control.Monad (unless)
import Hash.NonPipelined qualified
import Numeric (showHex)
import Reference.SHA3 (SpongeParameter)
import Reference.SHA3 qualified as SHA3
import Test.Hspec
import Test.QuickCheck hiding (Result)
import Prelude qualified as P

type RateBits = 1088

type StreamWordBits = 64

type BeatsPerBlock = RateBits `Div` StreamWordBits

type PermutationLatency = 24

type SqueezeBeats = 4

beatsPerBlock :: Int
beatsPerBlock = natToNum @BeatsPerBlock

permuteLatency :: Int
permuteLatency = natToNum @PermutationLatency

squeezeLatency :: Int
squeezeLatency = natToNum @SqueezeBeats

data SomeMessage where
  SomeMessage ::
    forall beats n.
    ( KnownNat beats,
      KnownNat (beats * 64),
      SpongeParameter 1600 1088 n ((beats * 64) + 2) 0 256
    ) =>
    Vec (beats * 64) Bit ->
    SomeMessage

data TestCase
  = TestCase
      SomeMessage

instance Show TestCase where
  show (TestCase (SomeMessage (_ :: Vec (beats * 64) Bit))) =
    "TestCase {"
    <> "beats=" <> show (natToNum @beats :: Int)
    <> "}"

data UpstreamStall
  = NoUpstreamStall
  | UpstreamStall [Bool]
  deriving (Show)

instance Arbitrary UpstreamStall where
  arbitrary = pure NoUpstreamStall

instance Arbitrary TestCase where
  arbitrary = oneof genCaseGenerators

-- A segment of output signals
data Segment = Segment
  { segmentInterval :: (Int, Int), -- [start cycle, end cycle)
    segmentData :: [BitVector 64],
    segmentValid :: [Bool],
    segmentLast :: [Bool],
    segmentReady :: [Bool]
  }
  deriving (Show, Eq)

data Result = Result
  { previousSegment :: Segment, -- the segment that contains the previous cycles
    digestSegment :: Segment, -- the 4-cycle segment containing the output digest
    followingSegment :: Segment -- the segment that contains the following cycles
  }
  deriving (Show, Eq)

toActualResult :: TestCase -> Result
toActualResult testCase@(TestCase (SomeMessage (message :: Vec (beats * 64) Bit))) =
  let messageWords = bitCoerce message :: Vec beats (BitVector 64)
      inputStream =
        withClockResetEnable clockGen resetGen enableGen
          $ feedInput messageWords
      output =
        Hash.NonPipelined.topEntity
          clockGen
          resetGen
          enableGen
          (pure True)
          inputStream
      sampleCount = expectedCycles testCase
      samples :: [(AXI4Stream 64, Bool)]
      samples = sampleN @System sampleCount output

      actualTdata = fmap (tdata . fst) samples
      actualTvalid = fmap (tvalid . fst) samples
      actualTlast = fmap (tlast . fst) samples
      actualTready = fmap snd samples

      digestStart = max 0 (sampleCount - 4)
      digestEnd = min sampleCount (digestStart + 4)
      followingStart = digestEnd

      mkSegment interval@(start, end) =
        Segment
          { segmentInterval = interval,
            segmentData = takeSlice start end actualTdata,
            segmentValid = takeSlice start end actualTvalid,
            segmentLast = takeSlice start end actualTlast,
            segmentReady = takeSlice start end actualTready
          }

      takeSlice start end = P.take (end - start) . P.drop start
   in Result
        { previousSegment = mkSegment (0, digestStart),
          digestSegment = mkSegment (digestStart, digestEnd),
          followingSegment = mkSegment (followingStart, sampleCount)
        }

expectedDigest :: SomeMessage -> Vec 4 (BitVector 64)
expectedDigest (SomeMessage (messageBits :: Vec (beats * 64) Bit)) =
  bitCoerce (SHA3.sha3_256 messageBits)

toExpectedResult :: TestCase -> Result
toExpectedResult testCase@(TestCase someMessage) =
  let sampleCount = expectedCycles testCase
      digestStart = max 0 (sampleCount - 4)
      digestEnd = min sampleCount (digestStart + 4)
      followingStart = digestEnd
      expected = expectedDigest someMessage
      (out0, out1, out2, out3) =
        case toList expected of
          [a, b, c, d] -> (a, b, c, d)
          other -> error $ "Unexpected digest words: " <> show other
      cycles = [0 .. sampleCount - 1]

      expectedTdata = fmap mkTdata cycles
        where
          mkTdata i
            | i == sampleCount - 4 = out0
            | i == sampleCount - 3 = out1
            | i == sampleCount - 2 = out2
            | i == sampleCount - 1 = out3
            | otherwise = 0

      expectedTvalid = fmap mkTvalid cycles
        where
          mkTvalid i
            | i >= sampleCount - 4 && i <= sampleCount - 1 = True
            | otherwise = False

      expectedTlast = fmap mkTlast cycles
        where
          mkTlast i
            | i == sampleCount - 1 = True
            | otherwise = False

      expectedTready = P.replicate sampleCount False

      mkSegment interval@(start, end) =
        Segment
          { segmentInterval = interval,
            segmentData = takeSlice start end expectedTdata,
            segmentValid = takeSlice start end expectedTvalid,
            segmentLast = takeSlice start end expectedTlast,
            segmentReady = takeSlice start end expectedTready
          }

      takeSlice start end = P.take (end - start) . P.drop start
   in Result
        { previousSegment = mkSegment (0, digestStart),
          digestSegment = mkSegment (digestStart, digestEnd),
          followingSegment = mkSegment (followingStart, sampleCount)
        }


genCaseFor ::
  forall beats n.
  ( KnownNat beats,
    KnownNat (beats * 64),
    SpongeParameter 1600 1088 n ((beats * 64) + 2) 0 256
  ) =>
  Gen TestCase
genCaseFor = do
  messageBits <- genMessageBits @(beats * 64)
  pure (TestCase (SomeMessage messageBits))

genCaseGenerators :: [Gen TestCase]
genCaseGenerators =
  [ genCaseFor @1,
    genCaseFor @2,
    genCaseFor @3,
    genCaseFor @4,
    genCaseFor @5,
    genCaseFor @6,
    genCaseFor @7,
    genCaseFor @8,
    genCaseFor @9,
    genCaseFor @10,
    genCaseFor @11,
    genCaseFor @12,
    genCaseFor @13,
    genCaseFor @14,
    genCaseFor @15,
    genCaseFor @16,
    genCaseFor @17,
    genCaseFor @18,
    genCaseFor @19,
    genCaseFor @20,
    genCaseFor @21,
    genCaseFor @22,
    genCaseFor @23,
    genCaseFor @24,
    genCaseFor @25,
    genCaseFor @26,
    genCaseFor @27,
    genCaseFor @28,
    genCaseFor @29,
    genCaseFor @30,
    genCaseFor @31,
    genCaseFor @32,
    genCaseFor @33,
    genCaseFor @34,
    genCaseFor @35,
    genCaseFor @36,
    genCaseFor @37,
    genCaseFor @38,
    genCaseFor @39,
    genCaseFor @40,
    genCaseFor @41,
    genCaseFor @42,
    genCaseFor @43,
    genCaseFor @44,
    genCaseFor @45,
    genCaseFor @46,
    genCaseFor @47,
    genCaseFor @48,
    genCaseFor @49,
    genCaseFor @50,
    genCaseFor @51,
    genCaseFor @52,
    genCaseFor @53,
    genCaseFor @54,
    genCaseFor @55,
    genCaseFor @56,
    genCaseFor @57,
    genCaseFor @58,
    genCaseFor @59,
    genCaseFor @60
  ]

genMessageBits ::
  forall n.
  KnownNat n =>
  Gen (Vec n Bit)
genMessageBits =
  sequenceA (repeat (boolToBit <$> arbitrary))

-- | Get a label for a test case
testCaseLabel :: TestCase -> String
testCaseLabel (TestCase (SomeMessage (_ :: Vec (beats * 64) Bit))) =
  show (natToNum @beats * 64 :: Int) <> "-bit"

-- | Predict the number of cycles with upstream stall support (using structural induction)
--   Current formula: beatCount + 24×(⌊beatCount/17⌋ + 1) + 4
expectedCycles :: TestCase -> Int
expectedCycles (TestCase (SomeMessage (_ :: Vec (beats * 64) Bit))) =
  let beatCount = natToNum @beats :: Int
      permutePhases = (beatCount `div` beatsPerBlock) + 1
   in beatCount + permuteLatency * permutePhases + squeezeLatency

runTestCase :: TestCase -> Expectation
runTestCase testCase = do
  let actualResult = toActualResult testCase
      expectedResult = toExpectedResult testCase
  compareResult actualResult expectedResult

compareResult :: Result -> Result -> Expectation
compareResult actual expected = do
  compareSegment "previous" (previousSegment actual) (previousSegment expected)
  compareSegment "digest" (digestSegment actual) (digestSegment expected)
  compareSegment "following" (followingSegment actual) (followingSegment expected)

-- | Format BitVector as hexadecimal string
showHexBV :: BitVector 64 -> String
showHexBV bv = "0x" <> P.replicate (16 - P.length hex) '0' <> hex
  where
    hex = showHex bv ""

-- | Find differences between actual and expected lists with cycle indices
findDataDifferences :: [BitVector 64] -> [BitVector 64] -> (Int, Int) -> [(Int, BitVector 64, BitVector 64)]
findDataDifferences actuals expecteds (start, _) =
  let indexed = P.zip3 [start..] actuals expecteds
  in [(i, a, e) | (i, a, e) <- indexed, a /= e]

-- | Find differences between actual and expected Bool lists with cycle indices
findBoolDifferences :: [Bool] -> [Bool] -> (Int, Int) -> [(Int, Bool, Bool)]
findBoolDifferences actuals expecteds (start, _) =
  let indexed = P.zip3 [start..] actuals expecteds
  in [(i, a, e) | (i, a, e) <- indexed, a /= e]

-- | Format a data difference as a string
formatDataDiff :: (Int, BitVector 64, BitVector 64) -> String
formatDataDiff (cycleIdx, actual, expected) =
  "  Cycle " <> show cycleIdx <> ": expected " <> showHexBV expected <> ", got " <> showHexBV actual

-- | Format a boolean difference as a string
formatBoolDiff :: (Int, Bool, Bool) -> String
formatBoolDiff (cycleIdx, actual, expected) =
  "  Cycle " <> show cycleIdx <> ": expected " <> show expected <> ", got " <> show actual

compareSegment :: String -> Segment -> Segment -> Expectation
compareSegment segLabel actual expected = do
  let prefix = "[" <> segLabel <> " segment] "
      interval = segmentInterval actual

  segmentInterval actual `shouldBe` segmentInterval expected

  unless (segmentData actual == segmentData expected) $ do
    let diffs = findDataDifferences (segmentData actual) (segmentData expected) interval
        diffCount = P.length diffs
        totalCount = P.length (segmentData expected)
        summary = show diffCount <> " difference(s) out of " <> show totalCount <> " cycles"
        diffLines = P.take 10 $ fmap formatDataDiff diffs
        hasMore = P.length diffs > 10
        moreMsg = if hasMore
                  then ["  ... and " <> show (P.length diffs - 10) <> " more differences"]
                  else []
    expectationFailure $ P.unlines $
      [prefix <> "Data mismatch (" <> summary <> "):"]
      <> diffLines
      <> moreMsg

  unless (segmentValid actual == segmentValid expected) $ do
    let diffs = findBoolDifferences (segmentValid actual) (segmentValid expected) interval
        diffCount = P.length diffs
        totalCount = P.length (segmentValid expected)
        summary = show diffCount <> " difference(s) out of " <> show totalCount <> " cycles"
        diffLines = P.take 10 $ fmap formatBoolDiff diffs
        hasMore = P.length diffs > 10
        moreMsg = if hasMore
                  then ["  ... and " <> show (P.length diffs - 10) <> " more differences"]
                  else []
    expectationFailure $ P.unlines $
      [prefix <> "Valid mismatch (" <> summary <> "):"]
      <> diffLines
      <> moreMsg

  unless (segmentLast actual == segmentLast expected) $ do
    let diffs = findBoolDifferences (segmentLast actual) (segmentLast expected) interval
        diffCount = P.length diffs
        totalCount = P.length (segmentLast expected)
        summary = show diffCount <> " difference(s) out of " <> show totalCount <> " cycles"
        diffLines = P.take 10 $ fmap formatBoolDiff diffs
        hasMore = P.length diffs > 10
        moreMsg = if hasMore
                  then ["  ... and " <> show (P.length diffs - 10) <> " more differences"]
                  else []
    expectationFailure $ P.unlines $
      [prefix <> "Last mismatch (" <> summary <> "):"]
      <> diffLines
      <> moreMsg

feedInput ::
  forall beats dom.
  ( KnownNat beats,
    HiddenClockResetEnable dom
  ) =>
  Vec beats (BitVector 64) ->
  Signal dom (AXI4Stream 64)
feedInput messageWords =
  mealy step (toList messageWords, 0 :: Int, 0 :: Int) (pure ())
  where
    step (xs, waitCount, emittedInBlock) _ =
      if waitCount > 0
        then ((xs, waitCount - 1, emittedInBlock), idleBeat)
        else
          case xs of
            [] -> (([], 0, 0), idleBeat)
            y : ys ->
              let isLast = P.null ys
                  emittedNow = emittedInBlock + 1
                  blockCompleted = emittedNow == beatsPerBlock
                  needGap = blockCompleted && not isLast
                  nextWait = if needGap then permuteLatency else 0
                  nextEmitted =
                    if blockCompleted
                      then 0
                      else emittedNow
                  nextState = (ys, nextWait, nextEmitted)
                  outBeat =
                    AXI4Stream
                      { tdata = y,
                        tvalid = True,
                        tlast = isLast
                      }
               in (nextState, outBeat)
    idleBeat =
      AXI4Stream
        { tdata = 0,
          tvalid = False,
          tlast = False
        }
