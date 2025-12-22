{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

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
import Reference.SHA3 (SpongeParameter)
import Reference.SHA3 qualified as SHA3
import Test.Hspec
import Test.QuickCheck hiding (Result)
import Prelude qualified as P

data SomeMessage where
  SomeMessage :: (KnownNat beats) => Vec (beats * 64) Bit -> SomeMessage

data TestCase
  = TestCase
      SomeMessage
      (Vec 4 (BitVector 64))
      UpstreamStall

instance Show TestCase where
  show (TestCase (SomeMessage (_ :: Vec (beats * 64) Bit)) expected control) =
    "TestCase {"
    <> "beats=" <> show (natToNum @beats :: Int)
    <> ", expected=" <> show expected
    <> ", control=" <> show control
    <> "}"

data UpstreamStall
  = NoUpstreamStall
  | UpstreamStall [Bool]
  deriving (Show)

instance Arbitrary UpstreamStall where
  arbitrary = pure NoUpstreamStall

data BeatChoice
  = Beats64
  | Beats128
  | Beats1024
  | Beats1088
  | Beats1600

instance Arbitrary TestCase where
  arbitrary = do
    choice <- elements beatChoices
    case choice of
      Beats64 -> genCaseFor @1
      Beats128 -> genCaseFor @2
      Beats1024 -> genCaseFor @16
      Beats1088 -> genCaseFor @17
      Beats1600 -> genCaseFor @25
    where
      beatChoices =
        [Beats64, Beats128, Beats1024, Beats1088, Beats1600]

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
toActualResult testCase@(TestCase (SomeMessage (message :: Vec (beats * 64) Bit)) _ control) =
  let messageWords = bitCoerce message :: Vec beats (BitVector 64)
      inputStream =
        withClockResetEnable clockGen resetGen enableGen
          $ feedInput control messageWords
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

toExpectedResult :: TestCase -> Result
toExpectedResult testCase@(TestCase _ expected _) =
  let sampleCount = expectedCycles testCase
      digestStart = max 0 (sampleCount - 4)
      digestEnd = min sampleCount (digestStart + 4)
      followingStart = digestEnd
      [d0, d1, d2, d3] = toList expected
      cycles = [0 .. sampleCount - 1]

      expectedTdata = fmap mkTdata cycles
        where
          mkTdata i
            | i == sampleCount - 4 = d0
            | i == sampleCount - 3 = d1
            | i == sampleCount - 2 = d2
            | i == sampleCount - 1 = d3
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
  stall <- arbitrary
  let expected = bitCoerce (SHA3.sha3_256 messageBits)
  pure (TestCase (SomeMessage messageBits) expected stall)

genMessageBits ::
  forall n.
  KnownNat n =>
  Gen (Vec n Bit)
genMessageBits =
  sequenceA (repeat (boolToBit <$> arbitrary))

-- | Get a label for a test case
testCaseLabel :: TestCase -> String
testCaseLabel (TestCase (SomeMessage (_ :: Vec (beats * 64) Bit)) _ _) =
  show (natToNum @beats * 64 :: Int) <> "-bit"

-- | Predict the number of cycles with upstream stall support (using structural induction)
--   Current formula: beatCount + 24×(⌊beatCount/17⌋ + 1) + 4
expectedCycles :: TestCase -> Int
expectedCycles (TestCase (SomeMessage (_ :: Vec (beats * 64) Bit)) _ control) =
  let beatCount = natToNum @beats :: Int
      -- Absorb phase: actual time to get all beats from upstream
      absorbCycles = case control of
        NoUpstreamStall -> beatCount
        UpstreamStall pattern -> countAbsorbCycles beatCount pattern

      -- Permute and squeeze are independent of upstream stalls
      permuteCycles = 24 * ((beatCount `div` 17) + 1)
      squeezeCycles = 4
   in absorbCycles + permuteCycles + squeezeCycles

-- | Use structural induction on the stall pattern list
countAbsorbCycles :: Int -> [Bool] -> Int
countAbsorbCycles 0 _ = 0 -- base case: no beats needed
countAbsorbCycles n [] = n -- base case: no more pattern, assume all True
countAbsorbCycles n (True : rest) = 1 + countAbsorbCycles (n - 1) rest -- absorbed one beat
countAbsorbCycles n (False : rest) = 1 + countAbsorbCycles n rest -- stalled, no progress

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

compareSegment :: String -> Segment -> Segment -> Expectation
compareSegment label actual expected = do
  let prefix = "[" <> label <> " segment] "

  segmentInterval actual `shouldBe` segmentInterval expected

  unless (segmentData actual == segmentData expected) $
    expectationFailure $ prefix <> "Data mismatch:\n  expected: "
      <> show (segmentData expected) <> "\n  but got:  " <> show (segmentData actual)

  unless (segmentValid actual == segmentValid expected) $
    expectationFailure $ prefix <> "Valid mismatch:\n  expected: "
      <> show (segmentValid expected) <> "\n  but got:  " <> show (segmentValid actual)

  unless (segmentLast actual == segmentLast expected) $
    expectationFailure $ prefix <> "Last mismatch:\n  expected: "
      <> show (segmentLast expected) <> "\n  but got:  " <> show (segmentLast actual)

feedInput ::
  forall beats dom.
  ( KnownNat beats,
    HiddenClockResetEnable dom
  ) =>
  UpstreamStall ->
  Vec beats (BitVector 64) ->
  Signal dom (AXI4Stream 64)
feedInput control messageWords =
  mealy step (toList messageWords, 0 :: Int, 0 :: Int, controlToList control) (pure ())
  where
    controlToList NoUpstreamStall = []
    controlToList (UpstreamStall xs) = xs

    step (xs, waitCount, emittedInBlock, ctrl) _ =
      if waitCount > 0
        then ((xs, waitCount - 1, emittedInBlock, ctrl), idleBeat)
        else
          let (canSend, ctrl') =
                case ctrl of
                  [] -> (True, [])
                  b : bs -> (b, bs)
           in if not canSend
                then ((xs, waitCount, emittedInBlock, ctrl'), idleBeat)
                else case xs of
                  [] -> (([], 0, 0, ctrl'), idleBeat)
                  y : ys ->
                    let isLast = P.null ys
                        emittedNow = emittedInBlock + 1
                        needGap = emittedNow == 17 && not isLast
                        nextWait = if needGap then 24 else 0
                        nextEmitted = if needGap then 0 else emittedNow
                        nextState = (ys, nextWait, nextEmitted, ctrl')
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
