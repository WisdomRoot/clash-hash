{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.TestCase
  ( SHA3 (SHA3),
    testCaseLabel,
    SomeMessage (..),
    ShakeSomeMessage (..),
    Shake (Shake),
    shakeTestCaseLabel,
    UpstreamStall (..),
    DownstreamBackpressure (..),
    Segment (..),
    Result (..),
    toActualResult,
    toExpectedResult,
    runTestCase,
    runShakeTestCase,
    expectedCycles,
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Control.Monad (unless)
import Hash.SHA3256 qualified as SHA3256
import Hash.SHAKE256 qualified as SHAKE256
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

type Dut =
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool ->
  Signal System (AXI4Stream StreamWordBits) ->
  Signal System (AXI4Stream StreamWordBits, Bool)

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
      SpongeParameter 1600 1088 n (beats * 64 + 2) 0 256
    ) =>
    Vec (beats * 64) Bit ->
    SomeMessage

data SHA3
  = SHA3
      SomeMessage
      UpstreamStall
      DownstreamBackpressure

instance Show SHA3 where
  show (SHA3 (SomeMessage (_ :: Vec (beats * 64) Bit)) upstream downstream) =
    "SHA3 {"
    <> "beats=" <> show (natToNum @beats :: Int)
    <> ", upstream=" <> show upstream
    <> ", downstream=" <> show downstream
    <> "}"

data ShakeSomeMessage where
  ShakeSomeMessage ::
    forall beats n.
    ( KnownNat beats,
      KnownNat (beats * 64),
      SpongeParameter 1600 1088 n (beats * 64 + 4) 0 256
    ) =>
    Vec (beats * 64) Bit ->
    ShakeSomeMessage

data Shake
  = Shake
      ShakeSomeMessage
      UpstreamStall
      DownstreamBackpressure

instance Show Shake where
  show (Shake (ShakeSomeMessage (_ :: Vec (beats * 64) Bit)) upstream downstream) =
    "Shake {"
    <> "beats=" <> show (natToNum @beats :: Int)
    <> ", upstream=" <> show upstream
    <> ", downstream=" <> show downstream
    <> "}"

data UpstreamStall
  = NoUpstreamStall
  | UpstreamStall [Bool]
  deriving (Show)

instance Arbitrary UpstreamStall where
  arbitrary = frequency
    [ (3, pure NoUpstreamStall),          -- 75% no stalls
      (1, UpstreamStall <$> genStalls)    -- 25% random stalls
    ]
    where
      genStalls = listOf (frequency [(3, pure True), (1, pure False)])
      -- Generates list of booleans: 75% True (send), 25% False (stall)

data DownstreamBackpressure
  = NoDownstreamBackpressure
  | DownstreamBackpressure [Bool]
  deriving (Show)

instance Arbitrary DownstreamBackpressure where
  arbitrary = frequency
    [ (3, pure NoDownstreamBackpressure),           -- 75% no backpressure
      (1, DownstreamBackpressure <$> genBackpressure) -- 25% random backpressure
    ]
    where
      genBackpressure = listOf (frequency [(3, pure True), (1, pure False)])
      -- Generates list of booleans: 75% True (accept), 25% False (stall)

instance Arbitrary SHA3 where
  arbitrary = oneof genCaseGenerators

instance Arbitrary Shake where
  arbitrary = oneof shakeGenCaseGenerators

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

toActualResult :: SHA3 -> Result
toActualResult = toActualResultWith SHA3256.topEntity

toExpectedResult :: SHA3 -> Result
toExpectedResult = toExpectedResultWith (bitCoerce . SHA3.sha3_256)

backpressureToList :: DownstreamBackpressure -> [Bool]
backpressureToList NoDownstreamBackpressure = []
backpressureToList (DownstreamBackpressure xs) = xs

genCaseFor ::
  forall beats n.
  ( KnownNat beats,
    KnownNat (beats * 64),
    SpongeParameter 1600 1088 n (beats * 64 + 2) 0 256
  ) =>
  Gen SHA3
genCaseFor = do
  messageBits <- genMessageBits @(beats * 64)
  upstreamStall <- arbitrary
  SHA3 (SomeMessage messageBits) upstreamStall <$> arbitrary

genCaseGenerators :: [Gen SHA3]
genCaseGenerators =
  [ genCaseFor @1,   -- Minimum message
    genCaseFor @16,  -- One beat before first block fills
    genCaseFor @17,  -- Exactly one block (triggers 1 permutation)
    genCaseFor @18,  -- Just over one block (triggers 2 permutations)
    genCaseFor @25,  -- Full Keccak state (1600 bits)
    genCaseFor @34,  -- Exactly two blocks (17 × 2)
    genCaseFor @51   -- Exactly three blocks (17 × 3)
  ]

shakeGenCaseFor ::
  forall beats n.
  ( KnownNat beats,
    KnownNat (beats * 64),
    SpongeParameter 1600 1088 n (beats * 64 + 4) 0 256
  ) =>
  Gen Shake
shakeGenCaseFor = do
  messageBits <- genMessageBits @(beats * 64)
  upstreamStall <- arbitrary
  Shake (ShakeSomeMessage messageBits) upstreamStall <$> arbitrary

shakeGenCaseGenerators :: [Gen Shake]
shakeGenCaseGenerators =
  [ shakeGenCaseFor @1,
    shakeGenCaseFor @16,
    shakeGenCaseFor @17,
    shakeGenCaseFor @18,
    shakeGenCaseFor @25,
    shakeGenCaseFor @34,
    shakeGenCaseFor @51
  ]

genMessageBits ::
  forall n.
  KnownNat n =>
  Gen (Vec n Bit)
genMessageBits =
  sequenceA (repeat (boolToBit <$> arbitrary))

-- | Get a label for a test case
testCaseLabel :: SHA3 -> String
testCaseLabel (SHA3 (SomeMessage (_ :: Vec (beats * 64) Bit)) _ _) =
  show (natToNum @beats * 64 :: Int) <> "-bit"

shakeTestCaseLabel :: Shake -> String
shakeTestCaseLabel (Shake (ShakeSomeMessage (_ :: Vec (beats * 64) Bit)) _ _) =
  show (natToNum @beats * 64 :: Int) <> "-bit"

--------------------------------------------------------------------------------
-- Shared helpers between SHA3-256 and SHAKE-256 cases
--------------------------------------------------------------------------------

type DigestWords = Vec SqueezeBeats (BitVector 64)

class HashCase c where
  withMessage ::
    c ->
    (forall beats n.
      ( KnownNat beats,
        KnownNat (beats * 64),
        SpongeParameter 1600 1088 n (beats * 64 + Suffix c) 0 256
      ) =>
      Vec (beats * 64) Bit ->
      r) ->
    r
  upstreamCtrl :: c -> UpstreamStall
  downstreamCtrl :: c -> DownstreamBackpressure
  expectedCyclesOf :: c -> Int

type family Suffix c :: Nat

type instance Suffix SHA3 = 2
type instance Suffix Shake = 4

instance HashCase SHA3 where
  withMessage (SHA3 (SomeMessage msg) _ _) f = f msg
  upstreamCtrl (SHA3 _ up _) = up
  downstreamCtrl (SHA3 _ _ down) = down
  expectedCyclesOf = expectedCycles

instance HashCase Shake where
  withMessage (Shake (ShakeSomeMessage msg) _ _) f = f msg
  upstreamCtrl (Shake _ up _) = up
  downstreamCtrl (Shake _ _ down) = down
  expectedCyclesOf = shakeExpectedCycles

toActualResultWith ::
  HashCase c =>
  Dut ->
  c ->
  Result
toActualResultWith dut testCase =
  withMessage testCase $ \(message :: Vec (beats * 64) Bit) ->
    let messageWords = bitCoerce message :: Vec beats (BitVector 64)
        inputStream =
          withClockResetEnable clockGen resetGen enableGen $
            feedInput (upstreamCtrl testCase) messageWords
        treadyPattern = backpressureToList (downstreamCtrl testCase)
        treadySignal = fromList (treadyPattern <> P.repeat True)
        output =
          dut
            clockGen
            resetGen
            enableGen
            treadySignal
            inputStream
        sampleCount = expectedCyclesOf testCase
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

toExpectedResultWith ::
  forall c.
  HashCase c =>
  (forall beats n.
     ( KnownNat beats,
       KnownNat (beats * 64),
       SpongeParameter 1600 1088 n (beats * 64 + Suffix c) 0 256
     ) =>
     Vec (beats * 64) Bit ->
     DigestWords) ->
  c ->
  Result
toExpectedResultWith digestFn testCase =
  withMessage testCase $ \(messageBits :: Vec (beats * 64) Bit) ->
    let sampleCount = expectedCyclesOf testCase
        digestStart = max 0 (sampleCount - 4)
        digestEnd = min sampleCount (digestStart + 4)
        followingStart = digestEnd
        expected = digestFn @beats messageBits
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
            mkTlast i = i == sampleCount - 1
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

-- | Predict the number of cycles with upstream stall and downstream backpressure support
--   Current formula: absorbCycles + 24×(⌊beatCount/17⌋ + 1) + squeezeCycles
expectedCycles :: SHA3 -> Int
expectedCycles (SHA3 (SomeMessage (_ :: Vec (beats * 64) Bit)) upstreamControl downstreamControl) =
  expectedCyclesFor (natToNum @beats) upstreamControl downstreamControl

shakeExpectedCycles :: Shake -> Int
shakeExpectedCycles (Shake (ShakeSomeMessage (_ :: Vec (beats * 64) Bit)) upstreamControl downstreamControl) =
  expectedCyclesFor (natToNum @beats) upstreamControl downstreamControl

expectedCyclesFor ::
  Int ->
  UpstreamStall ->
  DownstreamBackpressure ->
  Int
expectedCyclesFor beatCount upstreamControl downstreamControl =
  let absorbCycles =
        case upstreamControl of
          NoUpstreamStall -> beatCount
          UpstreamStall pattern -> countAbsorbCycles beatCount pattern
      permuteCycles = permuteLatency * (beatCount `div` beatsPerBlock + 1)
      squeezeCycles =
        case downstreamControl of
          NoDownstreamBackpressure -> squeezeLatency
          DownstreamBackpressure pattern ->
            let squeezeStart = absorbCycles + permuteCycles
                relevantPattern = P.drop squeezeStart pattern
             in countSqueezeCycles squeezeLatency relevantPattern
   in absorbCycles + permuteCycles + squeezeCycles

-- | Use structural induction on the stall pattern list
countAbsorbCycles :: Int -> [Bool] -> Int
countAbsorbCycles 0 _ = 0 -- base case: no beats needed
countAbsorbCycles n [] = n -- base case: no more pattern, assume all True
countAbsorbCycles n (True : rest) = 1 + countAbsorbCycles (n - 1) rest -- absorbed one beat
countAbsorbCycles n (False : rest) = 1 + countAbsorbCycles n rest -- stalled, no progress

-- | Count cycles needed to output all squeeze beats with backpressure
countSqueezeCycles :: Int -> [Bool] -> Int
countSqueezeCycles 0 _ = 0        -- base case: all beats sent
countSqueezeCycles n [] = n       -- base case: no more pattern, assume all True
countSqueezeCycles n (True : rest) = 1 + countSqueezeCycles (n - 1) rest  -- sent one beat
countSqueezeCycles n (False : rest) = 1 + countSqueezeCycles n rest       -- stalled, no progress

runHashTestCase ::
  forall c.
  HashCase c =>
  Dut ->
  (forall beats n.
     ( KnownNat beats,
       KnownNat (beats * 64),
       SpongeParameter 1600 1088 n (beats * 64 + Suffix c) 0 256
     ) =>
     Vec (beats * 64) Bit ->
     DigestWords) ->
  c ->
  Expectation
runHashTestCase dut digestFn testCase = do
  let actualResult = toActualResultWith dut testCase
      expectedResult = toExpectedResultWith digestFn testCase
  compareResult actualResult expectedResult

runTestCase :: SHA3 -> Expectation
runTestCase =
  runHashTestCase
    SHA3256.topEntity
    (bitCoerce . SHA3.sha3_256)

runShakeTestCase :: Shake -> Expectation
runShakeTestCase =
  runHashTestCase
    SHAKE256.topEntity
    (bitCoerce . SHA3.shake_256 @_ @256)

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
        moreMsg = (["  ... and " <> show (P.length diffs - 10) <> " more differences" | hasMore])
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
        moreMsg = (["  ... and " <> show (P.length diffs - 10) <> " more differences" | hasMore])
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
        moreMsg = (["  ... and " <> show (P.length diffs - 10) <> " more differences" | hasMore])
    expectationFailure $ P.unlines $
      [prefix <> "Last mismatch (" <> summary <> "):"]
      <> diffLines
      <> moreMsg

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
                        blockCompleted = emittedNow == beatsPerBlock
                        needGap = blockCompleted && not isLast
                        nextWait = if needGap then permuteLatency else 0
                        nextEmitted =
                          if blockCompleted
                            then 0
                            else emittedNow
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
