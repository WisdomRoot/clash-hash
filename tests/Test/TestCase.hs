{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Test.TestCase
  ( TestCase (TestCase),
    testCaseLabel,
    SomeMessage (..),
    UpstreamStall (..),
    runTestCase,
    try,
    expectedCycles,
    expectedCycles2,
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Hash.NonPipelined qualified
import Reference.SHA3 (SpongeParameter)
import Reference.SHA3 qualified as SHA3
import Test.Hspec
import Test.QuickCheck
import Prelude qualified as P

data SomeMessage where
  SomeMessage :: (KnownNat beats) => Vec (beats * 64) Bit -> SomeMessage

data TestCase
  = TestCase
      SomeMessage
      (Vec 4 (BitVector 64))
      UpstreamStall

data UpstreamStall
  = NoUpstreamStall
  | UpstreamStall [Bool]

instance Arbitrary UpstreamStall where
  arbitrary =
    frequency
      [ (3, pure NoUpstreamStall),
        (2, UpstreamStall <$> genPattern)
      ]
    where
      genPattern = do
        len <- chooseInt (0, 32)
        vectorOf len arbitrary

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

-- | Predict the number of cycles needed to complete a test case
expectedCycles :: TestCase -> Int
expectedCycles (TestCase (SomeMessage (_ :: Vec (beats * 64) Bit)) _ _) =
  let beatCount = natToNum @beats :: Int
      absorbCycles = beatCount
      permuteCycles = (24 + 1) * ((beatCount `div` 17) + 1) -- extra cycle of stall, to be removed
      squeezeCycles = 4
   in absorbCycles + permuteCycles + squeezeCycles

-- | Predict the number of cycles with upstream stall support (using structural induction)
expectedCycles2 :: TestCase -> Int
expectedCycles2 (TestCase (SomeMessage (_ :: Vec (beats * 64) Bit)) _ control) =
  let beatCount = natToNum @beats :: Int
      -- Absorb phase: actual time to get all beats from upstream
      absorbCycles = case control of
        NoUpstreamStall -> beatCount
        UpstreamStall pattern -> countAbsorbCycles beatCount pattern

      -- Permute and squeeze are independent of upstream stalls
      permuteCycles = (24 + 1) * ((beatCount `div` 17) + 1)
      squeezeCycles = 4
   in absorbCycles + permuteCycles + squeezeCycles

-- | Use structural induction on the stall pattern list
countAbsorbCycles :: Int -> [Bool] -> Int
countAbsorbCycles 0 _ = 0 -- base case: no beats needed
countAbsorbCycles n [] = n -- base case: no more pattern, assume all True
countAbsorbCycles n (True : rest) = 1 + countAbsorbCycles (n - 1) rest -- absorbed one beat
countAbsorbCycles n (False : rest) = 1 + countAbsorbCycles n rest -- stalled, no progress

try :: TestCase -> Expectation
try testCase@(TestCase (SomeMessage (message :: Vec (beats * 64) Bit)) _expected control) = do
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
      samples = sampleN @System sampleCount output

      outputs = P.drop (sampleCount - 4) samples

  print sampleCount

  print outputs

runTestCase ::
  TestCase ->
  Expectation
runTestCase testCase@(TestCase (SomeMessage (message :: Vec (beats * 64) Bit)) expected control) = do
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
      samples = sampleN @System sampleCount output
      actualStreams = P.take 4 $ P.filter (tvalid . fst) samples

  fmap (tdata . fst) actualStreams `shouldBe` toList expected
  fmap (tvalid . fst) actualStreams `shouldBe` P.replicate 4 True
  fmap (tlast . fst) actualStreams `shouldBe` [False, False, False, True]

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
      let (canSend, ctrl') =
            case ctrl of
              [] -> (True, [])
              b : bs -> (b, bs)
       in if waitCount > 0
            then ((xs, waitCount - 1, emittedInBlock, ctrl'), idleBeat)
            else
              if not canSend
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
