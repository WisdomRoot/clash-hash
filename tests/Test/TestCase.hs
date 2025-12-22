{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Test.TestCase
  ( TestCase (TestCase),
    testCaseLabel,
    SomeMessage (..),
    UpstreamStall (..),
    runTestCase,
    expectedCycles,
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
--   Current formula: 
--      if beat < 17 
--          then beatCount + 25×(⌊beatCount/17⌋ + 1) + 4
--          else beatCount + 25×(⌊beatCount/17⌋ + 1) + 5
expectedCycles :: TestCase -> Int
expectedCycles (TestCase (SomeMessage (_ :: Vec (beats * 64) Bit)) _ control) =
  let beatCount = natToNum @beats :: Int
      -- Absorb phase: actual time to get all beats from upstream
      absorbCycles = case control of
        NoUpstreamStall -> beatCount
        UpstreamStall pattern -> countAbsorbCycles beatCount pattern

      -- Permute and squeeze are independent of upstream stalls
      permuteCycles = (24 + 1) * ((beatCount `div` 17) + 1)
      squeezeCycles = 4
   in if beatCount < 17 
        then absorbCycles + permuteCycles + squeezeCycles
        else absorbCycles + permuteCycles + squeezeCycles + 1

-- | Use structural induction on the stall pattern list
countAbsorbCycles :: Int -> [Bool] -> Int
countAbsorbCycles 0 _ = 0 -- base case: no beats needed
countAbsorbCycles n [] = n -- base case: no more pattern, assume all True
countAbsorbCycles n (True : rest) = 1 + countAbsorbCycles (n - 1) rest -- absorbed one beat
countAbsorbCycles n (False : rest) = 1 + countAbsorbCycles n rest -- stalled, no progress


runTestCase :: TestCase -> Expectation
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

      -- Extract all 4 wires for all cycles
      actualTdata = fmap (tdata . fst) samples
      actualTvalid = fmap (tvalid . fst) samples
      actualTlast = fmap (tlast . fst) samples
      actualTready = fmap snd samples

      -- Build expected values for all 4 wires for all cycles
      [d0, d1, d2, d3] = toList expected
      cycles = [0 .. sampleCount - 1]

      -- Outputs appear at the last 5 cycles: d0, d1, d2, d3, then final idle
      expectedTdata = fmap mkTdata cycles
        where
          mkTdata i
            | i == sampleCount - 5 = d0
            | i == sampleCount - 4 = d1
            | i == sampleCount - 3 = d2
            | i == sampleCount - 2 = d3
            | otherwise = 0

      expectedTvalid = fmap mkTvalid cycles
        where
          mkTvalid i
            | i >= sampleCount - 5 && i <= sampleCount - 2 = True
            | otherwise = False

      expectedTlast = fmap mkTlast cycles
        where
          mkTlast i
            | i == sampleCount - 2 = True
            | otherwise = False

  -- Match ALL output wires cycle-by-cycle
  actualTdata `shouldBe` expectedTdata
  actualTvalid `shouldBe` expectedTvalid
  actualTlast `shouldBe` expectedTlast
  -- TODO: enable tready check once FSM state model is complete
  -- actualTready `shouldBe` expectedTready

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
