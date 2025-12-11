{-# LANGUAGE DataKinds #-}
{-# HLINT ignore "Use head" #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Stateful (spec) where

import AXI4Stream (AXI4Stream (..))
import Clash.Explicit.Testbench
import Clash.Prelude hiding (tlast)
import Data.Foldable (for_)
import Hash.Stateful4 qualified
import Hash.Stateful5 qualified
import Hash.Stateful6 qualified
import Hash.Stateful7 qualified
import Reference.SHA3 qualified as SHA3
import Reference.SHA3internal qualified as SHA3internal
import Sponge.Stateful qualified as Stateful
import Test.Hspec
import Prelude qualified as P

spec :: Spec
spec = describe "Stateful SHA3-256 Tests" $ do
  -- step0Tests
  -- step1Tests
  -- step2Tests
  -- step3Tests
  -- step4Tests
  -- step5Tests
  s7Tests

-- ============================================================================
-- Step 0: Baseline - Verify Hash.Combinational.truncate works
-- ============================================================================

step0Tests :: Spec
step0Tests = describe "Step 0: Baseline (copy of truncated)" $ it "stateful0 = truncated = SHA3.sha3_256 for 'abc'" $ do
  let msg = SHA3internal.toBitString $(listToVecTH "abc")
  let expected = SHA3.sha3_256 msg
  let actual = Stateful.stateful0 @256 @24 msg
  actual `shouldBe` expected

-- ============================================================================
-- Step 1: Registered version - Add clock domain
-- ============================================================================

step1Tests :: Spec
step1Tests = describe "Step 1: Registered (add clock domain)" $ it "stateful1 (registered) = SHA3.sha3_256 for 'abc'" $ do
  let msg = SHA3internal.toBitString $(listToVecTH "abc")
  let expected = SHA3.sha3_256 msg

  -- Create signal with message
  let msgSig = pure msg :: Signal System (Vec 24 Bit)
  let digestSig =
        withClockResetEnable clockGen resetGen enableGen
          $ Stateful.stateful1 @System @256 @24 msgSig

  -- Sample 3 cycles (register adds 1-cycle delay)
  -- Cycle 0: input arrives, register outputs initial value (zeros)
  -- Cycle 1: register outputs computed digest
  let samples = sampleN @System 3 digestSig
  let actual = samples P.!! 2 -- Take 3rd sample (index 2, after 2 cycles)
  actual `shouldBe` expected

-- ============================================================================
-- Step 2: Mealy machine with trivial state
-- ============================================================================

step2Tests :: Spec
step2Tests = describe "Step 2: Mealy machine (trivial state)" $ it "stateful2 (Mealy with trivial state) = SHA3.sha3_256 for 'abc'" $ do
  let msg = SHA3internal.toBitString $(listToVecTH "abc")
  let expected = SHA3.sha3_256 msg

  -- Create signal with message
  let msgSig = pure msg :: Signal System (Vec 24 Bit)
  let digestSig =
        withClockResetEnable clockGen resetGen enableGen
          $ Stateful.stateful2 @System @256 @24 msgSig

  -- Sample 3 cycles (Mealy has 1-cycle latency like register)
  let samples = sampleN @System 3 digestSig
  let actual = samples P.!! 2

  actual `shouldBe` expected

-- ============================================================================
-- Step 3: State machine with iteration counter
-- ============================================================================

step3Tests :: Spec
step3Tests = describe "Step 3: State machine (1 iteration with full permutation)" $ it "stateful3 (state machine with 1 iteration) = SHA3.sha3_256 for 'abc'" $ do
  let msg = SHA3internal.toBitString $(listToVecTH "abc")
  let expected = SHA3.sha3_256 msg

  -- Mealy machine timing (from debug output):
  -- Sample 0: output zeros (initial cycle)
  -- Sample 1: output zeros (cnt=0 path: absorbing)
  -- Sample 2: output digest (cnt=1 path: extracting)
  -- Sample 3: output zeros (reset, cnt=0 path again)
  -- Sample 4: output digest (cnt=1 path again)
  let msgSig = pure msg :: Signal System (Vec 24 Bit)
  let digestSig =
        withClockResetEnable clockGen resetGen enableGen
          $ Stateful.stateful3 @System @256 @24 msgSig

  -- Digest appears at sample 2 (index 2)
  let samples = sampleN @System 3 digestSig
  let actual = samples P.!! 2 -- Take 3rd sample (index 2)
  actual `shouldBe` expected

-- ============================================================================
-- Step 4: Single-round permutation with 24 iterations
-- ============================================================================

step4Tests :: Spec
step4Tests = describe "Hash.Stateful4.topEntity" $ it "Hash.Stateful4.topEntity = SHA3.sha3_256 for 1084-bit message (single block)" $ do
  -- NOTE: stateful4 only works correctly for single-block messages
  -- For single block: msgBits + 4 (padding) = 1088, so msgBits = 1084 (exact fit)
  -- Using 1084 bits = 135.5 bytes = 135 full bytes + 4 bits
  -- Pattern: "abcdefgh" (8 bytes) × 16 = 128 bytes, + "abcdefg" = 135 bytes = 1080 bits, + 0x4 = 1084 bits
  let msg =
        SHA3internal.toBitString $(listToVecTH "7867cffe3cd4818ed6f8e861e712238ffe23046f0e639f647f4edfcf23761b9ecadc1e45aa5adbb9580ddd5affaff1e00ee09176fc6f15eeb229e3c236ba331chdyanzq")
          ++ 0
          :> 1
          :> 0
          :> 0
          :> Nil -- Add 4 bits to make 1084 bits total
  let expected = SHA3.sha3_256 msg

  -- Convert to BitVector for hardware input
  let msgBV = pack msg :: BitVector 1084
  let msgSig = pure msgBV :: Signal System (BitVector 1084)

  -- Call topEntity
  let digestSig = Hash.Stateful4.topEntity clockGen resetGen enableGen msgSig

  -- Need 26 samples: initial + absorb (1) + 24 rounds
  let samples = sampleN @System 26 digestSig
  let actualBV = samples P.!! 25
  let actual = unpack actualBV :: Vec 256 Bit

  actual `shouldBe` expected

-- ============================================================================
-- Step 5: Duplicate of step4 under Hash.Stateful5
-- ============================================================================

step5Tests :: Spec
step5Tests = describe "Hash.Stateful5.topEntity" $ it "Hash.Stateful5.topEntity = SHA3.sha3_256 for 1084-bit multi-beat absorption" $ do
  let msg =
        SHA3internal.toBitString $(listToVecTH "7867cffe3cd4818ed6f8e861e712238ffe23046f0e639f647f4edfcf23761b9ecadc1e45aa5adbb9580ddd5affaff1e00ee09176fc6f15eeb229e3c236ba331chdyanzq")
          ++ 0
          :> 1
          :> 0
          :> 0
          :> Nil
  let expected = pack (SHA3.sha3_256 msg) :: BitVector 256

  -- Split into 17 beats
  let msgBV = pack msg :: BitVector 1084
  let msg1024 :: BitVector 1024
      msg60 :: BitVector 60
      (msg1024, msg60) = split msgBV
  let beats0_15 = bitCoerce msg1024 :: Vec 16 (BitVector 64)
  let beat16 = msg60 ++# 0 :: BitVector 64
  let inputBeats = beats0_15 :< beat16

  -- Create testbench using stimuliGenerator
  let testInput = stimuliGenerator clockGen resetGen inputBeats
  let output = Hash.Stateful5.topEntity clockGen resetGen enableGen testInput

  -- Sample outputs and check manually
  let samples = sampleN @System 50 output
  let actual = samples P.!! 41

  actual `shouldBe` expected

-- | Helper function for S6 backpressure tests
--   Returns (expected digest as Vec 4, sampled output)
runS6WithBackpressure ::
  forall n.
  (KnownNat n) =>
  Vec n Bool -> -- tready signal pattern
  Int -> -- number of samples to take
  (Vec 4 (BitVector 64), [AXI4Stream 64])
runS6WithBackpressure treadySignals numSamples =
  let msg =
        SHA3internal.toBitString $(listToVecTH "7867cffe3cd4818ed6f8e861e712238ffe23046f0e639f647f4edfcf23761b9ecadc1e45aa5adbb9580ddd5affaff1e00ee09176fc6f15eeb229e3c236ba331chdyanzq")
          ++ 0
          :> 1
          :> 0
          :> 0
          :> Nil
      expected = bitCoerce (SHA3.sha3_256 msg) :: Vec 4 (BitVector 64)

      -- Setup input
      msgBV = pack msg :: BitVector 1084
      msg1024 :: BitVector 1024
      msg60 :: BitVector 60
      (msg1024, msg60) = split msgBV
      beats0_15 = bitCoerce msg1024 :: Vec 16 (BitVector 64)
      beat16 = msg60 ++# 0 :: BitVector 64
      inputSignals = beats0_15 :< beat16

      output =
        Hash.Stateful6.topEntity
          clockGen
          resetGen
          enableGen
          (stimuliGenerator clockGen resetGen treadySignals)
          (stimuliGenerator clockGen resetGen inputSignals)

      samples = sampleN @System numSamples output
   in (expected, samples)

s6Tests :: Spec
s6Tests = describe "Hash.Stateful6.topEntity" $ do
  it "Hash.Stateful6.topEntity = SHA3.sha3_256 for 1084-bit multi-beat absorption + multi-beat scatter" $ do
    let msg =
          SHA3internal.toBitString $(listToVecTH "7867cffe3cd4818ed6f8e861e712238ffe23046f0e639f647f4edfcf23761b9ecadc1e45aa5adbb9580ddd5affaff1e00ee09176fc6f15eeb229e3c236ba331chdyanzq")
            ++ 0
            :> 1
            :> 0
            :> 0
            :> Nil
    let expected = pack (SHA3.sha3_256 msg) :: BitVector 256

    -- Split into 17 beats
    let msgBV = pack msg :: BitVector 1084
    let msg1024 :: BitVector 1024
        msg60 :: BitVector 60
        (msg1024, msg60) = split msgBV
    let beats0_15 = bitCoerce msg1024 :: Vec 16 (BitVector 64)
    let beat16 = msg60 ++# 0 :: BitVector 64
    let inputBeats = beats0_15 :< beat16

    -- Create testbench using stimuliGenerator
    let testInput = stimuliGenerator clockGen resetGen inputBeats
    let treadyAlwaysHigh = pure True -- No backpressure
    let output = Hash.Stateful6.topEntity clockGen resetGen enableGen treadyAlwaysHigh testInput

    -- Sample outputs and check manually
    let samples = sampleN @System 50 output
    let actual = tdata (samples P.!! 42) ++# tdata (samples P.!! 43) ++# tdata (samples P.!! 44) ++# tdata (samples P.!! 45)

    actual `shouldBe` expected

  it "complete backpressure - tready always False (stalls in Squeeze 0)" $ do
    let msg =
          SHA3internal.toBitString $(listToVecTH "7867cffe3cd4818ed6f8e861e712238ffe23046f0e639f647f4edfcf23761b9ecadc1e45aa5adbb9580ddd5affaff1e00ee09176fc6f15eeb229e3c236ba331chdyanzq")
            ++ 0
            :> 1
            :> 0
            :> 0
            :> Nil
    let expected = pack (SHA3.sha3_256 msg) :: BitVector 256

    -- Setup input (same as above)
    let msgBV = pack msg :: BitVector 1084
    let msg1024 :: BitVector 1024
        msg60 :: BitVector 60
        (msg1024, msg60) = split msgBV
    let beats0_15 = bitCoerce msg1024 :: Vec 16 (BitVector 64)
    let beat16 = msg60 ++# 0 :: BitVector 64
    let inputBeats = beats0_15 :< beat16

    let testInput = stimuliGenerator clockGen resetGen inputBeats
    let treadyAlwaysLow = pure False -- Complete backpressure
    let output = Hash.Stateful6.topEntity clockGen resetGen enableGen treadyAlwaysLow testInput

    -- Sample 100 cycles to observe stalling
    let samples = sampleN @System 100 output

    -- Verify state enters Squeeze 0 at cycle 42 (17 absorb + 24 permute + 1)
    let squeeze0Start = samples P.!! 42
    tvalid squeeze0Start `shouldBe` True
    tlast squeeze0Start `shouldBe` False

    -- Verify it stays stalled in Squeeze 0 (same tdata for many cycles)
    let squeeze0Data = tdata squeeze0Start
    let laterCycles = [samples P.!! i | i <- [43 .. 60]]
    P.all (\s -> tdata s == squeeze0Data && tvalid s && P.not (tlast s)) laterCycles `shouldBe` True

    -- Verify first beat matches expected (bits [1599:1536] = first 64 bits of digest)
    let expectedBeat0 = slice (SNat @255) (SNat @192) expected :: BitVector 64
    squeeze0Data `shouldBe` expectedBeat0

  it "Stall on the second output" $ do
    let treadySignals = replicate d41 True ++ True :> False :> True :> True :> Nil :: Vec 45 Bool
    let (expected, samples) = runS6WithBackpressure treadySignals 50
    let actual = P.take 5 $ P.drop 42 samples

    -- Verify outputs with stall on second beat
    fmap tdata actual
      `shouldBe` [ expected !! (0 :: Integer),
                   expected !! (1 :: Integer),
                   expected !! (1 :: Integer), -- stall
                   expected !! (2 :: Integer),
                   expected !! (3 :: Integer)
                 ]

    -- Verify tvalid correctness
    fmap tvalid actual
      `shouldBe` [ True,
                   True,
                   True, -- still valid during stall
                   True,
                   True
                 ]

  it "Stall on the first output" $ do
    -- Stall on first output: False at index 41 (to stall output at cycle 42)
    let treadySignals = replicate d41 True ++ False :> True :> True :> True :> Nil :: Vec 45 Bool
    let (expected, samples) = runS6WithBackpressure treadySignals 50
    let actual = P.take 5 $ P.drop 42 samples

    -- Verify outputs with stall on first beat
    fmap tdata actual
      `shouldBe` [ expected !! (0 :: Integer),
                   expected !! (0 :: Integer), -- stall
                   expected !! (1 :: Integer),
                   expected !! (2 :: Integer),
                   expected !! (3 :: Integer)
                 ]

    -- Verify tvalid stays True
    fmap tvalid actual `shouldBe` [True, True, True, True, True]

    -- Verify tlast only on the actual last beat (index 4)
    fmap tlast actual `shouldBe` [False, False, False, False, True]

  it "Stall on the third output" $ do
    -- Stall on third output: False at index 43 (to stall output at cycle 44)
    let treadySignals = replicate d43 True ++ False :> True :> True :> Nil :: Vec 46 Bool
    let (expected, samples) = runS6WithBackpressure treadySignals 50
    let actual = P.take 5 $ P.drop 42 samples

    -- Verify outputs with stall on third beat
    fmap tdata actual
      `shouldBe` [ expected !! (0 :: Integer),
                   expected !! (1 :: Integer),
                   expected !! (2 :: Integer),
                   expected !! (2 :: Integer), -- stall
                   expected !! (3 :: Integer)
                 ]

    fmap tvalid actual `shouldBe` [True, True, True, True, True]
    fmap tlast actual `shouldBe` [False, False, False, False, True]

  it "Stall on the fourth (last) output" $ do
    -- Stall on fourth output for multiple cycles: extend stall period
    let treadySignals = replicate d45 True ++ False :> False :> False :> False :> False :> True :> Nil :: Vec 51 Bool
    let (expected, samples) = runS6WithBackpressure treadySignals 50
    let actual = P.take 6 $ P.drop 42 samples

    -- Verify outputs - note: after last squeeze completes, transitions to idle (tdata=0, tvalid=False)
    fmap tdata (P.take 4 actual)
      `shouldBe` [ expected !! (0 :: Integer),
                   expected !! (1 :: Integer),
                   expected !! (2 :: Integer),
                   expected !! (3 :: Integer)
                 ]

    fmap tvalid (P.take 4 actual) `shouldBe` [True, True, True, True]
    fmap tlast (P.take 4 actual) `shouldBe` [False, False, False, True]

  it "Multiple stalls - stall on first and third outputs" $ do
    -- Stall on first and third: False at indices 41 (for cycle 42) and 44 (for cycle 44), plus one more False to keep in Squeeze 3
    let treadySignals = replicate d41 True ++ False :> True :> True :> False :> True :> False :> True :> Nil :: Vec 48 Bool
    let (expected, samples) = runS6WithBackpressure treadySignals 50
    let actual = P.take 7 $ P.drop 42 samples

    -- Verify outputs with two stalls
    fmap tdata actual
      `shouldBe` [ expected !! (0 :: Integer),
                   expected !! (0 :: Integer), -- first stall
                   expected !! (1 :: Integer),
                   expected !! (2 :: Integer),
                   expected !! (2 :: Integer), -- second stall
                   expected !! (3 :: Integer),
                   expected !! (3 :: Integer) -- stays at last after completion
                 ]

    fmap tvalid actual `shouldBe` [True, True, True, True, True, True, True]

  it "Long stall - stall first output for 3 cycles" $ do
    -- Stall for 3 cycles on first output: False at indices 41, 42, 43
    let treadySignals = replicate d41 True ++ False :> False :> False :> True :> True :> True :> Nil :: Vec 47 Bool
    let (expected, samples) = runS6WithBackpressure treadySignals 50
    let actual = P.take 7 $ P.drop 42 samples

    -- Verify outputs with 3-cycle stall on first beat
    fmap tdata actual
      `shouldBe` [ expected !! (0 :: Integer),
                   expected !! (0 :: Integer), -- stall cycle 1
                   expected !! (0 :: Integer), -- stall cycle 2
                   expected !! (0 :: Integer), -- stall cycle 3
                   expected !! (1 :: Integer),
                   expected !! (2 :: Integer),
                   expected !! (3 :: Integer)
                 ]

    fmap tvalid actual `shouldBe` [True, True, True, True, True, True, True]
    fmap tlast actual `shouldBe` [False, False, False, False, False, False, True]

s7Tests :: Spec
s7Tests = describe "Hash.Stateful7.topEntity" $
  for_ s7TestCases $ \(S7TestCase label message expected control) ->
    it label $ runS7Case message expected control

data S7TestCase where
  S7TestCase ::
    KnownNat beats =>
    { s7Label :: String
    , s7Message :: Vec (beats * 64) Bit
    , s7Expected :: Vec 4 (BitVector 64)
    , s7InputControl :: S7InputControl
    } ->
    S7TestCase

data S7InputControl
  = NoUpstreamStall
  | UpstreamStall [Bool]

s7TestCases :: [S7TestCase]
s7TestCases =
  [ S7TestCase "64-bit input" msg64 expected64 NoUpstreamStall
  , S7TestCase "128-bit input" msg128 expected128 NoUpstreamStall
  , S7TestCase "1024-bit input" msg1024 expected1024 NoUpstreamStall
  , S7TestCase "1088-bit input" msg1088 expected1088 NoUpstreamStall
  , S7TestCase "1600-bit input" msg1600 expected1600 NoUpstreamStall
  , S7TestCase "128-bit input (upstream stalls)" msg128 expected128 (UpstreamStall stallPattern)
  ]
  where
    msg64 :: Vec (1 * 64) Bit
    msg64 = SHA3internal.toBitString $(listToVecTH "qwertyui")
    expected64 :: Vec 4 (BitVector 64)
    expected64 = bitCoerce (SHA3.sha3_256 msg64)

    msg128 :: Vec (2 * 64) Bit
    msg128 = SHA3internal.toBitString $(listToVecTH "qwertyuiopasdfgh")
    expected128 :: Vec 4 (BitVector 64)
    expected128 = bitCoerce (SHA3.sha3_256 msg128)

    msg1024 :: Vec (16 * 64) Bit
    msg1024 =
      SHA3internal.toBitString
        $(listToVecTH "qwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfgh")
    expected1024 :: Vec 4 (BitVector 64)
    expected1024 = bitCoerce (SHA3.sha3_256 msg1024)

    msg1088 :: Vec (17 * 64) Bit
    msg1088 =
      SHA3internal.toBitString
        $(listToVecTH "qwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyui")
    expected1088 :: Vec 4 (BitVector 64)
    expected1088 = bitCoerce (SHA3.sha3_256 msg1088)

    msg1600 :: Vec (25 * 64) Bit
    msg1600 =
      SHA3internal.toBitString
        $(listToVecTH "01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789")
    expected1600 :: Vec 4 (BitVector 64)
    expected1600 = bitCoerce (SHA3.sha3_256 msg1600)
    stallPattern :: [Bool]
    stallPattern =
      [ True
      , False
      , True
      , True
      , False
      , True
      , True
      , True
      , False
      , True
      , True
      , False
      , True
      , True
      , True
      ]

runS7Case ::
  forall beats.
  KnownNat beats =>
  Vec (beats * 64) Bit ->
  Vec 4 (BitVector 64) ->
  S7InputControl ->
  Expectation
runS7Case message expected control = do
  let messageWords = bitCoerce message :: Vec beats (BitVector 64)
      inputStream =
        withClockResetEnable clockGen resetGen enableGen $
          s7InputStream control messageWords
      output =
        Hash.Stateful7.topEntity
          clockGen
          resetGen
          enableGen
          (pure True)
          inputStream
      beatCount = natToNum @beats :: Int
      blockTransitions =
        if beatCount <= 0
          then 0
          else P.max 0 (beatCount - 1) `div` 17
      gapCycles = blockTransitions * 24
      permutationCount = blockTransitions + 1
      sampleCount = beatCount + gapCycles + 1 + permutationCount * 24 + 4 + 64
      samples = sampleN @System sampleCount output
      actualStreams = P.take 4 $ P.filter (tvalid . fst) samples

  fmap (tdata . fst) actualStreams `shouldBe` toList expected
  fmap (tvalid . fst) actualStreams `shouldBe` P.replicate 4 True
  fmap (tlast . fst) actualStreams `shouldBe` [False, False, False, True]

s7InputStream ::
  forall beats dom.
  ( KnownNat beats
  , HiddenClockResetEnable dom
  ) =>
  S7InputControl ->
  Vec beats (BitVector 64) ->
  Signal dom (AXI4Stream 64)
s7InputStream control messageWords =
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
                            { tdata = y
                            , tvalid = True
                            , tlast = isLast
                            }
                     in (nextState, outBeat)
    idleBeat =
      AXI4Stream
        { tdata = 0
        , tvalid = False
        , tlast = False
        }
