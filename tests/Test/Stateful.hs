{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Stateful (spec) where

import Clash.Prelude
import qualified Prelude as P
import qualified Hash.Stateful as Stateful
import qualified Reference.SHA3 as SHA3
import qualified Reference.SHA3internal as SHA3internal
import Test.Hspec

spec :: Spec
spec = describe "Stateful SHA3-256 Tests" $ do
  step0Tests
  step1Tests
  step2Tests
  step3Tests

-- ============================================================================
-- Step 0: Baseline - Verify Hash.Combinational.truncated works
-- ============================================================================

step0Tests :: Spec
step0Tests = describe "Step 0: Baseline (copy of truncated)" $ do
  it "stateful0 = truncated = SHA3.sha3_256 for 'abc'" $ do
    let msg = SHA3internal.toBitString $(listToVecTH "abc")
    let expected = SHA3.sha3_256 msg
    let actual = Stateful.stateful0 @256 @24 msg
    actual `shouldBe` expected

-- ============================================================================
-- Step 1: Registered version - Add clock domain
-- ============================================================================

step1Tests :: Spec
step1Tests = describe "Step 1: Registered (add clock domain)" $ do
  it "stateful1 (registered) = SHA3.sha3_256 for 'abc'" $ do
    let msg = SHA3internal.toBitString $(listToVecTH "abc")
    let expected = SHA3.sha3_256 msg

    -- Create signal with message
    let msgSig = pure msg :: Signal System (Vec 24 Bit)
    let digestSig = withClockResetEnable clockGen resetGen enableGen $
                      Stateful.stateful1 @System @256 @24 msgSig

    -- Sample 3 cycles (register adds 1-cycle delay)
    -- Cycle 0: input arrives, register outputs initial value (zeros)
    -- Cycle 1: register outputs computed digest
    let samples = sampleN @System 3 digestSig
    let actual = samples P.!! 2  -- Take 3rd sample (index 2, after 2 cycles)

    actual `shouldBe` expected

-- ============================================================================
-- Step 2: Mealy machine with trivial state
-- ============================================================================

step2Tests :: Spec
step2Tests = describe "Step 2: Mealy machine (trivial state)" $ do
  it "stateful2 (Mealy with trivial state) = SHA3.sha3_256 for 'abc'" $ do
    let msg = SHA3internal.toBitString $(listToVecTH "abc")
    let expected = SHA3.sha3_256 msg

    -- Create signal with message
    let msgSig = pure msg :: Signal System (Vec 24 Bit)
    let digestSig = withClockResetEnable clockGen resetGen enableGen $
                      Stateful.stateful2 @System @256 @24 msgSig

    -- Sample 3 cycles (Mealy has 1-cycle latency like register)
    let samples = sampleN @System 3 digestSig
    let actual = samples P.!! 2

    actual `shouldBe` expected

-- ============================================================================
-- Step 3: State machine with iteration counter
-- ============================================================================

step3Tests :: Spec
step3Tests = describe "Step 3: State machine (1 iteration with full permutation)" $ do
  it "stateful3 (state machine with 1 iteration) = SHA3.sha3_256 for 'abc'" $ do
    let msg = SHA3internal.toBitString $(listToVecTH "abc")
    let expected = SHA3.sha3_256 msg

    -- Mealy machine timing (from debug output):
    -- Sample 0: output zeros (initial cycle)
    -- Sample 1: output zeros (cnt=0 path: absorbing)
    -- Sample 2: output digest (cnt=1 path: extracting)
    -- Sample 3: output zeros (reset, cnt=0 path again)
    -- Sample 4: output digest (cnt=1 path again)
    let msgSig = pure msg :: Signal System (Vec 24 Bit)
    let digestSig = withClockResetEnable clockGen resetGen enableGen $
                      Stateful.stateful3 @System @256 @24 msgSig

    -- Digest appears at sample 2 (index 2)
    let samples = sampleN @System 3 digestSig
    let actual = samples P.!! 2  -- Take 3rd sample (index 2)

    actual `shouldBe` expected
