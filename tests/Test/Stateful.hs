{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Stateful (spec) where

import Clash.Prelude
import qualified Prelude as P
import qualified Sponge.Stateful as Stateful
import qualified Hash.Stateful4
import qualified Reference.SHA3 as SHA3
import qualified Reference.SHA3internal as SHA3internal
import Test.Hspec

spec :: Spec
spec = describe "Stateful SHA3-256 Tests" $ do
  step0Tests
  step1Tests
  step2Tests
  step3Tests
  step4Tests

-- ============================================================================
-- Step 0: Baseline - Verify Hash.Combinational.truncate works
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

-- step4Tests :: Spec
-- step4Tests = describe "Step 4: Single-round permutation (24 iterations)" $ do
--   it "stateful4 (24 single-round iterations) = SHA3.sha3_256 for 'abc'" $ do
--     let msg = SHA3internal.toBitString $(listToVecTH "abc")
--     let expected = SHA3.sha3_256 msg

--     -- State machine timing:
--     -- Sample 0: initial (cnt=0)
--     -- Sample 1: absorb, output zeros, next=(1, xorState)
--     -- Samples 2-25: rounds 1-24 (cnt 1-24), output zeros, next=(cnt+1, permuted)
--     -- Sample 26: done (cnt=25), output digest, next=(0, zeros)
--     -- Total: 27 samples (0-26), digest appears at sample 26
--     let msgSig = pure msg :: Signal System (Vec 24 Bit)
--     let permutationComponent input = fmap (uncurry Permutation.KeccakF1600.keccakF1600Round) input
--     let digestSig = withClockResetEnable clockGen resetGen enableGen $
--                       Stateful.stateful4 @System @256 @24 permutationComponent msgSig

--     -- Need 27 samples: initial + absorb (1) + 24 rounds + output (1)
--     let samples = sampleN @System 27 digestSig
--     let actual = samples P.!! 26  -- Take 27th sample (index 26)

--     actual `shouldBe` expected

-- ============================================================================
-- Step 4: Single-round permutation with 24 iterations
-- ============================================================================


step4Tests :: Spec
step4Tests = describe "Hash.Stateful4.topEntity" $ do
  it "Hash.Stateful4.topEntity = SHA3.sha3_256 for 1084-bit message (single block)" $ do
    -- NOTE: stateful4 only works correctly for single-block messages
    -- For single block: msgBits + 4 (padding) = 1088, so msgBits = 1084 (exact fit)
    -- Using 1084 bits = 135.5 bytes = 135 full bytes + 4 bits
    -- Pattern: "abcdefgh" (8 bytes) × 16 = 128 bytes, + "abcdefg" = 135 bytes = 1080 bits, + 0x4 = 1084 bits
    let msg = SHA3internal.toBitString $(listToVecTH "abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefg")
          ++ (0 :> 1 :> 0 :> 0 :> Nil)  -- Add 4 bits to make 1084 bits total
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
