{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Stateful (spec) where

import Clash.Prelude
import qualified Hash.Stateful as Stateful
import qualified Reference.SHA3 as SHA3
import qualified Reference.SHA3internal as SHA3internal
import Test.Hspec

spec :: Spec
spec = describe "Stateful SHA3-256 Tests" $ do
  step0Tests

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
