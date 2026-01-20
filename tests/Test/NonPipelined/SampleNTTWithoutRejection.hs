{-# LANGUAGE OverloadedStrings #-}

module Test.NonPipelined.SampleNTTWithoutRejection (spec) where

import Test.Hspec
import Test.TestHarness.SampleNTTWithoutRejection (runTest)
import Test.TestHarness.SampleNTTSamples qualified as Samples
import Prelude

spec :: Spec
spec = describe "SampleNTT without rejection" $ do
  it "matches sample_ntt_without_rejection.py for first 10 chunks" $
    runTest Samples.seed1
