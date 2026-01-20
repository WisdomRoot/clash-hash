{-# LANGUAGE OverloadedStrings #-}

module Test.NonPipelined.SampleNTTCLI (spec) where

import Test.Hspec
import Test.TestHarness.SampleNTTCLI (runTest)
import Test.TestHarness.SampleNTTSamples qualified as Samples
import Prelude

spec :: Spec
spec = describe "SampleNTT CLI (raw SHAKE chunks)" $ do
  it "matches sample_ntt_cli.py for first 10 chunks" $
    runTest Samples.seed1
