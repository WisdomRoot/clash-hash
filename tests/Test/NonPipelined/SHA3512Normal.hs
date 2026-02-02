{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.NonPipelined.SHA3512Normal (spec) where

import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Prelude
import Test.Hspec
import Test.QuickCheck
import Test.TestHarness.SHA3512Normal
import Test.TestHarness.SHAKESamples qualified as Samples
import Test.TestHarness.SHAKECommon (makeBasicTest)

spec :: Spec
spec = describe "NonPipelined SHA3-512 (Normal) Tests" $ do
  let emptyFlushCase = makeBasicTest BS.empty 64

  it "0-bit input, 512-bit output (flush-only)" $ runTest emptyFlushCase

  describe "Basic functionality tests" $
    for_ Samples.sha3BasicCases $
      \testCase -> it (testLabel testCase) $ runTest testCase

  describe "Upstream stall handling" $
    for_ Samples.sha3StallCases $
      \testCase -> it (testLabel testCase) $ runTest testCase

  describe "Downstream backpressure handling" $
    for_ Samples.sha3BackpressureCases $
      \testCase -> it (testLabel testCase) $ runTest testCase

  describe "QuickCheck property tests" $
    it "correctly handles random test cases" $
      withMaxSuccess 10 $
        forAll sha3512NormalGen runTest
