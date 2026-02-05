{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.NonPipelined.SHA3512NormalG768 (spec) where

import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Prelude
import Test.Hspec
import Test.QuickCheck
import Test.TestHarness.SHA3512NormalG768
import Test.TestHarness.SHAKESamples qualified as Samples
import Test.TestHarness.SHAKECommon (makeBasicTest, makeBackpressureTest, makeCombinedTest, makeStallTest)

basicCases :: [SHA3512NormalG768Test]
basicCases =
  [ makeBasicTest (BS.replicate 32 0x00) 32,
    makeBasicTest (BS.replicate 32 0x61) 32,
    makeBasicTest (BS.replicate 32 0xFF) 32
  ]

stallCases :: [SHA3512NormalG768Test]
stallCases =
  [ makeStallTest (BS.replicate 32 0x7E) 32 Samples.stallPatternAggressive,
    makeStallTest (BS.replicate 32 0x33) 32 Samples.stallPatternModerate,
    makeStallTest (BS.replicate 32 0xAA) 32 Samples.stallPatternSimple
  ]

backpressureCases :: [SHA3512NormalG768Test]
backpressureCases =
  [ makeBackpressureTest (BS.replicate 32 0x10) 32 Samples.backpressurePatternSimple,
    makeBackpressureTest (BS.replicate 32 0x22) 32 Samples.backpressurePatternModerate,
    makeCombinedTest
      (BS.replicate 32 0x55)
      32
      Samples.stallPatternAggressive
      Samples.backpressurePatternAggressive,
    makeCombinedTest
      (BS.replicate 32 0x99)
      32
      Samples.stallPatternModerate
      Samples.backpressurePatternSimple
  ]

spec :: Spec
spec = describe "Component G768 (256-bit output) Tests" $ do
  describe "Basic functionality tests" $
    for_ basicCases $
      \testCase -> it (testLabel testCase) $ runTest testCase

  describe "Upstream stall handling" $
    for_ stallCases $
      \testCase -> it (testLabel testCase) $ runTest testCase

  describe "Downstream backpressure handling" $
    for_ backpressureCases $
      \testCase -> it (testLabel testCase) $ runTest testCase

  describe "QuickCheck property tests" $
    it "correctly handles random test cases" $
      withMaxSuccess 10 $
        forAll sha3512NormalG768Gen runTest
