{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.NonPipelined.SHA3512NormalG (spec) where

import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Prelude
import Test.Hspec
import Test.QuickCheck
import Test.TestHarness.SHA3512NormalG
import Test.TestHarness.SHAKESamples qualified as Samples
import Test.TestHarness.SHAKECommon (makeBasicTest, makeBackpressureTest, makeCombinedTest, makeStallTest)

basicCases :: [SHA3512NormalGTest]
basicCases =
  [ makeBasicTest BS.empty 32,
    makeBasicTest "qwertyui" 32,
    makeBasicTest Samples.msg576 32,
    makeBasicTest Samples.msg1152 32,
    makeBasicTest Samples.msg1600 32,
    makeBasicTest Samples.msg3200 32
  ]

stallCases :: [SHA3512NormalGTest]
stallCases =
  [ makeStallTest Samples.msg576 32 Samples.stallPatternAggressive,
    makeStallTest Samples.msg1152 32 Samples.stallPatternModerate,
    makeStallTest Samples.msg1600 32 Samples.stallPatternSimple
  ]

backpressureCases :: [SHA3512NormalGTest]
backpressureCases =
  [ makeBackpressureTest Samples.msg576 32 Samples.backpressurePatternSimple,
    makeBackpressureTest Samples.msg1152 32 Samples.backpressurePatternModerate,
    makeCombinedTest
      Samples.msg576
      32
      Samples.stallPatternAggressive
      Samples.backpressurePatternAggressive,
    makeCombinedTest
      Samples.msg1600
      32
      Samples.stallPatternModerate
      Samples.backpressurePatternSimple
  ]

spec :: Spec
spec = describe "Component G (256-bit output) Tests" $ do
  let emptyFlushCase = makeBasicTest BS.empty 32

  it "0-bit input, 256-bit output (flush-only)" $ runTest emptyFlushCase

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
        forAll sha3512NormalGGen runTest
