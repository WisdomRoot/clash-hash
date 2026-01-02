{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.NonPipelined.SHA3256 (spec) where

import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (for_)
import Prelude (($))
import Test.Hspec
import Test.QuickCheck
import Test.TestHarness.SHA3256

spec :: Spec
spec = describe "NonPipelined SHA3-256 Tests" $ do

  describe "Basic functionality tests" $ do
    for_ testCases $ \testCase ->
      it (testLabel testCase) $ runTest testCase

  describe "Upstream stall handling" $ do
    for_ testCasesWithStalls $ \testCase ->
      it (testLabel testCase) $ runTest testCase

  describe "Downstream backpressure handling" $ do
    for_ testCasesWithBackpressure $ \testCase ->
      it (testLabel testCase) $ runTest testCase

  describe "QuickCheck property tests" $ do
    it "correctly handles random test cases" $
      withMaxSuccess 10 $
        property $ \(testCase :: SHA3256Test) -> runTest testCase

-- | Basic test cases - critical boundary conditions
-- (QuickCheck now covers intermediate sizes)
testCases :: [SHA3256Test]
testCases =
  [ -- Single 64-bit word (8 bytes) - minimum
    makeBasicTest "qwertyui",
    -- 1088 bits (136 bytes) - exactly one rate block
    makeBasicTest msg1088,
    -- 1600 bits (200 bytes) - full Keccak state
    makeBasicTest msg1600,
    -- 3200 bits (400 bytes) - multiple blocks
    makeBasicTest msg3200
  ]

-- | Test cases with upstream stalls
-- (QuickCheck now covers most stall scenarios)
testCasesWithStalls :: [SHA3256Test]
testCasesWithStalls =
  [ -- Multi-block with aggressive stalls - stress test
    makeStallTest msg1088 stallPatternAggressive
  ]

-- | Test cases with downstream backpressure
-- (QuickCheck now covers most backpressure scenarios)
testCasesWithBackpressure :: [SHA3256Test]
testCasesWithBackpressure =
  [ -- Combined: both stalls and backpressure - comprehensive test
    makeCombinedTest
      msg1088
      stallPatternAggressive
      backpressurePatternAggressive
  ]
