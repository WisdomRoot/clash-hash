{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.NonPipelined.SHAKE256 (spec) where

import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (for_)
import Prelude (($))
import Test.Hspec
import Test.QuickCheck
import Test.TestHarness.SHAKE256

spec :: Spec
spec = describe "NonPipelined SHAKE-256 Tests" $ do

  describe "Basic functionality tests" $ do
    for_ testCases $ \testCase ->
      it (testLabel testCase) $ runTest testCase

  describe "Variable output length tests" $ do
    for_ testCasesVariableOutput $ \testCase ->
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
        property $ \(testCase :: SHAKE256Test) -> runTest testCase

-- | Basic test cases - critical boundary conditions
-- (QuickCheck now covers intermediate sizes and variable outputs)
testCases :: [SHAKE256Test]
testCases =
  [ -- Empty input (SHAKE256-specific)
    makeBasicTest BS8.empty 32,
    -- Single 64-bit word (8 bytes) - minimum
    makeBasicTest "qwertyui" 32,
    -- 1088 bits (136 bytes) - exactly one rate block
    makeBasicTest msg1088 32,
    -- 1600 bits (200 bytes) - full Keccak state
    makeBasicTest msg1600 32,
    -- 3200 bits (400 bytes) - multiple blocks
    makeBasicTest msg3200 32
  ]

-- | Test cases with variable output lengths
-- (QuickCheck now covers many output sizes)
-- NOTE: All outputs must be multiples of 8 bytes (64 bits) per hardware spec
testCasesVariableOutput :: [SHAKE256Test]
testCasesVariableOutput =
  [ -- Edge case: 8-byte output (minimum, 1 beat)
    makeVariableOutputTest "minimal8" 8,
    -- Medium output (5 beats = 40 bytes)
    makeVariableOutputTest "16byte_test_data" 40,
    -- Large output requiring multiple squeeze cycles (32 beats)
    makeVariableOutputTest "testdata" 256
  ]

-- | Test cases with upstream stalls
-- (QuickCheck now covers most stall scenarios)
testCasesWithStalls :: [SHAKE256Test]
testCasesWithStalls =
  [ -- Multi-block with aggressive stalls - stress test
    makeStallTest msg1088 32 stallPatternAggressive
  ]

-- | Test cases with downstream backpressure
-- (QuickCheck now covers most backpressure scenarios)
testCasesWithBackpressure :: [SHAKE256Test]
testCasesWithBackpressure =
  [ -- Combined: stalls + backpressure + large output - comprehensive test
    makeCombinedTest
      msg1088
      256
      stallPatternAggressive
      backpressurePatternAggressive
  ]
