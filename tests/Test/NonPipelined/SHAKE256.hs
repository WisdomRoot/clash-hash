{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.NonPipelined.SHAKE256 (spec) where

import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (for_)
import Prelude (($))
import Test.Hspec
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

-- | Basic test cases with various input sizes, all with 32-byte (256-bit) output
testCases :: [SHAKE256Test]
testCases =
  [ -- Empty input
    makeBasicTest BS8.empty 32,
    -- Single 64-bit word (8 bytes)
    makeBasicTest "qwertyui" 32,
    -- Two 64-bit words (16 bytes)
    makeBasicTest "qwertyuiopasdfgh" 32,
    -- 1024 bits (128 bytes) - fits in one rate block (rate=1088)
    makeBasicTest msg1024 32,
    -- 1088 bits (136 bytes) - exactly one rate block
    makeBasicTest msg1088 32,
    -- 1600 bits (200 bytes) - full Keccak state, needs two absorb cycles
    makeBasicTest msg1600 32,
    -- 3200 bits (400 bytes) - multiple blocks
    makeBasicTest msg3200 32
  ]

-- | Test cases with variable output lengths (key feature of SHAKE256)
testCasesVariableOutput :: [SHAKE256Test]
testCasesVariableOutput =
  [ -- 16-byte output (128 bits)
    makeVariableOutputTest "testdata" 16,
    -- 32-byte output (256 bits) - standard
    makeVariableOutputTest "testdata" 32,
    -- 64-byte output (512 bits)
    makeVariableOutputTest "testdata" 64,
    -- 128-byte output (1024 bits)
    makeVariableOutputTest "testdata" 128,
    -- 256-byte output (2048 bits) - multiple squeeze cycles
    makeVariableOutputTest "testdata" 256,
    -- Edge case: 1-byte output (input must be 8 bytes)
    makeVariableOutputTest "minimal8" 1,
    -- Edge case: odd number of output bytes (input must be multiple of 8)
    makeVariableOutputTest "16byte_test_data" 37
  ]

-- | Test cases with upstream stalls (tests absorb phase resilience)
testCasesWithStalls :: [SHAKE256Test]
testCasesWithStalls =
  [ -- Basic input with simple stall pattern
    makeStallTest "qwertyui" 32 stallPatternSimple,
    -- Longer input with moderate stalls
    makeStallTest "qwertyuiopasdfgh" 32 stallPatternModerate,
    -- Multi-block with aggressive stalls
    makeStallTest msg1088 32 stallPatternAggressive
  ]

-- | Test cases with downstream backpressure (tests squeeze phase resilience)
testCasesWithBackpressure :: [SHAKE256Test]
testCasesWithBackpressure =
  [ -- Basic test with simple backpressure
    makeBackpressureTest "qwertyui" 32 backpressurePatternSimple,
    -- Variable output with moderate backpressure
    makeBackpressureTest "testdata" 64 backpressurePatternModerate,
    -- Large output with aggressive backpressure
    makeBackpressureTest "largeoutput_test" 128 backpressurePatternAggressive,
    -- Combined: both stalls and backpressure
    makeCombinedTest
      "qwertyuiopasdfgh"
      32
      stallPatternSimple
      backpressurePatternSimple
  ]
