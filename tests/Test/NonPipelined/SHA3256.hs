{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.NonPipelined.SHA3256 (spec) where

import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (for_)
import Prelude (($))
import Test.Hspec
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

-- | Basic test cases with various input sizes
testCases :: [SHA3256Test]
testCases =
  [ -- Single 64-bit word (8 bytes)
    makeBasicTest "qwertyui",
    -- Two 64-bit words (16 bytes)
    makeBasicTest "qwertyuiopasdfgh",
    -- 1024 bits (128 bytes) - fits in one rate block (rate=1088)
    makeBasicTest msg1024,
    -- 1088 bits (136 bytes) - exactly one rate block
    makeBasicTest msg1088,
    -- 1600 bits (200 bytes) - full Keccak state, needs two absorb cycles
    makeBasicTest msg1600,
    -- 3200 bits (400 bytes) - multiple blocks
    makeBasicTest msg3200
  ]

-- | Test cases with upstream stalls (tests absorb phase resilience)
testCasesWithStalls :: [SHA3256Test]
testCasesWithStalls =
  [ -- Basic input with simple stall pattern
    makeStallTest "qwertyui" stallPatternSimple,
    -- Longer input with moderate stalls
    makeStallTest "qwertyuiopasdfgh" stallPatternModerate,
    -- Multi-block with aggressive stalls
    makeStallTest msg1088 stallPatternAggressive
  ]

-- | Test cases with downstream backpressure (tests squeeze phase resilience)
testCasesWithBackpressure :: [SHA3256Test]
testCasesWithBackpressure =
  [ -- Basic test with simple backpressure
    makeBackpressureTest "qwertyui" backpressurePatternSimple,
    -- Longer input with moderate backpressure
    makeBackpressureTest "qwertyuiopasdfgh" backpressurePatternModerate,
    -- Large output with aggressive backpressure
    makeBackpressureTest msg1024 backpressurePatternAggressive,
    -- Combined: both stalls and backpressure
    makeCombinedTest
      "qwertyuiopasdfgh"
      stallPatternSimple
      backpressurePatternSimple
  ]
