{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.NonPipelined.SHAKE128 (spec) where

import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (for_)
import Prelude (($))
import Test.Hspec
import Test.QuickCheck
import Test.TestHarness.SHAKE128

spec :: Spec
spec = describe "NonPipelined SHAKE-128 Tests" $ do

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
        property $ \(testCase :: SHAKE128Test) -> runTest testCase

-- | Basic test cases covering SHAKE128-specific boundaries
testCases :: [SHAKE128Test]
testCases =
  [ makeBasicTest BS8.empty 32,
    makeBasicTest "qwertyui" 32,
    makeBasicTest msg1344 32,
    makeBasicTest msg2016 32,
    makeBasicTest msg2688 64
  ]

-- | Variable output length coverage
testCasesVariableOutput :: [SHAKE128Test]
testCasesVariableOutput =
  [ makeVariableOutputTest "minimal8" 8,
    makeVariableOutputTest "16byte_test_data" 40,
    makeVariableOutputTest "testdata" 256
  ]

-- | Upstream stall scenarios
testCasesWithStalls :: [SHAKE128Test]
testCasesWithStalls =
  [ makeStallTest msg1344 64 stallPatternAggressive,
    makeStallTest msg2016 32 stallPatternModerate
  ]

-- | Downstream backpressure scenarios
testCasesWithBackpressure :: [SHAKE128Test]
testCasesWithBackpressure =
  [ makeBackpressureTest msg1344 128 backpressurePatternSimple,
    makeCombinedTest
      msg1408
      256
      stallPatternSimple
      backpressurePatternAggressive
  ]
