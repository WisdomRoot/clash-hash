{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.NonPipelined.SHAKE128 (spec) where

import Data.Foldable (for_)
import Prelude (($), fmap)
import Test.Hspec
import Test.QuickCheck
import Test.TestHarness.SHAKE128
import Test.TestHarness.SHAKECommon qualified as Common
import Test.TestHarness.SHAKESamples qualified as Samples

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

fromShared :: [Common.ShakeTest] -> [SHAKE128Test]
fromShared = fmap SHAKE128Test

-- | Basic test cases (shared with SHAKE256)
testCases :: [SHAKE128Test]
testCases = fromShared Samples.basicCases

-- | Variable output length coverage
testCasesVariableOutput :: [SHAKE128Test]
testCasesVariableOutput = fromShared Samples.variableOutputCases

-- | Upstream stall scenarios
testCasesWithStalls :: [SHAKE128Test]
testCasesWithStalls = fromShared Samples.stallCases

-- | Downstream backpressure scenarios
testCasesWithBackpressure :: [SHAKE128Test]
testCasesWithBackpressure = fromShared Samples.backpressureCases
