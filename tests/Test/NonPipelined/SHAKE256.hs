{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.NonPipelined.SHAKE256 (spec) where

import Data.Foldable (for_)
import Prelude (($), fmap)
import Test.Hspec
import Test.QuickCheck
import Test.TestHarness.SHAKE256
import Test.TestHarness.SHAKECommon qualified as Common
import Test.TestHarness.SHAKESamples qualified as Samples

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

fromShared :: [Common.ShakeTest] -> [SHAKE256Test]
fromShared = fmap SHAKE256Test

-- | Basic test cases - critical boundary conditions
testCases :: [SHAKE256Test]
testCases = fromShared Samples.basicCases

-- | Variable output length coverage
testCasesVariableOutput :: [SHAKE256Test]
testCasesVariableOutput = fromShared Samples.variableOutputCases

-- | Test cases with upstream stalls
testCasesWithStalls :: [SHAKE256Test]
testCasesWithStalls = fromShared Samples.stallCases

-- | Test cases with downstream backpressure
testCasesWithBackpressure :: [SHAKE256Test]
testCasesWithBackpressure = fromShared Samples.backpressureCases
