module Test.SampleNTT512 (spec) where

import Data.Foldable (for_)
import Prelude
import Test.Hspec
import Test.QuickCheck
import Test.TestHarness.SampleNTT512 (runTest, testLabel)
import Test.TestHarness.SampleNTTSamples qualified as Samples

spec :: Spec
spec = describe "NonPipelined SampleNTT512 Tests" $ do
  describe "Basic functionality tests (34-byte seeds)" $
    for_ Samples.basicSeedCases $
      \testCase -> it (testLabel testCase) $ runTest testCase

  describe "Upstream stall handling (34-byte seeds)" $
    for_ Samples.stallSeedCases $
      \testCase -> it (testLabel testCase) $ runTest testCase

  describe "Downstream backpressure handling (34-byte seeds)" $
    for_ Samples.backpressureSeedCases $
      \testCase -> it (testLabel testCase) $ runTest testCase

  describe "Combined stress tests (34-byte seeds)" $
    for_ Samples.combinedSeedCases $
      \testCase -> it (testLabel testCase) $ runTest testCase

  describe "QuickCheck property tests (34-byte seeds)" $
    it "correctly handles random 34-byte test cases" $
      withMaxSuccess 40 $ forAll Samples.genSampleNTTTest runTest
