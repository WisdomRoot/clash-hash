{-# LANGUAGE OverloadedStrings #-}

module Test.Reference.SampleNTT (spec) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Vector qualified as V
import Data.Word (Word16)
import Reference.SampleNTT
import Test.Hspec
import Prelude

spec :: Spec
spec = describe "Reference SampleNTT Tests" $ do
  basicSanityTests
  knownVectorTests
  propertyTests

-- | Basic sanity tests to verify the implementation works
basicSanityTests :: Spec
basicSanityTests = describe "Basic Sanity Tests" $ do
  it "produces exactly 256 coefficients" $ do
    let seed = "test_seed_1"
    let result = sampleNTT seed
    V.length result `shouldBe` 256

  it "all coefficients are in valid range [0, 3329)" $ do
    let seed = "test_seed_2"
    let result = sampleNTT seed
    let allValid = V.all (< q) result
    allValid `shouldBe` True

  it "is deterministic (same seed produces same output)" $ do
    let seed = "determinism_test"
    let result1 = sampleNTT seed
    let result2 = sampleNTT seed
    result1 `shouldBe` result2

  it "different seeds produce different outputs" $ do
    let seed1 = "seed_one"
    let seed2 = "seed_two"
    let result1 = sampleNTT seed1
    let result2 = sampleNTT seed2
    result1 `shouldNotBe` result2

-- | Known test vectors from C2SP/CCTV
-- TODO: Add actual test vectors from https://github.com/C2SP/CCTV/tree/main/ML-KEM
knownVectorTests :: Spec
knownVectorTests = describe "Known Vector Tests" $ do
  -- TODO: Add hardcoded test vectors from C2SP/CCTV
  pure ()

-- | Property-based tests
propertyTests :: Spec
propertyTests = describe "Property Tests" $ do
  it "coefficient range property" $ do
    -- Test multiple seeds to ensure range is always valid
    let seeds = ["prop1", "prop2", "prop3", "prop4", "prop5"]
    let results = map sampleNTT seeds
    let allInRange = all (\vec -> V.all (< q) vec) results
    allInRange `shouldBe` True

  it "determinism property" $ do
    -- Test that determinism holds for multiple seeds
    let seeds = ["det1", "det2", "det3"]
    let checkDeterminism seed =
          let r1 = sampleNTT seed
              r2 = sampleNTT seed
           in r1 == r2
    let allDeterministic = all checkDeterminism seeds
    allDeterministic `shouldBe` True
