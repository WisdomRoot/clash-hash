{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Constants (spec) where

import Clash.Prelude (Bit, Index, Vec)
import Permutation.Constants qualified
import qualified Reference.SHA3internal as SHA3internal
import Test.Hspec
import Prelude

spec :: Spec
spec = describe "Constants" $ do
  it "iota round constants match reference" $ do
    let expected = SHA3internal._iota_constants :: Vec 24 (Vec 64 Bit)
    let actual = $(Permutation.Constants.iotaReversed) :: Vec 24 (Vec 64 Bit)
    actual `shouldBe` expected

  it "theta 6 reversed matches reference" $ do
    let expected = fmap (fmap (1599 -)) (SHA3internal.theta_constants (SHA3internal.sha3_constants @6 @64 @1600))
    let actual = $(Permutation.Constants.thetaReversed 6) :: Vec 1600 (Vec 11 (Index 1600))
    actual `shouldBe` expected

  -- it "chi 6" $ do
  --   let expected = $(Permutation.Constants.chi 6)
  --   let actual = Permutation.Constants.chi6
  --   actual `shouldBe` expected

  it "chi 6 reversed" $ do
    let expected = fmap (\(i, j, k) -> (1599 - i, 1599 - j, 1599 - k)) $(Permutation.Constants.chi 6)
    let actual = Permutation.Constants.chi6Reversed
    actual `shouldBe` expected

  -- it "pi 6" $ do
  --   let expected = $(Permutation.Constants.pi 6)
  --   let actual = Permutation.Constants.pi6
  --   actual `shouldBe` expected

  it "pi 6 reversed" $ do
    let expected = fmap (1599 -) $(Permutation.Constants.pi 6)
    let actual = Permutation.Constants.pi6Reversed
    actual `shouldBe` expected

  -- it "rho 6" $ do
  --   let expected = $(Permutation.Constants.rho 6)
  --   let actual = Permutation.Constants.rho6
  --   actual `shouldBe` expected

  it "rho 6 reversed" $ do
    let expected = fmap (1599 -) $(Permutation.Constants.rho 6)
    let actual = Permutation.Constants.rho6Reversed
    actual `shouldBe` expected
