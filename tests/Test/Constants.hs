{-# LANGUAGE TemplateHaskell #-}

module Test.Constants (spec) where

import Clash.Prelude (Bit, Vec)
import Permutation.Constants qualified
import qualified Reference.SHA3internal as SHA3internal (_iota_constants)
import Test.Hspec
import Prelude

spec :: Spec
spec = describe "Constants" $ do
  it "iota round constants match reference" $ do
    let expected = SHA3internal._iota_constants :: Vec 24 (Vec 64 Bit)
    let actual = $(Permutation.Constants.iota) :: Vec 24 (Vec 64 Bit)
    actual `shouldBe` expected

  it "chi 6 reversed" $ do
    let expected = $(Permutation.Constants.chiReversed 6)
    let actual = Permutation.Constants.chi6Reversed
    actual `shouldBe` expected

  it "chi 6" $ do
    let expected = fmap (\(i, j, k) -> (1599 - i, 1599 - j, 1599 - k)) $(Permutation.Constants.chiReversed 6)
    let actual = Permutation.Constants.chi6
    actual `shouldBe` expected

  it "pi 6 reversed" $ do
    let expected = $(Permutation.Constants.piReversed 6)
    let actual = Permutation.Constants.pi6Reversed
    actual `shouldBe` expected

  it "pi 6" $ do
    let expected = fmap (1599 -) $(Permutation.Constants.piReversed 6)
    let actual = Permutation.Constants.pi6
    actual `shouldBe` expected

  it "rho 6 reversed" $ do
    let expected = $(Permutation.Constants.rhoReversed 6)
    let actual = Permutation.Constants.rho6Reversed
    actual `shouldBe` expected

  it "rho 6" $ do
    let expected = fmap (1599 -) $(Permutation.Constants.rhoReversed 6)
    let actual = Permutation.Constants.rho6
    actual `shouldBe` expected
