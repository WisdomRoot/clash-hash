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

  it "rho 6" $ do
    let expected = Permutation.Constants.rho6
    let actual = $(Permutation.Constants.rho 6)
    actual `shouldBe` expected
