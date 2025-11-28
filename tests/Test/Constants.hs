{-# LANGUAGE TemplateHaskell #-}

module Test.Constants (spec) where

import Clash.Prelude (Bit, Vec)
import Constants qualified
import SHA3internal qualified
import Test.Hspec
import Prelude

spec :: Spec
spec = describe "Constants" $ do
  it "iota round constants match reference" $ do
    let expected = SHA3internal._iota_constants :: Vec 24 (Vec 64 Bit)
    let actual = $(Constants.iota) :: Vec 24 (Vec 64 Bit)
    actual `shouldBe` expected

  it "rho 6" $ do
    let expected = Constants.rho6
    let actual = $(Constants.rho 6)
    actual `shouldBe` expected
