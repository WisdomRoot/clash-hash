{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}

module Test.NTT (spec) where

import Clash.Prelude qualified as C
import Component.NTT qualified as NTT
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude qualified as P

spec :: Spec
spec = describe "Component.NTT" P.$ do
  describe "butterfly" P.$ do
    it "computes butterfly correctly with inputs (1, 1, 1)" P.$ do
      let (outA, outB) = NTT.butterfly (1, 1, 1)
      outA `shouldBe` 2
      outB `shouldBe` 0

  describe "ntt256" P.$ do
    it "computes ntt256 on all-zero input correctly" P.$ do
      let zetas = C.repeat 0
          input = C.repeat 0
          output = NTT.ntt256 zetas input
      C.toList output `shouldBe` P.replicate 256 0
