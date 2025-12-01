{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Combinational (spec) where

import Clash.Prelude
import qualified Hash.Combinational as Comb
import qualified Reference.Combinational as Ref
import Test.Hspec
import qualified Reference.SHA3 as SHA3
import qualified Reference.SHA3internal as SHA3internal

spec :: Spec
spec = describe "Combinational SHA3" $ do
  describe "Step 1: pad" $ do
    it "Comb.padded = Ref.padded = pad for 'abc'" $ do
      let msg = SHA3internal.toBitString $(listToVecTH "abc")

      -- REFERENCE: pad
      let refResult = Ref.padded @24 @1 msg

      -- NEW: pad
      let ourResult = Comb.padded @24 msg

      ourResult `shouldBe` refResult

  describe "Step 2: absorb . pad" $ do
    it "Comb.absorbed = Ref.absorbed = absorb . pad for 'abc'" $ do
      let msg = SHA3internal.toBitString $(listToVecTH "abc")

      -- REFERENCE: absorb . pad using SHA3.keccakf
      let refResult = Ref.absorbed @24 @1 msg

      -- NEW: absorb . pad using keccakF1600
      let ourResult = Comb.absorbed @24 msg

      ourResult `shouldBe` refResult

  describe "Step 3: squeeze . absorb . pad" $ do
    it "Comb.squeezed = Ref.squeezed = squeeze . absorb . pad for 'abc'" $ do
      let msg = SHA3internal.toBitString $(listToVecTH "abc")

      -- REFERENCE: squeeze . absorb . pad using SHA3.keccakf
      let refResult = Ref.squeezed @24 @1 msg

      -- NEW: squeeze . absorb . pad using keccakF1600
      let ourResult = Comb.squeezed @24 msg

      ourResult `shouldBe` refResult

  describe "Step 4: trunc . squeeze . absorb . pad" $ do
    it "Comb.truncated = Ref.truncated = SHA3.sha3_256 for 'abc'" $ do
      let msg = SHA3internal.toBitString $(listToVecTH "abc")

      -- REFERENCE: full SHA3 sha3_256 digest
      let expected = SHA3.sha3_256 msg

      -- Reference decomposition
      let refResult = Ref.truncated @256 @24 @1 msg

      -- Incremental decomposition using keccakF1600
      let ourResult = Comb.truncated @256 @24 msg

      refResult `shouldBe` expected
      ourResult `shouldBe` refResult
