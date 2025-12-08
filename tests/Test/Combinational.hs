{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Combinational (spec) where

import Clash.Prelude
import Hash.Combinational qualified as Comb
import Reference.Combinational qualified as Ref
import Reference.SHA3 qualified as SHA3
import Reference.SHA3internal qualified as SHA3internal
import Test.Hspec

spec :: Spec
spec = describe "Combinational SHA3" $ do
  it "Comb.truncate = Ref.truncate = SHA3.sha3_256 for 'abc'" $ do
    let msg = SHA3internal.toBitString $(listToVecTH "abc")

    -- REFERENCE: full SHA3 sha3_256 digest
    let expected = SHA3.sha3_256 msg

    -- Reference decomposition
    let refResult = Ref.truncate @256 @24 @1 msg

    -- Incremental decomposition using keccakF1600
    let ourResult = Comb.truncate @256 @24 msg

    refResult `shouldBe` expected
    ourResult `shouldBe` refResult
