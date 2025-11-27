{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Sponge (spec) where

import Clash.Prelude
import qualified KeccakF1600.Permutation as Perm
import qualified Sponge.Pure
import Test.Hspec

spec :: Spec
spec = fdescribe "Sponge (Pure)" $ do
  let sha3Suffix = 0b01 :: BitVector 2

  it "SHA3-256 hash of 'abc'" $ do
    -- "abc" = 0x616263 (24 bits)
    let msg = 0x616263 :: BitVector 24
    let actual = Sponge.Pure.pureSponge @1600 @1088 @256 sha3Suffix Perm.keccakF1600 msg
    let expected = 0x3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532 :: BitVector 256
    actual `shouldBe` expected

  it "SHA3-256 hash of empty string" $ do
    -- Empty message (0 bits)
    let msg = 0 :: BitVector 0
    let actual = Sponge.Pure.pureSponge @1600 @1088 @256 sha3Suffix Perm.keccakF1600 msg
    let expected = 0xa7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a :: BitVector 256
    actual `shouldBe` expected
