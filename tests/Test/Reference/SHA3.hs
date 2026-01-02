{-# LANGUAGE OverloadedStrings #-}

module Test.Reference.SHA3 (spec) where

import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (for_)
import Prelude (String, ($))
import Reference.Hash qualified as Hash
import Reference.Crypton qualified as Crypton
import Test.Hspec

spec :: Spec
spec = describe "Reference SHA3-256 Tests" $ do
    for_ testCases $ \(label, input) ->
      it label $ do
        let cryptonResult = Crypton.sha3 input
            hashResult = Hash.sha3_256BS input
        hashResult `shouldBe` cryptonResult

testCases :: [(String, BS8.ByteString)]
testCases =
  [ ("Empty input", BS8.empty),
    ("Input: qwertyui", "qwertyui"),
    ("Input: qwertyuiopasdfgh", "qwertyuiopasdfgh"),
    ("Input: test", "test"),
    ("Input: The quick brown fox jumps over the lazy dog", "The quick brown fox jumps over the lazy dog")
  ]