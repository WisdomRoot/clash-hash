{-# LANGUAGE OverloadedStrings #-}

module Test.Reference.SHAKE256 (spec) where

import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (for_)
import Prelude (String, ($))
import Prelude qualified as P
import Reference.Hash qualified as Hash
import Reference.Crypton qualified as Crypton
import Test.Hspec

spec :: Spec
spec = describe "Reference SHAKE256 Tests" $ do
    for_ testCases $ \(label, outputBytes, input) ->
      it label $ do
        let cryptonResult = Crypton.shake256 outputBytes input
            hashResult = Hash.shake256BS outputBytes input
        hashResult `shouldBe` cryptonResult

testCases :: [(String, P.Int, BS8.ByteString)]
testCases =
  [ ("Empty input, 32-byte output", 32, BS8.empty),
    ("8-byte input, 32-byte output", 32, "qwertyui"),
    ("16-byte input, 64-byte output", 64, "qwertyuiopasdfgh"),
    ("Small output (16 bytes)", 16, "test"),
    ("Large output (128 bytes)", 128, "large output test")
  ]