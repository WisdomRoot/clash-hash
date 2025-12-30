module Test.Reference.SHAKE256 (spec) where

import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (for_)
import Prelude (Int, String, ($))
import Reference.SHAKE256Runtime (shake256, shake256Native)
import Test.Hspec

spec :: Spec
spec = describe "Reference SHAKE256 Tests" $ do
    for_ testCases $ \(label, outputBytes, input) ->
      it label $ do
        let cryptonResult = shake256 outputBytes input
            nativeResult = shake256Native outputBytes input
        nativeResult `shouldBe` cryptonResult

testCases :: [(String, Int, BS8.ByteString)]
testCases =
  [ ("Empty input, 32-byte output", 32, BS8.empty),
    ("8-byte input, 32-byte output", 32, BS8.pack "qwertyui"),
    ("16-byte input, 64-byte output", 64, BS8.pack "qwertyuiopasdfgh"),
    ("Small output (16 bytes)", 16, BS8.pack "test"),
    ("Large output (128 bytes)", 128, BS8.pack "large output test")
  ]
