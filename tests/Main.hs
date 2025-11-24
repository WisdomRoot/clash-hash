module Main (main) where

import Prelude
import qualified Test.SHA3
import qualified Test.Sponge.Properties
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  pureSpongeTests <- testSpec "Pure Sponge (Phase 1)" Test.Sponge.Properties.spec
  sha3DebugTests <- testSpec "SHA3-256 debug" Test.SHA3.debug
  defaultMain $ testGroup "All Tests"
    [ pureSpongeTests
    , sha3DebugTests
    ]
