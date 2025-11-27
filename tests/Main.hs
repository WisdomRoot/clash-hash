module Main (main) where

import Prelude
import qualified Test.Constants
import qualified Test.Permutation
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  -- Phase 0: Verification tests
  constantsTests <- testSpec "Constants" Test.Constants.spec
  permutationTests <- testSpec "Permutation" Test.Permutation.spec

  -- Phase 1: Pure sponge tests (DISABLED until Phase 0 complete)
  -- pureSpongeTests <- testSpec "Pure Sponge (Phase 1)" Test.Sponge.Properties.spec

  defaultMain $ testGroup "All Tests"
    [ constantsTests
    , permutationTests
    -- , pureSpongeTests
    ]
