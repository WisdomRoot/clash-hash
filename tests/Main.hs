module Main (main) where

import Prelude
import qualified Test.Constants
import qualified Test.Sponge.Ordering
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  -- Phase 0: Verification tests
  constantsTests <- testSpec "Constants" Test.Constants.spec
  orderingTests <- testSpec "Permutation Ordering" Test.Sponge.Ordering.spec

  -- Phase 1: Pure sponge tests (DISABLED until Phase 0 complete)
  -- pureSpongeTests <- testSpec "Pure Sponge (Phase 1)" Test.Sponge.Properties.spec

  defaultMain $ testGroup "All Tests"
    [ constantsTests
    , orderingTests
    -- , pureSpongeTests
    ]
