module Main (main) where

import Prelude
import qualified Test.SHA3
import qualified Test.Sponge.Properties
import qualified Test.Sponge.Ordering
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  -- Phase 0: Bit-ordering verification (must pass completely before Phase 1)
  orderingTests <- testSpec "Bit-Ordering (Phase 0)" Test.Sponge.Ordering.spec

  -- Phase 1: Pure sponge tests (DISABLED until Phase 0 complete)
  -- pureSpongeTests <- testSpec "Pure Sponge (Phase 1)" Test.Sponge.Properties.spec

  defaultMain $ testGroup "All Tests"
    [ orderingTests
    -- , pureSpongeTests
    ]
