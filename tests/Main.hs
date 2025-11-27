module Main (main) where

import Prelude
import qualified Test.Constants
import qualified Test.Permutation
import qualified Test.Sponge
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  -- Phase 0: Verification tests
  -- constantsTests <- testSpec "Constants" Test.Constants.spec
  -- permutationTests <- testSpec "Permutation" Test.Permutation.spec
  spongeTests <- testSpec "Sponge" Test.Sponge.spec

  defaultMain $ testGroup "All Tests"
    [
    --   constantsTests
    -- , permutationTests
    -- ,
    spongeTests
    ]
