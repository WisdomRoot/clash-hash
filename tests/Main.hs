{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main (main) where

import Test.Combinational qualified
import Test.Constants qualified
import Test.Permutation qualified
import Test.Stateful qualified
import Test.Tasty
import Test.Tasty.Hspec
import Prelude

main :: IO ()
main = do
  constantsTests <- testSpec "Constants" Test.Constants.spec
  permutationTests <- testSpec "Permutation" Test.Permutation.spec
  combinationalTests <- testSpec "Combinational" Test.Combinational.spec
  statefulTests <- testSpec "Stateful" Test.Stateful.spec

  defaultMain $
    testGroup
      "All Tests"
      [
        constantsTests,
        permutationTests,
        combinationalTests,
        statefulTests
      ]
