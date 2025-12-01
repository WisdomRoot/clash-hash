{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main (main) where

import Test.Combinational qualified
import Test.Complete qualified
import Test.Constants qualified
import Test.Permutation qualified
import Test.Tasty
import Test.Tasty.Hspec
import Prelude

main :: IO ()
main = do
  constantsTests <- testSpec "Constants" Test.Constants.spec
  permutationTests <- testSpec "Permutation" Test.Permutation.spec
  combinationalTests <- testSpec "Combinational" Test.Combinational.spec
  completeTests <- testSpec "Complete" Test.Complete.spec

  defaultMain $
    testGroup
      "All Tests"
      [ constantsTests,
        permutationTests,
        combinationalTests,
        completeTests
      ]
