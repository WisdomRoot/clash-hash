module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)
import Test.NTT qualified as NTT

main :: IO ()
main = do
  nttTests <- testSpec "NTT" NTT.spec
  defaultMain (testGroup "All Tests" [nttTests])
