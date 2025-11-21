module Main (main) where

import Prelude (IO)
import KeccakF1600Spec (spec)
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  hspecTests <- testSpec "KeccakF1600" spec
  defaultMain hspecTests
