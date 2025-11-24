module Main (main) where

import Prelude (IO)
import qualified Test.SHA3
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  hspecTests <- testSpec "SHA3-256" Test.SHA3.debug
  defaultMain hspecTests
