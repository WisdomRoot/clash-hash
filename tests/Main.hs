module Main (main) where

import Test.Hspec (hspec)
import Test.NTT256 qualified as NTT

main :: IO ()
main = hspec NTT.spec
