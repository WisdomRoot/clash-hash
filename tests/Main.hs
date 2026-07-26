module Main (main) where

import Test.Hspec (hspec)
import Test.NTT qualified as NTT

main :: IO ()
main = hspec NTT.spec
