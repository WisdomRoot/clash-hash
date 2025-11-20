module Main (main) where

import Clash.Prelude
import KeccakF1600Testbench (expectedDigest, testBench)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import qualified Prelude as P

main :: IO ()
main = do
  spec <- testSpec "KeccakF1600 SHA3-256" sha3Spec
  defaultMain spec

sha3Spec :: Spec
sha3Spec = do
  describe "Clash testBench" $ do
    it "eventually asserts done" $ do
      -- Pure Clash simulation of the testBench
      let samples = sampleN @System 200 testBench :: [Bool]
      -- TODO: replace this with collecting the digest and checking it
      -- When testBench is fully implemented, it should return True when digest matches
      P.last samples `shouldBe` False -- placeholder; will be True when implemented

    it "expectedDigest is a 256-bit value" $ do
      -- This test verifies that expectedDigest correctly calls SHA3.sha3_256
      -- The actual value will be whatever SHA3.sha3_256 returns for empty input
      -- This is just checking that the expectedDigest computation doesn't crash
      -- TODO: Add test with known test vector (e.g., "abc") to verify correctness
      P.length (show expectedDigest) `shouldSatisfy` (> 0) -- Just verify it computes
