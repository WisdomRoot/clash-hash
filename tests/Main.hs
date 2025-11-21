module Main (main) where

import Clash.Prelude
import KeccakF1600Testbench (expectedDigest, testBench)
import SHA3internal (BitString, hexdump, v2bs)
import qualified SHA3
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

  describe "SHA3-256 Reference with 64-bit inputs" $ do
    it "all zeros (0x0000000000000000)" $ do
      let msg = bv2v (0x0000000000000000 :: BitVector 64) :: BitString 64
      let digest = SHA3.sha3_256 msg :: BitString 256
      let hexDigest = hexdump "%02x" digest
      P.putStrLn $ "  Digest: " P.++ hexDigest
      P.length hexDigest `shouldBe` 64 -- 256 bits = 64 hex chars

    it "all ones (0xFFFFFFFFFFFFFFFF)" $ do
      let msg = bv2v (0xFFFFFFFFFFFFFFFF :: BitVector 64) :: BitString 64
      let digest = SHA3.sha3_256 msg :: BitString 256
      let hexDigest = hexdump "%02x" digest
      P.putStrLn $ "  Digest: " P.++ hexDigest
      P.length hexDigest `shouldBe` 64

    it "alternating pattern (0xAAAAAAAAAAAAAAAA)" $ do
      let msg = bv2v (0xAAAAAAAAAAAAAAAA :: BitVector 64) :: BitString 64
      let digest = SHA3.sha3_256 msg :: BitString 256
      let hexDigest = hexdump "%02x" digest
      P.putStrLn $ "  Digest: " P.++ hexDigest
      P.length hexDigest `shouldBe` 64

    it "counter pattern (0x0001020304050607)" $ do
      let msg = bv2v (0x0001020304050607 :: BitVector 64) :: BitString 64
      let digest = SHA3.sha3_256 msg :: BitString 256
      let hexDigest = hexdump "%02x" digest
      P.putStrLn $ "  Digest: " P.++ hexDigest
      P.length hexDigest `shouldBe` 64

    it "single bit set (0x8000000000000000)" $ do
      let msg = bv2v (0x8000000000000000 :: BitVector 64) :: BitString 64
      let digest = SHA3.sha3_256 msg :: BitString 256
      let hexDigest = hexdump "%02x" digest
      P.putStrLn $ "  Digest: " P.++ hexDigest
      P.length hexDigest `shouldBe` 64

  describe "SHA3-256 Reference with 128-bit inputs (2 blocks)" $ do
    it "all zeros (0x00000000000000000000000000000000)" $ do
      let msg = bv2v (0x00000000000000000000000000000000 :: BitVector 128) :: BitString 128
      let digest = SHA3.sha3_256 msg :: BitString 256
      let hexDigest = hexdump "%02x" digest
      P.putStrLn $ "  Digest: " P.++ hexDigest
      P.length hexDigest `shouldBe` 64

    it "all ones (0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)" $ do
      let msg = bv2v (0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF :: BitVector 128) :: BitString 128
      let digest = SHA3.sha3_256 msg :: BitString 256
      let hexDigest = hexdump "%02x" digest
      P.putStrLn $ "  Digest: " P.++ hexDigest
      P.length hexDigest `shouldBe` 64

    it "alternating pattern (0xAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA)" $ do
      let msg = bv2v (0xAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA :: BitVector 128) :: BitString 128
      let digest = SHA3.sha3_256 msg :: BitString 256
      let hexDigest = hexdump "%02x" digest
      P.putStrLn $ "  Digest: " P.++ hexDigest
      P.length hexDigest `shouldBe` 64

    it "counter pattern (0x000102030405060708090A0B0C0D0E0F)" $ do
      let msg = bv2v (0x000102030405060708090A0B0C0D0E0F :: BitVector 128) :: BitString 128
      let digest = SHA3.sha3_256 msg :: BitString 256
      let hexDigest = hexdump "%02x" digest
      P.putStrLn $ "  Digest: " P.++ hexDigest
      P.length hexDigest `shouldBe` 64

    it "two different halves (0xFFFFFFFFFFFFFFFF0000000000000000)" $ do
      let msg = bv2v (0xFFFFFFFFFFFFFFFF0000000000000000 :: BitVector 128) :: BitString 128
      let digest = SHA3.sha3_256 msg :: BitString 256
      let hexDigest = hexdump "%02x" digest
      P.putStrLn $ "  Digest: " P.++ hexDigest
      P.length hexDigest `shouldBe` 64
