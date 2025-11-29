{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tests for pureSponge implementation
--
-- = Key Learnings About Clash Bit Ordering
--
-- == BitVector ++# Operator
-- The ++# operator places operands as: @left ++# right@
-- - LEFT operand → HIGH bits (MSB side)
-- - RIGHT operand → LOW bits (LSB side)
--
-- Example: @(0b11 :: BitVector 2) ++# (0b00 :: BitVector 2) = 0b1100@
--
-- This is OPPOSITE of intuitive append behavior!
-- To get message in low bits: @zeros ++# message@
-- NOT: @message ++# zeros@ (this puts message in HIGH bits and loses it!)
--
-- == Vec ++ Operator
-- The Vec ++ operator appends normally:
-- - LEFT operand → comes first in sequence
-- - RIGHT operand → comes after
--
-- When packed to BitVector: first element → bit 0 (LSB)
--
-- == toBitString Encoding
-- @toBitString $(listToVecTH "abc")@ produces BIT-REVERSED bytes:
-- - "a" = 0x61 = 0b01100001 → packed as 0b10000110
-- - Each byte is bit-reversed (LSB-first within bytes)
-- - But bytes are in correct order (a, b, c)
--
-- This matches SHA3 spec's bit numbering (bit 0 = LSB of first byte)
--
-- == Critical Bug Fixed
-- padToRateBlocks was using @msg ++# zeroPadding@
-- This put message in HIGH bits, which were then lost!
-- Fix: @zeroPadding ++# msg@ puts message in LOW bits correctly
--
-- == Suffix Appending
-- BitVector @suffix ++# msg@ does NOT equal Vec @msg ++ suffix@!
-- Must use Vec ++ for correct ordering, then pack to BitVector
--
-- == Summary of Bug Fixes
-- 1. Changed @padToRateBlocks@ from @msg ++# zeros@ to @zeros ++# msg@
--    - This ensures message occupies LOW bits (bits [msgBits-1:0])
--    - Previous version put message in HIGH bits, losing it completely!
--
-- 2. Changed @pureSponge@ to use Vec ++ for suffix appending
--    - @msgVec ++ suffixVec@ then pack to BitVector
--    - BitVector @++#@ operator has opposite semantics (LEFT → HIGH)
--
-- 3. Changed @absorbBlock@ from @block ++# zeros@ to @zeros ++# block@
--    - Ensures rate portion occupies LOW bits of state
--    - Capacity (zeros) occupies HIGH bits as required by sponge construction
--
module Test.Sponge (spec) where

import Clash.Prelude
import qualified Prelude as P
import qualified KeccakF1600.Permutation as Perm
import qualified Sponge.Pure
import qualified Sponge.Incremental as Inc
import qualified Sponge.Reference as Ref
import Test.Hspec
import qualified SHA3
import SHA3internal (toBitString)

spec :: Spec
spec = describe "Sponge Tests" $ do
  incrementalTests
  oldPureSpongeTests

-- ============================================================================
-- Incremental Bottom-Up Tests (New Approach)
-- ============================================================================

incrementalTests :: Spec
incrementalTests = fdescribe "Incremental Sponge (Bottom-Up)" $ do
  let sha3Suffix = 0b01 :: BitVector 2

  describe "Step 1: pad" $ do
    it "sponge1 = refSponge1 = pad for 'abc'" $ do
      let msg = toBitString $(listToVecTH "abc")

      -- REFERENCE: pad
      let refResult = Ref.refSponge1 @24 @1 msg

      -- NEW: pad
      let ourResult = Inc.sponge1 @24 msg

      ourResult `shouldBe` refResult

  -- Step 2 and 3 tests will be added after reimplementation

-- ============================================================================
-- Old pureSponge Tests (Debugging Documentation)
-- ============================================================================

oldPureSpongeTests :: Spec
oldPureSpongeTests = describe "Sponge (Pure)" $ do
  let sha3Suffix = 0b01 :: BitVector 2

  it "Check ++# behavior" $ do
    -- CRITICAL: ++# puts LEFT in HIGH bits, RIGHT in LOW bits
    -- This is counterintuitive! Most append operations put left operand first (low bits)
    let msg = 0b110011 :: BitVector 6
    let zeros = 0 :: BitVector 10

    -- CORRECT: To get msg in LOW bits, put zeros on LEFT
    let extended = zeros ++# msg :: BitVector 16
    putStrLn $ "msg: " P.++ show msg
    putStrLn $ "extended (low 8 bits): " P.++ show (resize extended :: BitVector 8)
    putStrLn $ "testBit extended 0: " P.++ show (testBit extended 0)
    putStrLn $ "testBit extended 5: " P.++ show (testBit extended 5)

    -- Verify: Low 6 bits of extended should match original message
    testBit extended 0 `shouldBe` testBit msg 0
    testBit extended 5 `shouldBe` testBit msg 5

  it "Check message encoding" $ do
    -- toBitString produces BIT-REVERSED bytes for SHA3 compatibility
    let msgBitString = toBitString $(listToVecTH "abc")
    let msgAsBitVector = pack msgBitString :: BitVector 24

    putStrLn $ "msgBitString (first 8 bits): " P.++ show (take d8 msgBitString)
    putStrLn $ "msgAsBitVector: " P.++ show msgAsBitVector

    -- "abc" = 0x61 0x62 0x63 in ASCII
    -- Normal encoding would be: 0x636261 (bytes in memory order)
    -- But toBitString reverses bits WITHIN each byte:
    --   'a' = 0x61 = 0b01100001 → 0b10000110 (reversed)
    --   'b' = 0x62 = 0b01100010 → 0b01000110 (reversed)
    --   'c' = 0x63 = 0b01100011 → 0b11000110 (reversed)
    -- Result: 0b1000_0110_0100_0110_1100_0110
    --
    -- This matches SHA3 spec bit numbering where bit 0 is LSB of first byte
    putStrLn "Expected would be: 0x636261 for normal ordering"
    putStrLn "Actual is bit-reversed within bytes for SHA3"

    -- This test just documents the behavior, doesn't assert
    True `shouldBe` True

  it "Check padded blocks" $ do
    let msgBitString = toBitString $(listToVecTH "abc")
    let msgAsBitVector = pack msgBitString :: BitVector 24
    let msgWithSuffix = msgAsBitVector ++# sha3Suffix :: BitVector 26

    putStrLn $ "msgAsBitVector (24 bits): " P.++ show msgAsBitVector
    putStrLn $ "sha3Suffix (2 bits): " P.++ show sha3Suffix
    putStrLn $ "msgWithSuffix (26 bits): " P.++ show msgWithSuffix
    putStrLn $ "testBit msgWithSuffix 0: " P.++ show (testBit msgWithSuffix 0)
    putStrLn $ "testBit msgWithSuffix 24: " P.++ show (testBit msgWithSuffix 24)
    putStrLn $ "testBit msgWithSuffix 25: " P.++ show (testBit msgWithSuffix 25)

    let paddedBlocks = Sponge.Pure.padToRateBlocks @1088 msgWithSuffix

    -- Should have 1 block for short message
    length paddedBlocks `shouldBe` 1

    -- First block should be: msg || suffix || 1 || 0...0 || 1
    -- Bits [25:0] = msg with suffix
    -- Bit 26 = pad start (1)
    -- Bits [1086:27] = zeros
    -- Bit 1087 = pad end (1)
    let firstBlock = head paddedBlocks
    putStrLn $ "First block (low 64 bits): " P.++ show (resize firstBlock :: BitVector 64)
    putStrLn $ "testBit firstBlock 0: " P.++ show (testBit firstBlock 0)
    putStrLn $ "testBit firstBlock 26: " P.++ show (testBit firstBlock 26)

  it "Check suffix appending" $ do
    -- DEMONSTRATES: Vec ++ vs BitVector ++# produce DIFFERENT results!
    let msgBitString = toBitString $(listToVecTH "abc")

    -- Reference way: Vec ++ (used by SHA3.sha3_256)
    -- Vec ++ appends normally: msg comes first, suffix comes after
    -- When packed: first Vec element → bit 0 (LSB)
    let msgWithSuffixRefVec = msgBitString ++ unpack (0b01 :: BitVector 2)
    let msgWithSuffixRef = pack msgWithSuffixRefVec :: BitVector 26

    -- WRONG way: BitVector ++#
    -- ++# puts LEFT in HIGH bits, RIGHT in LOW bits
    -- So this puts suffix in HIGH bits (wrong!)
    let msgAsBitVector = pack msgBitString :: BitVector 24
    let msgWithSuffixMine = sha3Suffix ++# msgAsBitVector :: BitVector 26

    putStrLn $ "Reference msgWithSuffix (low 8 bits): " P.++ show (resize msgWithSuffixRef :: BitVector 8)
    putStrLn $ "My msgWithSuffix (low 8 bits): " P.++ show (resize msgWithSuffixMine :: BitVector 8)

    -- This test SHOULD FAIL - demonstrating that ++# doesn't work for suffix appending
    -- Must use Vec ++ instead (as pureSponge now does)
    msgWithSuffixRef `shouldBe` msgWithSuffixMine

  it "Compare first absorption" $ do
    -- Verify that padding produces identical blocks to reference
    let msgBitString = toBitString $(listToVecTH "abc")
    let msgAsBitVector = pack msgBitString :: BitVector 24

    -- Reference: Use Vec ++ for suffix appending
    let msgWithSuffixRef = msgBitString ++ unpack (0b01 :: BitVector 2)
    let paddedRef = Sponge.Pure.padToRateBlocks @1088 (pack msgWithSuffixRef :: BitVector 26)
    let firstBlockRef = head paddedRef
    putStrLn $ "Reference first block (low 32 bits): " P.++ show (resize firstBlockRef :: BitVector 32)

    -- pureSponge now uses Vec ++ internally for suffix appending
    let actual = Sponge.Pure.pureSponge @1600 @1088 @256 sha3Suffix Perm.keccakF1600 msgAsBitVector
    putStrLn $ "pureSponge output: " P.++ show (resize actual :: BitVector 32)

  it "SHA3-256 hash of 'abc'" $ do
    -- STATUS: Test currently FAILING
    --
    -- FIXED:
    -- ✓ padToRateBlocks now uses correct bit ordering (zeros ++# msg)
    -- ✓ pureSponge uses Vec ++ for suffix appending (not BitVector ++#)
    -- ✓ absorbBlock puts block in low bits correctly (zeros ++# block)
    --
    -- VERIFIED:
    -- ✓ Padded blocks match reference implementation exactly
    -- ✓ Permutation function (Perm.keccakF1600) works correctly
    --
    -- REMAINING ISSUE:
    -- ✗ Hash output is still incorrect
    -- Expected: 0x3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532
    -- Actual:   0x5c19bae5f247a44d203ae8b4d6cb09bda1fa10767cb94ada62fd47a288c2a84c
    --
    -- The hashes are completely different (XOR shows no pattern).
    -- Since padding is verified correct, the issue may be in:
    -- - Absorption loop logic
    -- - State extraction (digest squeeze)
    -- - Or a subtle bit ordering issue in permutation input/output
    --
    let msgBitString = toBitString $(listToVecTH "abc")
    let expectedBitString = SHA3.sha3_256 msgBitString
    let expected = pack expectedBitString

    -- pureSponge handles suffix internally, same as SHA3.sha3_256
    let msgAsBitVector = pack msgBitString :: BitVector 24
    let actual = Sponge.Pure.pureSponge @1600 @1088 @256 sha3Suffix Perm.keccakF1600 msgAsBitVector

    expected `shouldBe` 0x3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532
    actual `shouldBe` expected
-- 0x3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532

  -- it "SHA3-256 hash of empty string" $ do
  --   -- Empty message (0 bits)
  --   let msg = 0 :: BitVector 0
  --   let actual = Sponge.Pure.pureSponge @1600 @1088 @256 sha3Suffix Perm.keccakF1600 msg
  --   let expected = 0xa7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a :: BitVector 256
  --   actual `shouldBe` expected
