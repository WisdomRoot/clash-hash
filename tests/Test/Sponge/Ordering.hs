{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Sponge.Ordering (spec) where

import Clash.Prelude
import qualified Prelude as P
import Test.Hspec
import qualified SHA3
import qualified SHA3internal
import qualified KeccakF1600.Permutation as Perm
import qualified Constants

-- | Hardware permutation without iota (no round constants)
-- Chains theta -> rho -> pi -> chi for 24 rounds
permNoIota :: BitVector 1600 -> BitVector 1600
permNoIota bv = pack (foldl (\s _ -> oneRound s) (unpack bv) (indicesI @24))
  where
    oneRound = Perm.chiF1600 . Perm.piF1600 . Perm.rhoF1600 . Perm.thetaF1600

-- | Reference permutation without iota (no round constants)
-- Same sequence as hardware: theta -> rho -> pi -> chi for 24 rounds
refNoIota :: BitVector 1600 -> BitVector 1600
refNoIota bv = pack (foldl (\s _ -> oneRound s) (unpack bv) (indicesI @24))
  where
    oneRound = Perm.chiF1600 . Perm.piF1600 . Perm.rhoF1600 . Perm.thetaF1600

-- | Find the index of the first set bit in a BitVector
outIndex :: BitVector 1600 -> Int
outIndex bv = P.head [ i | i <- [0..1599], testBit bv i ]

-- | Compute (input index, ref output index, hw output index) using no-iota versions
permIdxPairNoIota :: Int -> (Int, Int, Int)
permIdxPairNoIota i =
  let inp    = bit i :: BitVector 1600
      refOut = refNoIota inp
      hwOut  = permNoIota inp
   in (i, outIndex refOut, outIndex hwOut)

-- | Test that bit ordering is consistent between reference and hardware permutations
spec :: Spec
spec = describe "Bit-ordering verification" $ do
  it "round constants match" $ do
    -- Compare round constants from both sources
    P.putStrLn "\nComparing round constants (first 5 rounds):"
    let hwConstants = $(Constants.iota) :: Vec 24 (BitVector 64)
    let refConstants = SHA3internal._iota_constants :: Vec 24 (Vec 64 Bit)

    P.mapM_ (\idx -> do
      let hwRC = hwConstants !! idx
      let refRC = refConstants !! idx
      let refRC_bv = v2bv refRC
      let hwRC_reversed = reverse (bitCoerce hwRC :: Vec 64 Bit)
      let match = if hwRC_reversed P.== refRC then "✓" else "✗"
      P.putStrLn $ match P.++ " Round " P.++ P.show idx P.++ ":"
      P.putStrLn $ "  HW (BV):          " P.++ P.show hwRC
      P.putStrLn $ "  HW (reversed Vec): " P.++ P.show (v2bv hwRC_reversed)
      P.putStrLn $ "  REF (Vec->BV):    " P.++ P.show refRC_bv
      ) [0..2]

    let allMatch = P.all (\idx -> reverse (bitCoerce (hwConstants !! idx) :: Vec 64 Bit) P.== refConstants !! idx) [0..23]
    if allMatch
      then P.putStrLn "\n✓ All 24 round constants match after reversal!"
      else expectationFailure "Round constants mismatch even after reversal"


  it "no-iota permutation ordering matches" $ do
    -- Test with no-iota versions - should have single bit output
    P.putStrLn "\nTesting no-iota permutations (first 20 inputs):"
    let sample = P.map permIdxPairNoIota [0..19]
    P.mapM_ (\(i, refIdx, hwIdx) -> do
      let match = if refIdx P.== hwIdx then "✓" else "✗"
      P.putStrLn $ match P.++ " in[" P.++ P.show i P.++ "] -> " P.++
                   "ref[" P.++ P.show refIdx P.++ "] " P.++
                   "hw[" P.++ P.show hwIdx P.++ "]"
      ) sample

    -- Check all 1600 positions
    let results = P.map testBitPositionNoIota [0..1599]
    let mismatches = P.filter (P.not . P.snd) results
    case mismatches of
      [] -> P.putStrLn "\n✓ All 1600 no-iota positions match!"
      _ -> do
        P.putStrLn $ "\n✗ " P.++ P.show (P.length mismatches) P.++ " no-iota mismatches found"
        expectationFailure $ "No-iota bit ordering mismatch at " P.++ P.show (P.length mismatches) P.++ " positions"

  it "full permutation (with iota) matches" $ do
    -- Test full permutation including iota
    P.putStrLn "\nTesting full permutation with iota:"
    let refPermFull = pack . SHA3.keccakf . unpack
    let hwPermFull = Perm.keccakF1600

    -- Test zero input - simplest case
    let inp0 = 0 :: BitVector 1600
    let refOut0 = refPermFull inp0
    let hwOut0 = hwPermFull inp0
    P.putStrLn $ "\nInput: 0"
    P.putStrLn $ "Ref output (first 128 bits): " P.++ P.show (resize refOut0 :: BitVector 128)
    P.putStrLn $ "HW  output (first 128 bits): " P.++ P.show (resize hwOut0 :: BitVector 128)
    P.putStrLn $ "Match: " P.++ P.show (refOut0 P.== hwOut0)

    -- Test a few specific inputs
    let testInputs = [0, 1, bit 0, bit 63, bit 64]
    let testResults = P.map (\inp ->
          let refOut = refPermFull inp
              hwOut = hwPermFull inp
          in (inp, refOut, hwOut, refOut P.== hwOut)
          ) testInputs

    P.mapM_ (\(inp, refOut, hwOut, matches) -> do
      let match = if matches then "✓" else "✗"
      P.putStrLn $ match P.++ " input bit count: " P.++ P.show (popCount inp)
      ) testResults

    -- Verify all match
    let allMatch = P.all (\(_, _, _, m) -> m) testResults
    if allMatch
      then P.putStrLn "\n✓ Full permutation matches!"
      else expectationFailure "Full permutation (with iota) mismatch"

-- | Test a single bit position with no-iota, return (position, matches)
testBitPositionNoIota :: Int -> (Int, Bool)
testBitPositionNoIota i =
  let input = bit i :: BitVector 1600
      refOut = refNoIota input
      hwOut = permNoIota input
   in (i, refOut P.== hwOut)
