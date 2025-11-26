{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Sponge.Ordering (spec) where

import Clash.Prelude
import qualified Prelude as P
import qualified KeccakF1600.Permutation as Perm
import qualified SHA3internal
import qualified Constants
import Test.Hspec
import GHC.Base (VecCount(Vec16))
import Clash.Sized.Internal.BitVector (Bit(Bit))

-- | Test that bit ordering is consistent between reference and hardware permutations
spec :: Spec
spec = describe "Permutation bit-ordering verification" $ do
  it "1 round permutation matches" $ do
    -- Test single round permutation
    putStrLn "\nTesting 1 round permutation:"
    let inp0 = 123 :: BitVector 1600
    let refOut = pack (SHA3internal.keccakf1Round @6 @64 @1600 0 (unpack inp0))
    let hwOut = Perm.keccakF1600Round 0 inp0

    -- let tailRef = drop d64 (unpack refOut :: Vec 1600 Bit)
    -- let tailHw  = drop d64 (unpack hwOut :: Vec 1600 Bit)

    print refOut
    print hwOut


    -- putStrLn $ "Ref output (first 128 bits): " <> show (resize refOut :: BitVector 128)
    -- putStrLn $ "HW  output (first 128 bits): " <> show (resize hwOut :: BitVector 128)
    -- putStrLn $ "Match: " <> show (refOut == hwOut)

    if refOut == hwOut
      then putStrLn "\n✓ 1 round permutation matches!"
      else expectationFailure "1 round permutation mismatch"

  -- it "debug theta index transformation" $ do
  --   putStrLn "\n=== Debugging Theta Index Transformation ===\n"

  --   -- Show what indices theta XORs for a few sample positions
  --   let sampleIndices = [0, 1, 63, 64, 65] :: [Int]
  --   let sha3Consts = SHA3internal.sha3_constants @6 @64 @1600
  --   let constTheta = $(Constants.theta 6)

  --   putStrLn "SHA3internal theta constants (first few positions):"
  --   P.mapM_ (\idx -> do
  --     let indices = SHA3internal.theta_constants sha3Consts !! idx
  --     putStrLn $ "  Position " P.<> P.show idx P.<> " XORs indices: " P.<> P.show (toList indices)
  --     ) sampleIndices

  --   putStrLn "\nConstants.theta indices (first few positions):"
  --   P.mapM_ (\idx -> do
  --     let indices = constTheta !! idx
  --     putStrLn $ "  Position " P.<> P.show idx P.<> " XORs indices: " P.<> P.show (P.map fromIntegral (toList indices) :: [Int])
  --     ) sampleIndices

  --   putStrLn "\nComparing index lists for position 0:"
  --   let sha3Idx0 = SHA3internal.theta_constants sha3Consts !! 0
  --   let constIdx0 = constTheta !! 0
  --   putStrLn $ "  SHA3internal: " P.<> P.show (toList sha3Idx0)
  --   putStrLn $ "  Constants:    " P.<> P.show (P.map fromIntegral (toList constIdx0) :: [Int])
  --   putStrLn $ "  Match (same values): " P.<> P.show (toList sha3Idx0 == P.map fromIntegral (toList constIdx0))

  -- it "debug theta with .rev applied" $ do
  --   putStrLn "\n=== Theta Index Transformation with .rev ===\n"

  --   let sampleIndices = [0, 1, 63, 64, 65] :: [Int]
  --   let sha3Consts = SHA3internal.sha3_constants @6 @64 @1600
  --   let constTheta = $(Constants.theta 6)

  --   putStrLn "SHA3internal theta (no .rev):"
  --   P.mapM_ (\idx -> do
  --     let indices = SHA3internal.theta_constants sha3Consts !! idx
  --     putStrLn $ "  Output position " P.<> P.show idx P.<> " reads from: " P.<> P.show (toList indices)
  --     ) sampleIndices

  --   putStrLn "\nConstants.theta with .rev applied:"
  --   P.mapM_ (\idx -> do
  --     let rawIndices = constTheta !! idx
  --     let revIndices = fmap (\i -> 1599 - fromIntegral i) rawIndices :: Vec 11 (Index 1600)
  --     putStrLn $ "  Output position " P.<> P.show idx P.<> " reads from: " P.<> P.show (toList revIndices)
  --     ) sampleIndices

  --   putStrLn "\nComparison for output position 0:"
  --   let sha3Idx0 = SHA3internal.theta_constants sha3Consts !! 0
  --   let constIdx0Raw = constTheta !! 0
  --   let constIdx0Rev = fmap (\i -> 1599 - fromIntegral i) constIdx0Raw :: Vec 11 (Index 1600)
  --   putStrLn $ "  SHA3internal (no .rev):     " P.<> P.show (toList sha3Idx0)
  --   putStrLn $ "  Constants.theta (with .rev): " P.<> P.show (toList constIdx0Rev)
  --   putStrLn $ "  Match: " P.<> P.show (sha3Idx0 == constIdx0Rev)
