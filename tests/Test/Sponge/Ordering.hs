{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Sponge.Ordering (spec) where

import Clash.Prelude
import qualified KeccakF1600.Permutation as Perm
import qualified SHA3internal
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
