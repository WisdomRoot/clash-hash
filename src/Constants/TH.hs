{-# LANGUAGE BinaryLiterals #-}

module Constants.TH
  ( iota
  , chi
  ) where

import Clash.Prelude hiding (Exp)
import qualified Prelude as P
import Language.Haskell.TH (Exp, Q)

-- | Template Haskell generator that produces the Vec of 24 Keccak round constants.
iota :: Q Exp
iota = do
  let seed :: BitVector 8
      seed = 0b10000000

      bitPositions :: [Int]
      bitPositions = [0, 1, 3, 7, 15, 31, 63]

      expandLFSR :: BitVector 8 -> BitVector 64
      expandLFSR s =
        P.foldl setBitAt 0 (P.zip [0 .. 6] bitPositions)
        where
          setBitAt acc (j, pos) =
            let tap = 7 - j
            in if testBit s tap
                 then setBit acc pos
                 else acc

      lfsrStep :: BitVector 8 -> BitVector 8
      lfsrStep s =
        let fb = testBit s 0
            s' = s `rotateR` 1
            mask = 0b10001110 :: BitVector 8
        in if fb then s' `xor` mask else s'

      generateRoundConstant :: BitVector 8 -> (BitVector 64, BitVector 8)
      generateRoundConstant lfsr = (expandLFSR lfsr, lfsrStep lfsr)

      constants :: Vec 24 (BitVector 64)
      constants = unfoldrI generateRoundConstant seed

  listToVecTH (toList constants)

-- | Template Haskell generator for Chi transformation index triples.
-- For Keccak-f[200]: generates Vec 200 (Index 200, Index 200, Index 200)
-- Chi formula: A'[x,y] = A[x,y] ⊕ (¬A[x+1,y] ∧ A[x+2,y])
chi :: Q Exp
chi = do
  let w = 8   -- lane width for Keccak-f[200]
      b = 200 -- total state size

      -- Convert flat index to (i,j,k) coordinates
      -- i = row (0-4), j = column (0-4), k = bit in lane (0-7)
      erect idx =
        let i = idx `P.div` (5 P.* w)
            j = (idx `P.mod` (5 P.* w)) `P.div` w
            k = idx `P.mod` w
        in (i, j, k)

      -- Convert (i,j,k) back to flat index
      flatten (i, j, k) = i P.* (5 P.* w) P.+ j P.* w P.+ k

      -- Generate chi triple for position idx
      -- Returns (i0, i1, i2) where:
      --   i0 = A[i,j,k]
      --   i1 = A[i,j+1,k]
      --   i2 = A[i,j+2,k]
      chiTriple idx =
        let (i, j, k) = erect idx
            i0 = flatten (i, j, k)
            i1 = flatten (i, (j P.+ 1) `P.mod` 5, k)
            i2 = flatten (i, (j P.+ 2) `P.mod` 5, k)
        in (i0, i1, i2)

      -- Generate all 200 triples
      triples :: [(Int, Int, Int)]
      triples = P.map chiTriple [0..b P.- 1]

  listToVecTH triples
