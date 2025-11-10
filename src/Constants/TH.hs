{-# LANGUAGE BinaryLiterals #-}

module Constants.TH
  ( iota,
    chi,
    pi,
  )
where

import Clash.Prelude (BitVector, Index, Vec, listToVecTH, rotateR, setBit, testBit, toList, unfoldrI, xor)
import Language.Haskell.TH
import Prelude hiding (pi)

-- | Template Haskell generator that produces the Vec of 24 Keccak round constants.
iota :: Q Exp
iota = do
  let seed :: BitVector 8
      seed = 0b10000000

      bitPositions :: [Int]
      bitPositions = [0, 1, 3, 7, 15, 31, 63]

      expandLFSR :: BitVector 8 -> BitVector 64
      expandLFSR s =
        foldl setBitAt 0 (zip [0 .. 6] bitPositions)
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
  let w = 8 :: Int -- lane width for Keccak-f[200]
      b = 200 :: Int -- total state size

      -- Convert flat index to (i,j,k) coordinates
      -- i = row (0-4), j = column (0-4), k = bit in lane (0-7)
      erect idx =
        let i = idx `div` (5 * w)
            j = (idx `mod` (5 * w)) `div` w
            k = idx `mod` w
         in (i, j, k)

      -- Convert (i,j,k) back to flat index
      flatten (i, j, k) = i * (5 * w) + j * w + k

      -- Generate chi triple for position idx
      -- Returns (i0, i1, i2) where:
      --   i0 = A[i,j,k]
      --   i1 = A[i,j+1,k]
      --   i2 = A[i,j+2,k]
      chiTriple idx =
        let (i, j, k) = erect idx
            i0 = flatten (i, j, k)
            i1 = flatten (i, (j + 1) `mod` 5, k)
            i2 = flatten (i, (j + 2) `mod` 5, k)
         in (i0, i1, i2) :: (Int, Int, Int)

      -- Generate all 200 triples as Int tuples
      triples :: [(Int, Int, Int)]
      triples = map chiTriple [0 .. b - 1]

      -- Convert Int tuples to Index 200 tuples
      mkTriple :: (Int, Int, Int) -> (Index 200, Index 200, Index 200)
      mkTriple (i0, i1, i2) = (fromIntegral i0, fromIntegral i1, fromIntegral i2)

      indexTriples :: [(Index 200, Index 200, Index 200)]
      indexTriples = map mkTriple triples

  listToVecTH indexTriples

-- | Template Haskell generator for Pi transformation permutation.
-- For Keccak-f[200]: generates Vec 200 (Index 200)
-- Pi formula: (i, j, k) -> (j, 3*i + j, k)
pi :: Q Exp
pi = do
  let w = 8 :: Int -- lane width for Keccak-f[200]
      b = 200 :: Int -- total state size

      -- Convert flat index to (i,j,k) coordinates
      -- i = row (0-4), j = column (0-4), k = bit in lane (0-7)
      erect idx =
        let i = idx `div` (5 * w)
            j = (idx `mod` (5 * w)) `div` w
            k = idx `mod` w
         in (i, j, k)

      -- Convert (i,j,k) back to flat index
      flatten (i, j, k) = i * (5 * w) + j * w + k

      -- Generate pi permutation for position idx
      -- Pi transformation: (i, j, k) -> (j, 3*i + j, k)
      piPermute idx =
        let (i, j, k) = erect idx
            i' = j
            j' = (3 * i + j) `mod` 5
            k' = k
         in flatten (i', j', k')

      -- Generate all 200 source indices as Int
      srcIndices :: [Int]
      srcIndices = map piPermute [0 .. b - 1]

      -- Convert Int to Index 200
      indexList :: [Index 200]
      indexList = map fromIntegral srcIndices

  listToVecTH indexList

-- -- | Template Haskell generator for Theta transformation index lookup.
-- -- Takes Keccak parameter @l@ (lane width w = 2^l) and returns
-- -- @Vec (25*w) (Vec 11 (Index (25*w)))@.
-- theta :: Int -> Q Exp
-- theta l = do
--   let allIndices = thetaIndices l
--       b = 25 * (2 ^ l)

--       idxType = AppT (ConT ''Index) (LitT (NumTyLit (fromIntegral b)))
--       vec11Type = AppT (AppT (ConT ''Vec) (LitT (NumTyLit 11))) idxType

--       mkIndex n =
--         SigE (LitE (IntegerL (fromIntegral n))) idxType

--       mkVec :: Int -> Type -> [Exp] -> Exp
--       mkVec len elemType elems =
--         let cons x xs = InfixE (Just x) (ConE '(:>)) (Just xs)
--             body = foldr cons (ConE 'Nil) elems
--             vecType = AppT (AppT (ConT ''Vec) (LitT (NumTyLit (fromIntegral len)))) elemType
--         in SigE body vecType

--       mkInner row = mkVec 11 idxType (map mkIndex row)
--       mkOuter rows = mkVec b vec11Type (map mkInner rows)

--   pure (mkOuter allIndices)
