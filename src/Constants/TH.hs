{-# LANGUAGE BinaryLiterals #-}

module Constants.TH
  ( iota
  , chi
  , pi
  , rho
  -- , theta
  , theta200
  ) where

import Clash.Prelude (BitVector, Vec((:>), Nil), Index, unfoldrI, listToVecTH, toList, testBit, setBit, rotateR, xor)
import Prelude hiding (pi)
import Language.Haskell.TH

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
  let w = 8 :: Int   -- lane width for Keccak-f[200]
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
      triples = map chiTriple [0..b - 1]

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
  let w = 8 :: Int   -- lane width for Keccak-f[200]
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
      srcIndices = map piPermute [0..b - 1]

      -- Convert Int to Index 200
      indexList :: [Index 200]
      indexList = map fromIntegral srcIndices

  listToVecTH indexList

-- | Template Haskell generator for Rho transformation permutation.
-- For Keccak-f[200]: generates Vec 200 (Index 200)
-- Rho rotates each lane by a different amount specified by the rotation offsets matrix.
rho :: Q Exp
rho = do
  let w = 8 :: Int   -- lane width for Keccak-f[200]
      b = 200 :: Int -- total state size

      -- Convert flat index to (i,j,k) coordinates
      erect idx =
        let i = idx `div` (5 * w)
            j = (idx `mod` (5 * w)) `div` w
            k = idx `mod` w
        in (i, j, k)

      -- Convert (i,j,k) back to flat index
      flatten (i, j, k) = i * (5 * w) + j * w + k

      -- Generate the 5×5 rotation offsets matrix per Keccak specification
      -- r[0][0] = 0, and for (x,y) starting at (1,0), traversing via (x,y) ← (y, 2x+3y):
      -- r[x][y] = ((t+1)(t+2)/2) mod w for t = 0..23
      rotationOffsets :: [[Int]]
      rotationOffsets =
        let -- Start with r[0][0] = 0
            initial = replicate 5 (replicate 5 0)

            -- Generate states (t, x, y) for t = 0..23, starting with (x, y) = (1, 0)
            -- Coordinate update: (x, y) ← (y, (2x + 3y) mod 5)
            states = take 24 $ iterate step (0, 1, 0)
              where
                step (t, x, y) =
                  let t' = t + 1
                      x' = y
                      y' = (2 * x + 3 * y) `mod` 5
                  in (t', x', y')

            -- Compute rotation amount r = ((t+1)(t+2)/2) mod w for each state
            statesWithRotation = map (\(t, x, y) ->
              (x, y, ((t + 1) * (t + 2) `div` 2) `mod` w)) states

            -- Fill in the rotation amounts
            fillOffset matrix (x, y, r) =
              let row = matrix !! x
                  row' = take y row ++ [r] ++ drop (y + 1) row
              in take x matrix ++ [row'] ++ drop (x + 1) matrix

        in foldl fillOffset initial statesWithRotation

      -- Get rotation offset for position (i, j)
      getOffset i j = (rotationOffsets !! i) !! j

      -- Generate rho permutation for position idx
      -- Rho transformation: (i, j, k) -> (i, j, k - offset[i][j])
      rhoPermute idx =
        let (i, j, k) = erect idx
            offset = getOffset i j
            k' = (k - offset) `mod` w
        in flatten (i, j, k')

      -- Generate all 200 source indices as Int
      srcIndices :: [Int]
      srcIndices = map rhoPermute [0..b - 1]

      -- Convert Int to Index 200
      indexList :: [Index 200]
      indexList = map fromIntegral srcIndices

  listToVecTH indexList

-- | Pure computation of theta transformation indices.
-- Takes Keccak parameter @l@ (lane width w = 2^l) and returns
-- a list of index lists for each position in the state.
thetaIndices :: Int -> [[Int]]
thetaIndices l =
  let w = 2 ^ l
      b = 25 * w

      erect idx =
        let i = idx `div` (5 * w)
            j = (idx `mod` (5 * w)) `div` w
            k = idx `mod` w
        in (i, j, k)

      flatten (i, j, k) = i * (5 * w) + j * w + k

      thetaIndicesFor idx =
        let (i, j, k) = erect idx
            self = flatten (i, j, k)
            -- Theta computes column parities C[x] = ⊕_y A[x,y] where x corresponds to j (column)
            -- D[x,z] = C[x-1,z] ⊕ rot(C[x+1,z],1)
            -- For position (i,j,k), we need column j-1 and column j+1 parities
            colPrev = [flatten (row, (j + 4) `mod` 5, k) | row <- [0..4]]
            colNext = [flatten (row, (j + 1) `mod` 5, (k + w - 1) `mod` w) | row <- [0..4]]
        in self : colPrev ++ colNext

  in map thetaIndicesFor [0 .. b - 1]

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


-- | Template Haskell generator for Theta transformation index lookup.
-- For Keccak-f[200]: generates Vec 200 (Vec 11 (Index 200))
-- Theta formula: A'[i,j,k] = A[i,j,k] ⊕ parity(column j-1, bit k) ⊕ parity(column j+1, bit k-1)
-- where parity is XOR of all 5 rows in that column.
theta200 :: Q Exp
theta200 = do
  let w = 8 :: Int   -- lane width for Keccak-f[200]
      b = 200 :: Int -- total state size

      -- Convert flat index to (i,j,k) coordinates
      erect idx =
        let i = idx `div` (5 * w)
            j = (idx `mod` (5 * w)) `div` w
            k = idx `mod` w
        in (i, j, k)

      -- Convert (i,j,k) back to flat index
      flatten (i, j, k) = i * (5 * w) + j * w + k

      -- Generate theta index list for position idx
      -- Returns 11 indices:
      --   1. The bit itself: (i, j, k)
      --   2-6. All 5 rows in column j-1 at bit k: (0..4, j-1, k)
      --   7-11. All 5 rows in column j+1 at bit k-1: (0..4, j+1, k-1)
      indices idx =
        let (i, j, k) = erect idx

            -- The bit itself
            self = flatten (i, j, k)

            -- All rows in column j-1 at bit position k
            col_jMinus1 = [flatten (row, (j - 1) `mod` 5, k) | row <- [0..4]]

            -- All rows in column j+1 at bit position k-1
            col_jPlus1 = [flatten (row, (j + 1) `mod` 5, (k - 1) `mod` w) | row <- [0..4]]

        in self : col_jMinus1 ++ col_jPlus1

      -- Generate all 200 index lists
      allIndices :: [[Int]]
      allIndices = map indices [0..b - 1]

      -- Convert [[Int]] to [[Index 200]] and then to [Vec 11 (Index 200)]
      convertInner :: [Int] -> Vec 11 (Index 200)
      convertInner xs =
        let idxs = map fromIntegral xs :: [Index 200]
        in case idxs of
             [i0,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10] ->
               i0 :> i1 :> i2 :> i3 :> i4 :> i5 :> i6 :> i7 :> i8 :> i9 :> i10 :> Nil
             _ -> error "thetaIndices must produce exactly 11 elements"

      indexVecs :: [Vec 11 (Index 200)]
      indexVecs = map convertInner allIndices

  -- Now convert [Vec 11 (Index 200)] to Vec 200 (Vec 11 (Index 200))
  listToVecTH indexVecs
