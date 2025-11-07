{-# LANGUAGE BinaryLiterals #-}

module Constants.TH
  ( iota
  , chi
  , pi
  , rho
  , theta
  ) where

import Clash.Prelude hiding (Exp, Type, pi)
import qualified Prelude as P
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
  let w = 8 :: Int   -- lane width for Keccak-f[200]
      b = 200 :: Int -- total state size

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
        in (i0, i1, i2) :: (Int, Int, Int)

      -- Generate all 200 triples as Int tuples
      triples :: [(Int, Int, Int)]
      triples = P.map chiTriple [0..b P.- 1]

      -- Convert Int tuples to Index 200 tuples
      mkTriple :: (Int, Int, Int) -> (Index 200, Index 200, Index 200)
      mkTriple (i0, i1, i2) = (P.fromIntegral i0, P.fromIntegral i1, P.fromIntegral i2)

      indexTriples :: [(Index 200, Index 200, Index 200)]
      indexTriples = P.map mkTriple triples

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
        let i = idx `P.div` (5 P.* w)
            j = (idx `P.mod` (5 P.* w)) `P.div` w
            k = idx `P.mod` w
        in (i, j, k)

      -- Convert (i,j,k) back to flat index
      flatten (i, j, k) = i P.* (5 P.* w) P.+ j P.* w P.+ k

      -- Generate pi permutation for position idx
      -- Pi transformation: (i, j, k) -> (j, 3*i + j, k)
      piPermute idx =
        let (i, j, k) = erect idx
            i' = j
            j' = (3 P.* i P.+ j) `P.mod` 5
            k' = k
        in flatten (i', j', k')

      -- Generate all 200 source indices as Int
      srcIndices :: [Int]
      srcIndices = P.map piPermute [0..b P.- 1]

      -- Convert Int to Index 200
      indexList :: [Index 200]
      indexList = P.map P.fromIntegral srcIndices

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
        let i = idx `P.div` (5 P.* w)
            j = (idx `P.mod` (5 P.* w)) `P.div` w
            k = idx `P.mod` w
        in (i, j, k)

      -- Convert (i,j,k) back to flat index
      flatten (i, j, k) = i P.* (5 P.* w) P.+ j P.* w P.+ k

      -- Generate the 5×5 rotation offsets matrix per Keccak specification
      -- r[0][0] = 0, and for (x,y) starting at (1,0), traversing via (x,y) ← (y, 2x+3y):
      -- r[x][y] = ((t+1)(t+2)/2) mod w for t = 0..23
      rotationOffsets :: [[Int]]
      rotationOffsets =
        let -- Start with r[0][0] = 0
            initial = P.replicate 5 (P.replicate 5 0)

            -- Generate states (t, x, y) for t = 0..23, starting with (x, y) = (1, 0)
            -- Coordinate update: (x, y) ← (y, (2x + 3y) mod 5)
            states = P.take 24 $ P.iterate step (0, 1, 0)
              where
                step (t, x, y) =
                  let t' = t P.+ 1
                      x' = y
                      y' = (2 P.* x P.+ 3 P.* y) `P.mod` 5
                  in (t', x', y')

            -- Compute rotation amount r = ((t+1)(t+2)/2) mod w for each state
            statesWithRotation = P.map (\(t, x, y) ->
              (x, y, ((t P.+ 1) P.* (t P.+ 2) `P.div` 2) `P.mod` w)) states

            -- Fill in the rotation amounts
            fillOffset matrix (x, y, r) =
              let row = matrix P.!! x
                  row' = P.take y row P.++ [r] P.++ P.drop (y P.+ 1) row
              in P.take x matrix P.++ [row'] P.++ P.drop (x P.+ 1) matrix

        in P.foldl fillOffset initial statesWithRotation

      -- Get rotation offset for position (i, j)
      getOffset i j = (rotationOffsets P.!! i) P.!! j

      -- Generate rho permutation for position idx
      -- Rho transformation: (i, j, k) -> (i, j, k - offset[i][j])
      rhoPermute idx =
        let (i, j, k) = erect idx
            offset = getOffset i j
            k' = (k P.- offset) `P.mod` w
        in flatten (i, j, k')

      -- Generate all 200 source indices as Int
      srcIndices :: [Int]
      srcIndices = P.map rhoPermute [0..b P.- 1]

      -- Convert Int to Index 200
      indexList :: [Index 200]
      indexList = P.map P.fromIntegral srcIndices

  listToVecTH indexList

-- | Template Haskell generator for Theta transformation index lookup.
-- Takes Keccak parameter @l@ (lane width w = 2^l) and returns
-- @Vec (25*w) (Vec 11 (Index (25*w)))@.
theta :: Int -> Q Exp
theta l = do
  let w = 2 P.^ l
      b = 25 * w

      erect idx =
        let i = idx `P.div` (5 P.* w)
            j = (idx `P.mod` (5 P.* w)) `P.div` w
            k = idx `P.mod` w
        in (i, j, k)

      flatten (i, j, k) = i P.* (5 P.* w) P.+ j P.* w P.+ k

      thetaIndices idx =
        let (i, j, k) = erect idx
            self = flatten (i, j, k)
            colMinus = [flatten (row, (j P.- 1) `P.mod` 5, k) | row <- [0..4]]
            colPlus  = [flatten (row, (j P.+ 1) `P.mod` 5, (k P.- 1) `P.mod` w) | row <- [0..4]]
        in self : colMinus P.++ colPlus

      allIndices :: [[Int]]
      allIndices = P.map thetaIndices [0 .. b P.- 1]

      idxType = AppT (ConT ''Index) (LitT (NumTyLit (fromIntegral b)))
      vec11Type = AppT (AppT (ConT ''Vec) (LitT (NumTyLit 11))) idxType

      mkIndex n =
        SigE (LitE (IntegerL (fromIntegral n))) idxType

      mkVec :: Int -> Type -> [Exp] -> Exp
      mkVec len elemType elems =
        let cons x xs = InfixE (Just x) (ConE '(:>)) (Just xs)
            body = P.foldr cons (ConE 'Nil) elems
            vecType = AppT (AppT (ConT ''Vec) (LitT (NumTyLit (fromIntegral len)))) elemType
        in SigE body vecType

      mkInner row = mkVec 11 idxType (P.map mkIndex row)
      mkOuter rows = mkVec b vec11Type (P.map mkInner rows)

  pure (mkOuter allIndices)
