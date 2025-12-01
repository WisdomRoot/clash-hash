module Permutation.Indices
  ( theta,
    rho,
    pi,
    chi,
  )
where

import Prelude hiding (pi)

-- | Pure computation of theta transformation indices.
-- Takes Keccak parameter @l@ (lane width w = 2^l) and returns
-- a list of index lists for each position in the state.
theta :: Int -> [[Int]]
theta l =
  let w = 2 ^ l
      b = 25 * w

      erect idx =
        let i = idx `div` (5 * w)
            j = (idx `mod` (5 * w)) `div` w
            k = idx `mod` w
         in (i, j, k)

      flatten (i, j, k) = i * (5 * w) + j * w + k

      indicesFor idx =
        let (i, j, k) = erect idx
            self = flatten (i, j, k)
            -- Theta computes column parities C[x] = ⊕_y A[x,y] where x corresponds to j (column)
            -- D[x,z] = C[x-1,z] ⊕ rot(C[x+1,z],1)
            -- For position (i,j,k), we need column j-1 and column j+1 parities
            colPrev = [flatten (row, (j + 4) `mod` 5, k) | row <- [0 .. 4]]
            colNext = [flatten (row, (j + 1) `mod` 5, (k + w - 1) `mod` w) | row <- [0 .. 4]]
         in self : colPrev ++ colNext
   in map indicesFor [0 .. b - 1]

-- | Pure computation of rho transformation indices (version 2).
-- Mirrors the exact logic of _rho_constants from SHA3internal.hs
-- Takes Keccak parameter @l@ (lane width w = 2^l) and returns
-- a list of source indices for the rho permutation.
rho :: Int -> [Int]
rho l =
  let w = 2 ^ l
      b = 25 * w

      erect idx =
        let i = idx `div` (5 * w)
            j = (idx `mod` (5 * w)) `div` w
            k = idx `mod` w
         in (i, j, k)

      flatten (i, j, k) = i * (5 * w) + j * w + k

      -- Generate 24 traversal states using the _rho_constants formula
      -- Starting state: (t=0, i=0, j=1, k=1)
      -- Next state: (t', i', j', k') = (t+1, 3*i + 2*j, i, k * (t+3) div (t+1))
      generateStates :: [(Integer, Integer, Integer, Integer)]
      generateStates =
        take 24 $ iterate step (0, 0, 1, 1)
        where
          step (t, i, j, k) =
            let t' = t + 1
                i' = (3 * i + 2 * j) `mod` 5
                j' = i
                k' = k * (t + 3) `div` (t + 1)
             in (t', i', j', k')

      -- Create (position_index, rotation_offset) pairs
      -- position_index = 5*i + j
      positionRotationPairs =
        map (\(_, i, j, k) -> (5 * i + j, k)) generateStates

      -- Sort by position index
      sortedPairs = sortBy (\(p1, _) (p2, _) -> compare p1 p2) positionRotationPairs
        where
          sortBy cmp = foldr insert []
            where
              insert x [] = [x]
              insert x (y:ys)
                | cmp x y == GT = y : insert x ys
                | otherwise = x : y : ys

      -- Extract rotation offsets
      rotationOffsets = map snd sortedPairs

      -- Build 5×5 rotation matrix: r[0][0] = 0, rest from rotationOffsets
      -- Total 25 positions, position (0,0) = index 0 gets 0, rest get values from rotationOffsets
      rotationMatrix =
        let allOffsets = 0 : rotationOffsets  -- Prepend 0 for position (0,0)
            rows = chunkBy 5 allOffsets
         in rows
        where
          chunkBy _ [] = []
          chunkBy n xs = take n xs : chunkBy n (drop n xs)

      -- Get rotation offset for position (i, j)
      getOffset i j = (rotationMatrix !! i) !! j

      -- Apply rho transformation: (i, j, k) -> source index at (i, j, k - offset[i][j])
      rhoPermute idx =
        let (i, j, k) = erect idx
            offset = fromInteger (getOffset i j) :: Int
            k' = (k - offset) `mod` w
         in flatten (i, j, k')

   in map rhoPermute [0 .. b - 1]

-- | Pure computation of pi transformation indices.
-- Takes Keccak parameter @l@ (lane width w = 2^l) and returns
-- a list of source indices for the pi permutation.
pi :: Int -> [Int]
pi l =
  let w = 2 ^ l
      b = 25 * w

      erect idx =
        let i = idx `div` (5 * w)
            j = (idx `mod` (5 * w)) `div` w
            k = idx `mod` w
         in (i, j, k)

      flatten (i, j, k) = i * (5 * w) + j * w + k

      -- Generate pi permutation for position idx
      -- Pi transformation: (i, j, k) -> (j, 3*i + j, k)
      piPermute idx =
        let (i, j, k) = erect idx
            i' = j
            j' = (3 * i + j) `mod` 5
            k' = k
         in flatten (i', j', k')

   in map piPermute [0 .. b - 1]

-- | Pure computation of chi transformation index triples.
-- Takes Keccak parameter @l@ (lane width w = 2^l) and returns
-- a list of index triples for the chi transformation.
chi :: Int -> [(Int, Int, Int)]
chi l =
  let w = 2 ^ l
      b = 25 * w

      erect idx =
        let i = idx `div` (5 * w)
            j = (idx `mod` (5 * w)) `div` w
            k = idx `mod` w
         in (i, j, k)

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
         in (i0, i1, i2)

   in map chiTriple [0 .. b - 1]
