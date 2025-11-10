module Constants.Indices
  ( theta,
  )
where

import Prelude

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
