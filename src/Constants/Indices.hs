module Constants.Indices
  ( theta,
    rho,
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

-- | Pure computation of rho transformation indices.
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
            statesWithRotation =
              map
                ( \(t, x, y) ->
                    (x, y, ((t + 1) * (t + 2) `div` 2) `mod` w)
                )
                states

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
   in map rhoPermute [0 .. b - 1]
