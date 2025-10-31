{-# LANGUAGE TemplateHaskell #-}

module SHA3F252
  ( keccakf25OneRound
  , topEntity
  ) where

import Clash.Prelude

-- Keccak-f[25]: 5×5×1 bits, l=0, w=1, b=25
-- Standalone implementation without SHA3internal dependencies

type State25 = Vec 25 Bit

-- Theta transformation for 25-bit state
-- C[x] = A[x,0] xor A[x,1] xor A[x,2] xor A[x,3] xor A[x,4]
-- D[x] = C[x-1] xor C[x+1]
-- A[x,y] = A[x,y] xor D[x]
theta25 :: State25 -> State25
theta25 s = zipWith xor s d_expanded where
  -- Extract columns: C[x] = xor of all bits in column x
  c0 = s!!0 `xor` s!!5  `xor` s!!10 `xor` s!!15 `xor` s!!20
  c1 = s!!1 `xor` s!!6  `xor` s!!11 `xor` s!!16 `xor` s!!21
  c2 = s!!2 `xor` s!!7  `xor` s!!12 `xor` s!!17 `xor` s!!22
  c3 = s!!3 `xor` s!!8  `xor` s!!13 `xor` s!!18 `xor` s!!23
  c4 = s!!4 `xor` s!!9  `xor` s!!14 `xor` s!!19 `xor` s!!24
  -- D[x] = C[x-1] xor C[x+1]
  d0 = c4 `xor` c1  -- C[-1] = C[4], C[+1] = C[1]
  d1 = c0 `xor` c2
  d2 = c1 `xor` c3
  d3 = c2 `xor` c4
  d4 = c3 `xor` c0
  -- Expand D to match state layout
  d_expanded = d0 :> d1 :> d2 :> d3 :> d4 :>
               d0 :> d1 :> d2 :> d3 :> d4 :>
               d0 :> d1 :> d2 :> d3 :> d4 :>
               d0 :> d1 :> d2 :> d3 :> d4 :>
               d0 :> d1 :> d2 :> d3 :> d4 :> Nil

-- Rho transformation: rotation offsets (no rotation for w=1)
-- For w=1, all rotations are 0, so rho is identity
rho25 :: State25 -> State25
rho25 = id

-- Pi transformation: permutation
-- A[y, 2*x + 3*y] = A[x, y]
-- Flattened: index = 5*y + x
pi25 :: State25 -> State25
pi25 s = map (s !!) indices where
  indices = 0 :> 6 :> 12 :> 18 :> 24 :>
            3 :> 9 :> 10 :> 16 :> 22 :>
            1 :> 7 :> 13 :> 19 :> 20 :>
            4 :> 5 :> 11 :> 17 :> 23 :>
            2 :> 8 :> 14 :> 15 :> 21 :> Nil

-- Chi transformation: nonlinear step
-- A[x,y] = A[x,y] xor ((not A[x+1,y]) and A[x+2,y])
chi25 :: State25 -> State25
chi25 s = zipWith3 (\a b c -> a `xor` (complement b .&. c)) s s1 s2 where
  -- s1: x+1 (mod 5) for each position
  s1 = map (s !!) $ 1 :> 2 :> 3 :> 4 :> 0 :>
                    6 :> 7 :> 8 :> 9 :> 5 :>
                    11 :> 12 :> 13 :> 14 :> 10 :>
                    16 :> 17 :> 18 :> 19 :> 15 :>
                    21 :> 22 :> 23 :> 24 :> 20 :> Nil
  -- s2: x+2 (mod 5) for each position
  s2 = map (s !!) $ 2 :> 3 :> 4 :> 0 :> 1 :>
                    7 :> 8 :> 9 :> 5 :> 6 :>
                    12 :> 13 :> 14 :> 10 :> 11 :>
                    17 :> 18 :> 19 :> 15 :> 16 :>
                    22 :> 23 :> 24 :> 20 :> 21 :> Nil

-- Iota transformation: XOR with round constant
-- Only affects A[0,0] (index 0)
-- Round 0 constant: 1
iota25 :: State25 -> State25
iota25 s = replace (0 :: Index 25) (s!!(0 :: Index 25) `xor` 1) s

-- One round of Keccak-f[25]
keccakf25OneRound :: State25 -> State25
keccakf25OneRound = iota25 . chi25 . pi25 . rho25 . theta25

-- Hardware top entity
{-# ANN topEntity
  (Synthesize
    { t_name = "Keccakf25_OneRound2"
    , t_inputs = [ PortName "CLK"
                 , PortName "RST"
                 , PortName "EN"
                 , PortName "DIN"
                 ]
    , t_output = PortName "DOUT"
    }) #-}
{-# OPAQUE topEntity #-}

topEntity :: Clock System
          -> Reset System
          -> Enable System
          -> Signal System State25
          -> Signal System State25
topEntity = exposeClockResetEnable $ fmap keccakf25OneRound
