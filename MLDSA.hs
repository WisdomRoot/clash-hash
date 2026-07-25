module MLDSA
  ( module MLDSA.Auxiliary,
    module MLDSA.KeyGen,
    module MLDSA.NTT,
    p,
    cT0,
    cT1,
    cT2,
    modExp,
    extGCD,
  )
where

import MLDSA.Auxiliary
import MLDSA.KeyGen
import MLDSA.NTT
import MLDSA.Polynomial

p :: Integer
p = 8380417

cT0 :: String
cT0 = "ML-DSA-PKE"

cT1 :: String
cT1 = "ML-DSA-KEM"

cT2 :: String
cT2 = "ML-DSA-SIG"

modExp :: Integer -> Integer -> Integer -> Integer
modExp _ 0 _ = 1
modExp b e m
  | even e =
      let r = modExp b (e `div` 2) m
       in (r * r) `mod` m
  | otherwise =
      (b * modExp b (e - 1) m) `mod` m

extGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extGCD a 0 = (1, 0, a)
extGCD a b = (t, s - q * t, g)
  where
    (q, r) = a `quotRem` b
    (s, t, g) = extGCD b r

