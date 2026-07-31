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

import Data.ByteString (ByteString)
import qualified Data.Vector as BoxedV
import MLDSA.Auxiliary
import MLDSA.KeyGen
import MLDSA.NTT
import MLDSA.Polynomial

data KeyGenResult = KeyGenResult
  { keyGenRho :: ByteString,
    keyGenRhoPrime :: ByteString,
    keyGenK :: ByteString,
    keyGenAHat :: PolyMat,
    keyGenS1 :: PolyVec,
    keyGenS2 :: PolyVec,
    keyGenT :: PolyVec
  }
  deriving (Show)

-- Parameters:
--   q      = 8380417
--   eta    = 2 or 4, depending on the ML-DSA parameter set
--   k      = number of matrix rows
--   l      = number of matrix columns
--   zetas  = the 256-entry ML-DSA NTT twiddle-factor table
--   xi     = 32-byte random seed

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

keyGenFromSeed :: Int -> Int -> Int -> Int -> Poly -> ByteString -> KeyGenResult
keyGenFromSeed q eta k l zetas xi
  | q <= 0 = error "keyGenFromSeed: q must be positive"
  | eta /= 2 && eta /= 4 = error "keyGenFromSeed: eta must be 2 or 4"
  | k <= 0 = error "keyGenFromSeed: k must be positive"
  | l <= 0 = error "keyGenFromSeed: l must be positive"
  | BoxedV.length aHat /= k = error "keyGenFromSeed: ExpandA returned wrong row count"
  | BoxedV.length s1 /= l = error "keyGenFromSeed: ExpandS returned wrong s1 length"
  | BoxedV.length s2 /= k = error "keyGenFromSeed: ExpandS returned wrong s2 length"
  | otherwise =
      KeyGenResult
        { keyGenRho = rho,
          keyGenRhoPrime = rhoPrime,
          keyGenK = key,
          keyGenAHat = aHat,
          keyGenS1 = s1,
          keyGenS2 = s2,
          keyGenT = t
        }
  where
    seeds@(KeyGenSeeds rho rhoPrime key) = expandSeed xi
    aHat = expandA (fromIntegral q) k l rho
    (s1, s2) = expandS (fromIntegral eta) k l rhoPrime
    t = addPolyVec (fromIntegral q) (BoxedV.map (invNtt (fromIntegral q) zetas) (matrixVectorMulNTT (fromIntegral q) aHat (nttPolyVec (fromIntegral q) zetas s1))) s2

keyGen :: Int -> Int -> Int -> Int -> Poly -> IO KeyGenResult
keyGen q eta k l zetas = keyGenFromSeed q eta k l zetas <$> generateXi
