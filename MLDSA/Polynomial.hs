module MLDSA.Polynomial
  ( Poly,
    PolyVec,
    PolyMat,
    nttPolyVec,
    addPoly,
    mulPolyNTT,
    zeroPoly,
    dotProductNTT,
    matrixVectorMulNTT,
    addPolyVec,
    zeroVector
  )
where

import qualified Data.Vector as V
import MLDSA.NTT (ntt)

type Poly = V.Vector Integer

type PolyVec = V.Vector Poly

type PolyMat = V.Vector PolyVec

nttPolyVec :: Integer -> Poly -> PolyVec -> PolyVec
nttPolyVec q zetas = V.map (ntt q zetas)

addPoly :: Integer -> Poly -> Poly -> Poly
addPoly q = V.zipWith $ \x y -> (x + y) `mod` q

subPoly :: Integer -> Poly -> Poly -> Poly
subPoly q = V.zipWith $ \x y -> (x - y + q) `mod` q

negPoly :: Integer -> Poly -> Poly
negPoly q = V.map (\x -> (-x) `mod` q)

mulPolyNTT :: Integer -> Poly -> Poly -> Poly
mulPolyNTT q = V.zipWith $ \x y -> (x * y) `mod` q

zeroPoly :: Poly
zeroPoly = V.replicate 256 0

dotProductNTT :: Integer -> PolyVec -> PolyVec -> Poly
dotProductNTT q xs ys
  | V.length xs /= V.length ys = error "dotProductNTT: vector length mismatch"
  | otherwise = V.foldl' (addPoly q) zeroPoly (V.zipWith (mulPolyNTT q) xs ys)

matrixVectorMulNTT :: Integer -> PolyMat -> PolyVec -> PolyVec
matrixVectorMulNTT q matrix vector = V.map (\row -> dotProductNTT q row vector) matrix

addPolyVec :: Integer -> PolyVec -> PolyVec -> PolyVec
addPolyVec q xs ys
  | V.length xs /= V.length ys = error "addPolyVec: vector length mismatch"
  | otherwise = V.zipWith (addPoly q) xs ys

subPolyVec :: Integer -> PolyVec -> PolyVec -> PolyVec
subPolyVec q xs ys
  | V.length xs /= V.length ys = error "subPolyVec: vector length mismatch"
  | otherwise = V.zipWith (subPoly q) xs ys

negPolyVec :: Integer -> PolyVec -> PolyVec
negPolyVec q = V.map (negPoly q)

zeroVector :: Int -> PolyVec
zeroVector n = V.replicate n zeroPoly
