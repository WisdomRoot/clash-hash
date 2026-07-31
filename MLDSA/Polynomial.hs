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

import qualified Data.Vector as BoxedV
import qualified Data.Vector.Unboxed as V
import MLDSA.NTT (ntt)

type Poly = V.Vector Integer

type PolyVec = BoxedV.Vector Poly

type PolyMat = BoxedV.Vector PolyVec

nttPolyVec :: Integer -> Poly -> PolyVec -> PolyVec
nttPolyVec q zetas = BoxedV.map (ntt q zetas)

addPoly :: Integer -> Poly -> Poly -> Poly
addPoly q = V.zipWith $ \x y -> (x + y) `mod` q

subPoly :: Integer -> Poly -> Poly -> Poly
subPoly q = V.zipWith $ \x y -> (x - y + q) `mod` q

negPoly :: Integer -> Poly -> Poly
negPoly q = V.map (q -)

mulPolyNTT :: Integer -> Poly -> Poly -> Poly
mulPolyNTT q = V.zipWith $ \x y -> fromIntegral ((fromIntegral x * fromIntegral y) `mod` fromIntegral q)

zeroPoly :: Poly
zeroPoly = V.replicate 256 0

dotProductNTT :: Integer -> PolyVec -> PolyVec -> Poly
dotProductNTT q xs ys
  | BoxedV.length xs /= BoxedV.length ys = error "dotProductNTT: vector length mismatch"
  | otherwise = BoxedV.foldl' (addPoly q) zeroPoly (BoxedV.zipWith (mulPolyNTT q) xs ys)

matrixVectorMulNTT :: Integer -> PolyMat -> PolyVec -> PolyVec
matrixVectorMulNTT q matrix vector = BoxedV.map (\row -> dotProductNTT q row vector) matrix

addPolyVec :: Integer -> PolyVec -> PolyVec -> PolyVec
addPolyVec q xs ys
  | BoxedV.length xs /= BoxedV.length ys = error "addPolyVec: vector length mismatch"
  | otherwise = BoxedV.zipWith (addPoly q) xs ys

subPolyVec :: Integer -> PolyVec -> PolyVec -> PolyVec
subPolyVec q xs ys
  | BoxedV.length xs /= BoxedV.length ys = error "subPolyVec: vector length mismatch"
  | otherwise = BoxedV.zipWith (subPoly q) xs ys

negPolyVec :: Integer -> PolyVec -> PolyVec
negPolyVec q = BoxedV.map (negPoly q)

zeroVector :: Integer -> PolyVec
zeroVector n = BoxedV.replicate n zeroPoly
