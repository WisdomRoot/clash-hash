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
  )
where

import qualified Data.Vector as BoxedV
import qualified Data.Vector.Unboxed as V
import MLDSA.NTT (ntt)

type Poly = V.Vector Int

type PolyVec = BoxedV.Vector Poly

type PolyMat = BoxedV.Vector PolyVec

nttPolyVec :: Int -> V.Vector Int -> BoxedV.Vector (V.Vector Int) -> BoxedV.Vector (V.Vector Int)
nttPolyVec q zetas = BoxedV.map (ntt q zetas)

addPoly :: Int -> Poly -> Poly -> Poly
addPoly q = V.zipWith $ \x y -> (x + y) `mod` q

mulPolyNTT :: Int -> Poly -> Poly -> Poly
mulPolyNTT q = V.zipWith $ \x y -> fromInteger $ (toInteger x * toInteger y) `mod` toInteger q

zeroPoly :: Poly
zeroPoly = V.replicate 256 0

dotProductNTT :: Int -> PolyVec -> PolyVec -> Poly
dotProductNTT q xs ys
  | BoxedV.length xs /= BoxedV.length ys = error "dotProductNTT: vector length mismatch"
  | otherwise = BoxedV.foldl' (addPoly q) zeroPoly (BoxedV.zipWith (mulPolyNTT q) xs ys)

matrixVectorMulNTT :: Int -> PolyMat -> PolyVec -> PolyVec
matrixVectorMulNTT q matrix vector = BoxedV.map (\row -> dotProductNTT q row vector) matrix

addPolyVec :: Int -> PolyVec -> PolyVec -> PolyVec
addPolyVec q xs ys
  | BoxedV.length xs /= BoxedV.length ys = error "addPolyVec: vector length mismatch"
  | otherwise = BoxedV.zipWith (addPoly q) xs ys