module MLDSA.NTT ( ntt,invNtt) where

import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import Data.STRef (modifySTRef', newSTRef, readSTRef)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

nttSize :: Integer
nttSize = 256

fipsQ :: Integer
fipsQ = 8380417

inverseNttFactor :: Integer
inverseNttFactor = 8347681

ntt :: Integer -> V.Vector Integer -> V.Vector Integer -> V.Vector Integer
ntt q zetas w
  | q /= fipsQ =
      error "ntt: FIPS 204 requires q = 8380417"
  | V.length w /= nttSize =
      error "ntt: input must contain exactly 256 coefficients"
  | V.length zetas < nttSize =
      error "ntt: zetas must contain entries indexed from 0 to 255"
  | otherwise =
      runST $ do
        wHat <- V.thaw (V.map (modQ q) w)
        mRef <- newSTRef 0

        let loopLen len = when (len >= 1) $ do
                let loopStart start = when (start < nttSize) $ do
                        modifySTRef' mRef (+ 1)
                        m <- readSTRef mRef

                        let z = modQ q (zetas V.! m)

                        forM_ [start .. start + len - 1] $ \j -> do
                          wj <- VM.read wHat j
                          wjLen <- VM.read wHat (j + len)

                          let t = mulMod q z wjLen

                          VM.write wHat (j + len) (modQ q (wj - t))
                          VM.write wHat j (modQ q (wj + t))

                        loopStart (start + 2 * len)

                loopStart 0

                loopLen (len `div` 2)

        loopLen 128

        V.freeze wHat

invNtt :: Integer -> V.Vector Integer -> V.Vector Integer -> V.Vector Integer
invNtt q zetas wHatInput
  | q /= fipsQ =
      error "invNtt: FIPS 204 requires q = 8380417"
  | V.length wHatInput /= nttSize =
      error "invNtt: input must contain exactly 256 coefficients"
  | V.length zetas < nttSize =
      error "invNtt: zetas must contain entries indexed from 0 to 255"
  | otherwise =
      runST $ do
        w <- V.thaw (V.map (modQ q) wHatInput)

        mRef <- newSTRef nttSize

        let loopLen len = when (len < nttSize) $ do
                let loopStart start = when (start < nttSize) $ do
                        modifySTRef' mRef (subtract 1)
                        m <- readSTRef mRef

                        let z = modQ q (negate (zetas V.! m))

                        forM_ [start .. start + len - 1] $ \j -> do
                          t <- VM.read w j
                          wjLen <- VM.read w (j + len)
                          VM.write w j (modQ q (t + wjLen))
                          VM.write w (j + len) (mulMod q z (t - wjLen))

                        loopStart (start + 2 * len)

                loopStart 0
                loopLen (2 * len)

        loopLen 1

        forM_ [0 .. nttSize - 1] $ \j -> do
          x <- VM.read w j
          VM.write w j (mulMod q inverseNttFactor x)

        V.freeze w

modQ :: Integer -> Integer -> Integer
modQ q x = x `mod` q

mulMod :: Integer -> Integer -> Integer -> Integer
mulMod q a b =
  fromInteger $
    (toInteger a * toInteger b) `mod` toInteger q

roundTripTest :: V.Vector Integer -> V.Vector Integer -> Bool
roundTripTest zetas input =
  invNtt fipsQ zetas (ntt fipsQ zetas input)
    == V.map (`mod` fipsQ) input
