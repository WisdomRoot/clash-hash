{-# LANGUAGE DataKinds #-}

module MLDSA.KeyGen where

import Crypto.Hash (SHAKE128 (..), SHAKE256 (..), hashWith)
import Crypto.Random (getRandomBytes)
import Data.Bits (shiftL, (.&.))
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Vector as BoxedV
import qualified Data.Vector.Unboxed as V
import Data.Word (Word8)
import MLDSA.Polynomial
import MLDSA.NTT

data KeyGenSeeds = KeyGenSeeds
  { seedRho :: ByteString,
    seedRhoPrime :: ByteString,
    seedK :: ByteString
  }
  deriving (Show)

generateXi :: IO ByteString
generateXi = getRandomBytes 32

expandSeed :: ByteString -> KeyGenSeeds
expandSeed xi =
  let expanded :: ByteString
      expanded = convert $ hashWith (SHAKE256 :: SHAKE256 1024) xi
      rho = BS.take 32 expanded
      rhoPrime = BS.take 64 (BS.drop 32 expanded)
      k = BS.take 32 (BS.drop 96 expanded)
   in KeyGenSeeds rho rhoPrime k

getNewKeyGenSeeds :: IO KeyGenSeeds
getNewKeyGenSeeds = expandSeed <$> generateXi

expandA :: Integer -> Integer -> Integer -> ByteString -> BoxedV.Vector (BoxedV.Vector (V.Vector Integer))
expandA q k l rho =
  BoxedV.generate k $ \r ->
    BoxedV.generate l $ \s ->
      let rhoPrime = rho `BS.snoc` fromIntegral s `BS.snoc` fromIntegral r
       in rejNTTPoly q rhoPrime

expandS :: Integer -> Integer -> Integer -> ByteString -> (BoxedV.Vector (V.Vector Integer), BoxedV.Vector (V.Vector Integer))
expandS eta k l rhoPrime =
  ( BoxedV.generate l $ \r ->
      let rhoNu = rhoPrime `BS.snoc` fromIntegral r `BS.snoc` 0
       in rejBoundedPoly eta rhoNu,
    BoxedV.generate k $ \s ->
      let rhoNu = rhoPrime `BS.snoc` fromIntegral s `BS.snoc` 1
       in rejBoundedPoly eta rhoNu
  )

coeffFromThreeBytes :: Word8 -> Word8 -> Word8 -> Integer
coeffFromThreeBytes b0 b1 b2 =
  let b2' = b2 .&. 0x7F
      z0 = fromIntegral b0
      z1 = fromIntegral b1 `shiftL` 8
      z2 = fromIntegral b2' `shiftL` 16
   in z0 + z1 + z2

rejNTTPoly :: Integer -> ByteString -> V.Vector Integer
rejNTTPoly q rho
  | q <= 0 = error "rejNTTPoly: q must be positive"
  | otherwise = V.fromListN 256 (sample byteStream 0)
  where
    expanded :: ByteString
    expanded =
      convert (hashWith (SHAKE128 :: SHAKE128 8192) rho)

    byteStream :: [Word8]
    byteStream = BS.unpack expanded

    sample :: [Word8] -> Integer -> [Integer]
    sample _ 256 = []
    sample (b0 : b1 : b2 : rest) j =
      let z = coeffFromThreeBytes b0 b1 b2
       in if z < q
            then z : sample rest (j + 1)
            else sample rest j
    sample _ j =
      error $
        "SHAKE128 output exhausted at coefficient "
          ++ show j

rejBoundedPoly :: Integer -> ByteString -> V.Vector Integer
rejBoundedPoly eta rho
  | eta <= 0 = error "rejBoundedPoly: eta must be positive"
  | otherwise = V.fromListN 256 (sample byteStream 0)
  where
    expanded :: ByteString
    expanded = convert (hashWith (SHAKE256 :: SHAKE256 8192) rho)

    byteStream :: [Word8]
    byteStream = BS.unpack expanded

    sample :: [Word8] -> Integer -> [Integer]
    sample _ 256 = []
    sample (b : bs) j =
      let z0 = fromIntegral (b .&. 0x0F)
          z1 = fromIntegral (b `div` 16)
          valid0 = z0 <= 2 * eta
          valid1 = z1 <= 2 * eta
       in case (valid0, valid1) of
            (True, True) -> if j == 255
                then [eta - z0]
                else (eta - z0) : (eta - z1) : sample bs (j + 2)
            (True, False) -> (eta - z0) : sample bs (j + 1)
            (False, True) -> (eta - z1) : sample bs (j + 1)
            (False, False) -> sample bs j
    sample [] j =
      error $ "SHAKE256 output exhausted at coefficient " ++ show j

keygenInternal :: Integer -> Integer -> Integer -> Integer -> V.Vector Integer -> ByteString -> (KeyGenSeeds, PolyMat, PolyVec, PolyVec, PolyVec)
keygenInternal q eta k l zetas xi =
  let seeds@(KeyGenSeeds rho rhoPrime _) =
        expandSeed xi

      aHat =
        expandA q k l rho

      (s1, s2) =
        expandS eta k l rhoPrime

      s1Hat =
        BoxedV.map (ntt q zetas) s1

      tHat =
        matrixVectorMulNTT q aHat s1Hat

      tWithoutS2 =
        BoxedV.map (invNtt q zetas) tHat

      t =
        addPolyVec q tWithoutS2 s2
   in (seeds, aHat, s1, s2, t)