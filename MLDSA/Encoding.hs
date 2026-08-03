module MLDSA.Encoding where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import Data.Bits (shiftL, (.&.))
import Data.Int (Int64)
import Data.Word (Word8)
import qualified Data.Vector as V

import MLDSA.Polynomial

-- | Calculate bit length for encoding
bitLength :: Integer -> Int
bitLength 0 = 1
bitLength x = go 0 x
  where
    go n 0 = n
    go n y = go (n + 1) (y `div` 2)

packUnsignedValues :: Int -> [Integer] -> BS.ByteString
packUnsignedValues width values =
  bitsToBytes $
    concatMap
      (\x -> intToBits (fromIntegral x) width)
      values

bitPack :: Integer -> Integer -> Poly -> BS.ByteString
bitPack a b poly = bitsToBytes $ concatMap pack poly
  where
    alpha = bitLength (a + b)
    pack coeff = intToBits (fromIntegral (a - coeff)) alpha

simpleBitPack :: Integer -> Poly -> BS.ByteString
simpleBitPack b poly =
  packUnsignedValues width encoded
  where
    width = bitLength b
    encoded = map fromIntegral (V.toList poly)