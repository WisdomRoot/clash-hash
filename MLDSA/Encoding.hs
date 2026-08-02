module MLDSA.Encoding where

import qualified Data.ByteString as BS
import MLDSA.Polynomial

bitLength :: Integer -> Int
bitLength 0 = 1
bitLength x = go 0 x
  where
    go n 0 = n
    go n y = go (n + 1) (y `div` 2)

bitPack :: Integer -> Integer -> Poly -> BS.ByteString
bitPack a b poly = bitsToBytes $ concatMap pack poly
  where
    alpha = bitLength (a + b)
    pack coeff = intToBits (fromIntegral (a - coeff)) alpha