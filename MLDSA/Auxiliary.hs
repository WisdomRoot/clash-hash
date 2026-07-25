{-# LANGUAGE DataKinds #-}

module MLDSA.Auxiliary where

import Data.Bits (shiftL, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)

class DomainSpec domain where
  domainSalt :: domain -> ByteString
  domainSeedLength :: domain -> Int
  domainExpLength :: domain -> Int

data PQCDomain = PQC_MLDSA_256_192 | PQC_MLDSA_256_134 | PQC_MLDSA_512_396

instance DomainSpec PQCDomain where
  domainSalt PQC_MLDSA_256_192 = BS.pack [0x8C, 0x29, 0xAB, 0x2B]
  domainSalt PQC_MLDSA_256_134 = BS.pack [0xDE, 0x10, 0x27, 0x03]
  domainSalt PQC_MLDSA_512_396 = BS.pack [0x96, 0x75, 0x3C, 0x76]

  domainSeedLength PQC_MLDSA_256_192 = 48
  domainSeedLength PQC_MLDSA_256_134 = 48
  domainSeedLength PQC_MLDSA_512_396 = 48

  domainExpLength PQC_MLDSA_256_192 = 656
  domainExpLength PQC_MLDSA_256_134 = 448
  domainExpLength PQC_MLDSA_512_396 = 1280

intToBits :: Int -> Int -> [Word8]
intToBits x alpha = take alpha $ map (fromIntegral . (`mod` 2)) $ iterate (`div` 2) x

bitsToInt :: [Word8] -> Int -> Int
bitsToInt bits alpha = foldr (\bit acc -> fromIntegral bit + 2 * acc) 0 (take alpha bits)

intToBytes :: Int -> Int -> BS.ByteString
intToBytes x alpha = BS.pack $ take alpha $ map (fromIntegral . (`mod` 256)) $ iterate (`div` 256) x

bitsToBytes :: [Word8] -> BS.ByteString
bitsToBytes bits =
  let byteChunks = chunksOf 8 bits
      bytes = map (\chunk -> fromIntegral (bitsToInt chunk 8)) byteChunks
   in BS.pack bytes

bytesToBits :: ByteString -> [Word8]
bytesToBits bytes = concatMap (\b -> intToBits (fromIntegral b) 8) (BS.unpack bytes)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

power2Round :: Integer -> Integer -> (Integer, Integer)
power2Round q r =
  let rp = r `mod` q
      t2 = (r - rp) `div` 2
   in (t2, rp)
