{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list comprehension" #-}
module Reference.SampleNTT
  ( sampleNTT,
    sampleNTTBS,
    shake128XOF,
    q,
  )
where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word16, Word8)
import Reference.Crypton qualified as Crypton
import Prelude

-- | Kyber/ML-KEM modulus Q = 3329
q :: Word16
q = 3329

-- | Generate 2048 bytes from SHAKE-128 XOF (using crypton library)
-- Pre-generates 2048 bytes to handle unlucky rejection sampling cases
shake128XOF :: ByteString -> [Word8]
shake128XOF seed = BS.unpack (Crypton.shake128 2048 seed)

-- | Extract two 12-bit values from three bytes (little-endian)
-- Returns (d1, d2) where:
--   d1 = b0 + 256 * (b1 & 0x0F)
--   d2 = (b1 >> 4) + 16 * b2
extractTwoCoeffs :: Word8 -> Word8 -> Word8 -> (Word16, Word16)
extractTwoCoeffs b0 b1 b2 =
  let d1 = fromIntegral b0 + 256 * (fromIntegral b1 .&. 0x0F)
      d2 = (fromIntegral b1 `shiftR` 4) + 16 * fromIntegral b2
   in (d1, d2)

-- | Rejection sampling: convert byte stream to valid coefficients
-- Accepts coefficients < 3329, rejects others
rejectionSample :: [Word8] -> [Word16]
rejectionSample = go
  where
    go (b0 : b1 : b2 : rest) =
      let (d1, d2) = extractTwoCoeffs b0 b1 b2
          accepted1 = if d1 < q then [d1] else []
          accepted2 = if d2 < q then [d2] else []
       in accepted1 ++ accepted2 ++ go rest
    go _ = [] -- Not enough bytes (shouldn't happen with 2048-byte buffer)

-- | Sample 256 NTT-domain coefficients from byte stream
-- Implements FIPS 203 Algorithm 6: SampleNTT(B)
sampleNTT :: ByteString -> Vector Word16
sampleNTT seed =
  let byteStream = shake128XOF seed
      coeffs = rejectionSample byteStream
   in V.fromList (take 256 coeffs)

-- | ByteString wrapper for sampleNTT
-- Converts coefficient vector to ByteString (little-endian Word16s)
sampleNTTBS :: ByteString -> ByteString
sampleNTTBS seed =
  let coeffVec = sampleNTT seed
      coeffList = V.toList coeffVec
      bytes = concatMap word16ToBytes coeffList
   in BS.pack bytes
  where
    word16ToBytes :: Word16 -> [Word8]
    word16ToBytes w = [fromIntegral (w .&. 0xFF), fromIntegral (w `shiftR` 8)]
