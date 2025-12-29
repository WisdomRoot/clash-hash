{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Runtime SHAKE256 implementation using ByteString
--   Wrapper around Reference.SHA3.shake_256
module Reference.SHAKE256Runtime
  ( shake256,
    shake256Native,
  )
where

import Clash.Prelude hiding (fromList)
import Clash.Sized.Vector qualified as V
import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHAKE256 (..))
import Data.Bits qualified as Bits
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Constraint (Dict (..))
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import GHC.TypeLits (SomeNat (..), someNatVal)
import Prelude (Int, fromIntegral, ($), (.), (<>))
import Prelude qualified as P
import Reference.SHA3 qualified as SHA3
import Unsafe.Coerce (unsafeCoerce)

-- | Compute SHAKE256 hash with variable output length (using crypton library)
--
-- The first argument is the output length in bytes.
-- The second argument is the input message.
--
-- >>> shake256 32 "test"  -- 32 bytes (256 bits) output
shake256 :: Int -> ByteString -> ByteString
shake256 outputBytes input =
  let outputBits = outputBytes P.* 8
   in case fromJust (someNatVal (fromIntegral outputBits)) of
        SomeNat (_ :: Proxy n) ->
          convert (hash input :: Digest (SHAKE256 n))

-- | Compute SHAKE256 hash with variable output length (using value-level sponge implementation)
--
-- This function implements the SHAKE256 sponge construction at the value level,
-- avoiding type-level constraints that would require unsafeCoerce.
--
-- The first argument is the output length in bytes.
-- The second argument is the input message.
--
-- >>> shake256Native 32 "test"  -- 32 bytes (256 bits) output
shake256Native :: Int -> ByteString -> ByteString
shake256Native outputBytes input =
  let inputBits = byteStringToBitList input
      outputBits = outputBytes P.* 8
      -- SHAKE256 parameters: b=1600, r=1088, c=512
      resultBits = shake256Sponge inputBits outputBits
   in bitListToByteString resultBits

-- | SHAKE256 sponge construction (value-level implementation)
shake256Sponge :: [Bit] -> Int -> [Bit]
shake256Sponge inputBits outputBits =
  let rate = 1088 -- SHAKE256 rate
      capacity = 512 -- SHAKE256 capacity
      stateSize = 1600 -- Keccak-f[1600]

      -- Pad input (SHAKE256 uses domain separator 0x1F = 11111)
      paddedInput = padShake256 inputBits rate

      -- Absorb phase
      absorbedState = absorbPhase paddedInput rate stateSize

      -- Squeeze phase
      squeezedBits = squeezePhase absorbedState rate outputBits
   in squeezedBits

-- | Pad input for SHAKE256 (domain separator 0x1F, then pad10*1)
padShake256 :: [Bit] -> Int -> [[Bit]]
padShake256 input rate =
  let -- SHAKE256 domain separator: 0x1F = 11111 (4 bits)
      domainSep = [1, 1, 1, 1]
      inputWithDomain = input P.++ domainSep
      inputLen = P.length inputWithDomain

      -- Calculate padding needed for pad10*1
      -- Need to pad to multiple of rate
      totalLen = inputLen P.+ 2 -- +2 for the 1...1 padding
      paddingNeeded = rate P.- (totalLen `P.mod` rate)

      -- pad10*1: append 1, then zeros, then 1
      padded = inputWithDomain P.++ [1] P.++ P.replicate paddingNeeded 0 P.++ [1]

      -- Split into rate-sized blocks
      blocks = chunksOf rate padded
   in blocks

-- | Absorb phase: XOR input blocks into state and apply permutation
absorbPhase :: [[Bit]] -> Int -> Int -> Vec 1600 Bit
absorbPhase blocks rate stateSize =
  let initialState = P.replicate 1600 0
   in P.foldl absorbBlock (V.unsafeFromList initialState) blocks
  where
    absorbBlock :: Vec 1600 Bit -> [Bit] -> Vec 1600 Bit
    absorbBlock state block =
      let -- XOR block into first 'rate' bits of state
          stateList = toList state
          paddedBlock = P.take rate (block P.++ P.repeat 0)
          xored = P.zipWith xor (P.take rate stateList) paddedBlock P.++ P.drop rate stateList
          xoredState = V.unsafeFromList xored :: Vec 1600 Bit
          -- Apply Keccak-f[1600]
          permuted = SHA3.keccakf xoredState
       in permuted

-- | Squeeze phase: extract output bits from state
squeezePhase :: Vec 1600 Bit -> Int -> Int -> [Bit]
squeezePhase state rate outputBits =
  go state []
  where
    go :: Vec 1600 Bit -> [Bit] -> [Bit]
    go s acc
      | P.length acc P.>= outputBits = P.take outputBits acc
      | otherwise =
          let -- Extract 'rate' bits from state
              extracted = P.take rate (toList s)
              newAcc = acc P.++ extracted
              -- Apply Keccak-f[1600] for next block
              permuted = SHA3.keccakf s
           in go permuted newAcc

-- | Split list into chunks of size n
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (chunk, rest) = P.splitAt n xs
   in chunk : chunksOf n rest

-- | Convert ByteString to list of Bits (LSB first for each byte)
byteStringToBitList :: ByteString -> [Bit]
byteStringToBitList bs =
  P.concatMap byteToBits (BS.unpack bs)
  where
    byteToBits :: Word8 -> [Bit]
    byteToBits w = [if Bits.testBit w i then 1 else 0 | i <- [0 .. 7]]

-- | Convert list of Bits to ByteString (LSB first for each byte)
bitListToByteString :: [Bit] -> ByteString
bitListToByteString bits =
  BS.pack (bitsToBytes bits)
  where
    bitsToBytes :: [Bit] -> [Word8]
    bitsToBytes [] = []
    bitsToBytes bs =
      let (chunk, rest) = P.splitAt 8 bs
          paddedChunk = P.take 8 (chunk P.++ P.repeat 0)
          byte = P.foldl accumBit 0 (P.zip [0..] paddedChunk)
       in byte : bitsToBytes rest

    accumBit :: Word8 -> (Int, Bit) -> Word8
    accumBit acc (i, b) = if b P.== 1 then Bits.setBit acc i else acc

-- | Convert ByteString to Vec of Bits
byteStringToBits :: forall n. KnownNat n => ByteString -> Vec n Bit
byteStringToBits bs =
  let bytes = BS.unpack bs
      numBits = natToNum @n
      numBytes = (numBits P.+ 7) `P.div` 8
      paddedBytes = bytes P.++ P.replicate (numBytes P.- P.length bytes) 0
      bits = P.concatMap word8ToBits (P.take numBytes paddedBytes)
   in V.unsafeFromList (P.take numBits bits)
  where
    word8ToBits :: Word8 -> [Bit]
    word8ToBits w = [if Bits.testBit w i then 1 else 0 | i <- [0 .. 7]]

-- | Convert Vec of Bits to ByteString
bitsToByteString :: forall n. KnownNat n => Vec n Bit -> ByteString
bitsToByteString bits =
  let bitsList = toList bits
      bytes = packBytes bitsList
   in BS.pack bytes
  where
    packBytes :: [Bit] -> [Word8]
    packBytes [] = []
    packBytes bs =
      let (chunk, rest) = P.splitAt 8 bs
          byte = P.foldl setBit 0 (P.zip [0 ..] chunk)
       in byte : packBytes rest

    setBit :: Word8 -> (Int, Bit) -> Word8
    setBit acc (i, b) = if b P.== 1 then Bits.setBit acc i else acc

-- | Convert ByteString to Vec Bit (LSB first for each byte)
byteStringToVec :: forall n. KnownNat n => ByteString -> Vec n Bit
byteStringToVec bs =
  let numBits = natToNum @n
      bytes = BS.unpack bs
      -- Convert each byte to 8 bits (LSB first)
      allBits = P.concatMap byteToBits bytes
      -- Take exactly n bits, pad with zeros if needed
      exactBits = P.take numBits (allBits P.++ P.repeat 0)
   in V.unsafeFromList exactBits
  where
    byteToBits :: Word8 -> [Bit]
    byteToBits w = [if Bits.testBit w i then 1 else 0 | i <- [0 .. 7]]

-- | Convert Vec Bit to ByteString (LSB first for each byte)
vecToByteString :: forall n. KnownNat n => Vec n Bit -> ByteString
vecToByteString vec =
  let bits = toList vec
      bytes = bitsToBytes bits
   in BS.pack bytes
  where
    bitsToBytes :: [Bit] -> [Word8]
    bitsToBytes [] = []
    bitsToBytes bs =
      let (chunk, rest) = P.splitAt 8 bs
          -- Pad chunk to 8 bits if needed
          paddedChunk = P.take 8 (chunk P.++ P.repeat 0)
          byte = P.foldl accumBit 0 (P.zip [0..] paddedChunk)
       in byte : bitsToBytes rest

    accumBit :: Word8 -> (Int, Bit) -> Word8
    accumBit acc (i, b) = if b P.== 1 then Bits.setBit acc i else acc
