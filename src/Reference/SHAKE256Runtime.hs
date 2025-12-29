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
  case fromJust (someNatVal (fromIntegral outputBytes)) of
    SomeNat (_ :: Proxy n) ->
      convert (hash input :: Digest (SHAKE256 n))

-- | Compute SHAKE256 hash with variable output length (using Reference.SHA3 implementation)
--
-- This function wraps Reference.SHA3.shake_256 by manually computing type-level
-- parameters and providing constraint witnesses using unsafeCoerce.
--
-- The first argument is the output length in bytes.
-- The second argument is the input message.
--
-- WARNING: Uses unsafeCoerce to bypass type-level constraint checking.
-- The runtime computation of n and k must match the SpongeParameter constraints.
--
-- >>> shake256Native 32 "test"  -- 32 bytes (256 bits) output
shake256Native :: Int -> ByteString -> ByteString
shake256Native outputBytes input =
  let m = BS.length input P.* 8
      d = outputBytes P.* 8
      -- Compute derived parameters according to SpongeParameter constraints
      n = (m P.+ 4 P.+ 1088 P.+ 1) `P.div` 1088
      k = d `P.div` 1088
   in case fromJust (someNatVal (fromIntegral m)) of
        SomeNat (_ :: Proxy m') ->
          case fromJust (someNatVal (fromIntegral d)) of
            SomeNat (_ :: Proxy d') ->
              case fromJust (someNatVal (fromIntegral n)) of
                SomeNat (_ :: Proxy n') ->
                  case fromJust (someNatVal (fromIntegral k)) of
                    SomeNat (_ :: Proxy k') ->
                      withSpongeParameter @m' @d' @n' @k' P.$
                        let inputVec = byteStringToBits @m' input
                            outputVec = SHA3.shake_256 @m' @d' inputVec
                         in bitsToByteString outputVec

-- | Provide a constraint witness for SpongeParameter using unsafeCoerce
--
-- WARNING: This is unsafe! The caller must ensure that the type-level parameters
-- satisfy the SpongeParameter constraints at runtime.
withSpongeParameter ::
  forall m d n k r.
  (KnownNat m, KnownNat d, KnownNat n, KnownNat k) =>
  (SHA3.SpongeParameter 1600 1088 n (m + 4) k d => r) ->
  r
withSpongeParameter f =
  case unsafeCoerce (Dict :: Dict ()) :: Dict (SHA3.SpongeParameter 1600 1088 n (m + 4) k d) of
    Dict -> f

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
