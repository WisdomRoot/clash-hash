{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Sponge.Pure
  ( pureSponge
  , padToRateBlocks
  , PaddedBlocks
  ) where

import Clash.Prelude

-- | Reverse all bits in a BitVector
reverseBits :: forall n. KnownNat n => BitVector n -> BitVector n
reverseBits bv = bitCoerce (reverse (bitCoerce bv :: Vec n Bit))

-- | Number of rate-blocks needed for message + padding
-- msgBits message + 2 suffix bits + 1 pad start + at least 1 pad end = msgBits + 4 minimum
type PaddedBlocks rate msgBits = DivRU (msgBits + 4) rate

-- | Pure SHA3 sponge construction
--
-- Takes raw message, applies pad10*1 internally, returns exact digest length.
-- Phase 1: SHA3-only (no SHAKE support)
--
-- = Parameters
-- * @b@ - Permutation width (1600 for Keccak-f[1600])
-- * @rate@ - Rate in bits (1088 for SHA3-256)
-- * @digest@ - Digest size in bits (256 for SHA3-256)
-- * @msgBits@ - Message size in bits
--
-- = Arguments
-- * Suffix - 2-bit domain separation (0b01 for SHA3)
-- * Permutation function - Full permutation (e.g., keccakF1600)
-- * Message - Raw message bits
--
-- = Returns
-- * Exact digest (no truncation needed by caller)
pureSponge ::
  forall b rate digest msgBits.
  ( KnownNat b, KnownNat rate, KnownNat digest, KnownNat msgBits
  , rate <= b, digest <= rate, digest <= b, 1 <= rate
  ) =>
  BitVector 2 ->                  -- ^ Suffix (0b01 for SHA3)
  (BitVector b -> BitVector b) -> -- ^ Permutation function (all rounds)
  BitVector msgBits ->            -- ^ Raw message
  BitVector digest                -- ^ Digest output
pureSponge suffix permute msg =
  let paddedBlocks = padToRateBlocks @rate suffix msg
      zeroState = 0 :: BitVector b
      absorbState = foldl absorbBlock zeroState paddedBlocks
      -- Extract low 'digest' bits from state
   in resize absorbState
  where
    absorbBlock :: BitVector b -> BitVector rate -> BitVector b
    absorbBlock st block =
      -- XOR block into rate portion (low bits), then permute
      let blockExtended = resize block :: BitVector b
          st' = st `xor` blockExtended
       in permute st'

-- | Pad message to multiple of rate blocks using pad10*1 rule
--
-- = Padding format
-- @M || suffix[1:0] || 1 || 0...0 || 1@
--
-- For SHA3-256 (rate=1088):
-- * Message at bits [msgBits-1:0]
-- * Suffix at bits [msgBits+1:msgBits]
-- * Pad start at bit [msgBits+2]
-- * Pad zeros at bits [rate-2:msgBits+3]
-- * Pad end at bit [rate-1]
--
-- = Edge cases
-- * If msgBits+4 > rate: spans multiple blocks
-- * If msgBits == rate: requires full padding block
padToRateBlocks ::
  forall rate msgBits.
  (KnownNat rate, KnownNat msgBits, 1 <= rate) =>
  BitVector 2 ->                -- ^ Suffix
  BitVector msgBits ->          -- ^ Message
  Vec (PaddedBlocks rate msgBits) (BitVector rate)
padToRateBlocks suffix msg =
  let msgBitsNat = natToNum @msgBits
      rateNat = natToNum @rate
      nBlocks = natToNum @(PaddedBlocks rate msgBits)

      -- Build the full padded bitvector by setting bits
      -- Start with message extended to full length
      msgExtended = resize msg :: BitVector (rate * PaddedBlocks rate msgBits)

      -- Set suffix bits at positions msgBits and msgBits+1
      withSuffix = setBitIf (testBit suffix 0) msgBitsNat $
                   setBitIf (testBit suffix 1) (msgBitsNat + 1) msgExtended

      -- Set pad start bit at position msgBits+2
      withPadStart = setBit withSuffix (msgBitsNat + 2)

      -- Set pad end bit at position (rate * nBlocks - 1)
      fullPadded = setBit withPadStart (rateNat * nBlocks - 1)

   in bitCoerce fullPadded
  where
    setBitIf :: KnownNat n => Bool -> Int -> BitVector n -> BitVector n
    setBitIf cond idx bv = if cond then setBit bv idx else bv
