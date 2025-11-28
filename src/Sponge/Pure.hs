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

-- | Number of rate-blocks needed for message + suffix + padding
-- msgBits message + 2 suffix bits + 1 pad start + at least 1 pad end = msgBits + 4 minimum
type PaddedBlocks rate msgBits = DivRU (msgBits + 4) rate

-- | Pure SHA3 sponge construction
--
-- Takes raw message, appends suffix, applies pad10*1, returns exact digest length.
-- Phase 1: SHA3-only (no SHAKE support)
--
-- = Parameters
-- * @b@ - Permutation width (1600 for Keccak-f[1600])
-- * @rate@ - Rate in bits (1088 for SHA3-256)
-- * @digest@ - Digest size in bits (256 for SHA3-256)
-- * @msgBits@ - Message size in bits (NOT including suffix)
--
-- = Arguments
-- * Suffix - 2-bit domain separation (0b01 for SHA3) - appended to message internally
-- * Permutation function - Full permutation (e.g., keccakF1600)
-- * Message - Raw message bits (suffix will be appended)
--
-- = Returns
-- * Exact digest (no truncation needed by caller)
pureSponge ::
  forall b rate digest msgBits.
  ( KnownNat b, KnownNat rate, KnownNat digest, KnownNat msgBits
  , rate <= b, digest <= rate, digest <= b, 1 <= rate
  , msgBits + 2 <= rate * PaddedBlocks rate (msgBits + 2)  -- For padToRateBlocks call
  ) =>
  BitVector 2 ->                  -- ^ Suffix (0b01 for SHA3)
  (BitVector b -> BitVector b) -> -- ^ Permutation function (all rounds)
  BitVector msgBits ->            -- ^ Raw message
  BitVector digest                -- ^ Digest output
pureSponge suffix perm msg =
  -- Convert to Vec, append suffix using Vec ++, then pack back to BitVector
  let msgVec = unpack msg :: Vec msgBits Bit
      suffixVec = unpack suffix :: Vec 2 Bit
      msgWithSuffixVec = msgVec ++ suffixVec
      msgWithSuffix = pack msgWithSuffixVec :: BitVector (msgBits + 2)
      paddedBlocks = padToRateBlocks @rate msgWithSuffix
      zeroState = 0 :: BitVector b
      absorbState = foldl absorbBlock zeroState paddedBlocks
      -- Extract low 'digest' bits from state
   in resize absorbState
  where
    absorbBlock :: BitVector b -> BitVector rate -> BitVector b
    absorbBlock st block =
      -- XOR block into rate portion (low bits), then permute
      -- ++# puts left in HIGH bits, so: zeros (capacity) in high, block (rate) in low
      let blockExtended = (0 :: BitVector (b - rate)) ++# block
          st' = st `xor` blockExtended
       in perm st'

-- | Pad message to multiple of rate blocks using pad10*1 rule
--
-- = Padding format
-- @M || 1 || 0...0 || 1@
--
-- For SHA3-256 (rate=1088):
-- * Message at bits [msgBits-1:0]
-- * Pad start at bit [msgBits]
-- * Pad zeros at bits [rate-2:msgBits+1]
-- * Pad end at bit [rate-1]
--
-- = Edge cases
-- * If msgBits+2 > rate: spans multiple blocks
-- * If msgBits == rate: requires full padding block
padToRateBlocks ::
  forall rate msgBits.
  ( KnownNat rate, KnownNat msgBits, 1 <= rate
  , msgBits <= rate * PaddedBlocks rate msgBits  -- Needed for ++# type inference
  ) =>
  BitVector msgBits ->          -- ^ Message (with suffix already appended)
  Vec (PaddedBlocks rate msgBits) (BitVector rate)
padToRateBlocks msg =
  let msgBitsNat = natToNum @msgBits
      totalBits = natToNum @(rate * PaddedBlocks rate msgBits)

      -- Extend message to full size with zeros
      -- ++# puts LEFT operand in HIGH bits, so put zeros on left, msg on right
      msgExtended = (0 :: BitVector (rate * PaddedBlocks rate msgBits - msgBits)) ++# msg

      -- Create masks for the padding bits
      padStartMask = bit msgBitsNat :: BitVector (rate * PaddedBlocks rate msgBits)
      padEndMask = bit (totalBits - 1) :: BitVector (rate * PaddedBlocks rate msgBits)

      -- OR them together
      fullPadded = msgExtended .|. padStartMask .|. padEndMask

   in bitCoerce fullPadded
