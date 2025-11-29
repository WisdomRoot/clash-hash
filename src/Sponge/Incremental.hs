{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Incremental bottom-up sponge implementation
-- Built layer by layer, tested against SHA3.hs reference at each step
module Sponge.Incremental where

import Clash.Prelude
import qualified KeccakF1600.Permutation

-- | Type alias for BitString (Vec of Bit)
type BitString n = Vec n Bit

-- | Number of rate-blocks needed for padded message
-- The message already has suffix (2 bits), so we need room for:
--   msgBits + pad start (1 bit) + pad end (1 bit) = msgBits + 2
type PaddedBlocks rate msgBits = DivRU (msgBits + 2) rate

-- ============================================================================
-- Step 1: pad (message → padded blocks)
-- ============================================================================

-- | Pad message to rate-sized blocks
sponge1 ::
  forall msgBits n.
  ( KnownNat msgBits, KnownNat n
  , n ~ PaddedBlocks 1088 msgBits
  , msgBits + 2 <= n * 1088
  , msgBits + 4 <= n * 1088  -- Need room for msg + suffix (2) + pad start (1) + pad end (1)
  ) =>
  BitString msgBits ->   -- Message (without suffix)
  Vec n (BitString 1088) -- Padded blocks
sponge1 msg =
  let msgWithSuffix = msg ++ unpack (0b01 :: BitVector 2)
   in unconcatI @n @1088 $
        msgWithSuffix ++ singleton 1 ++ repeat @(n * 1088 - (msgBits + 4)) 0 ++ singleton 1

-- ============================================================================
-- Step 2: absorb . pad (message → absorbed state)
-- ============================================================================

-- | sponge2 = absorb . sponge1
-- Absorb padded blocks into state using keccakF1600
sponge2 ::
  forall msgBits n.
  ( KnownNat msgBits, KnownNat n
  , n ~ PaddedBlocks 1088 msgBits
  , msgBits + 2 <= n * 1088
  , msgBits + 4 <= n * 1088
  ) =>
  BitString msgBits ->   -- Message (without suffix)
  BitString 1600         -- Absorbed state
sponge2 msg =
  let blocks = sponge1 msg  -- Step 1: pad
      -- absorb: foldl (keccakF1600 . xor) (repeat 0) blocks
      absorb :: Vec n (BitString 1088) -> BitString 1600
      absorb = foldl g (repeat 0)
        where
          g :: BitString 1600 -> BitString 1088 -> BitString 1600
          g s block =
            let xorState = zipWith xor s (block ++ repeat @512 0)
                stateAsBitVector = pack xorState :: BitVector 1600
                permuted = KeccakF1600.Permutation.keccakF1600 stateAsBitVector
             in unpack permuted
   in absorb blocks
