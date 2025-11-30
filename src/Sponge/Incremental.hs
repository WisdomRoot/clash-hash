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

-- ============================================================================
-- Step 3: squeeze . absorb . pad (message → squeezed blocks)
-- ============================================================================

-- | sponge3 = squeeze . sponge2
-- Squeeze output blocks from absorbed state using keccakF1600
sponge3 ::
  forall msgBits n.
  ( KnownNat msgBits, KnownNat n
  , n ~ PaddedBlocks 1088 msgBits
  , msgBits + 2 <= n * 1088
  , msgBits + 4 <= n * 1088
  ) =>
  BitString msgBits ->         -- Message (without suffix)
  Vec 1 (BitString 1088)       -- Squeezed blocks (1 block for SHA3-256)
sponge3 msg =
  let state = sponge2 msg  -- Step 2: absorb . pad
      -- squeeze: map takeI . iterateI keccakF1600
      -- For SHA3-256: need 1 block (256 bits fits in 1088-bit rate)
   in map (leToPlusKN @1088 @1600 takeI) $ iterateI keccakF1600Wrapper state
  where
    keccakF1600Wrapper :: BitString 1600 -> BitString 1600
    keccakF1600Wrapper s =
      let stateAsBitVector = pack s :: BitVector 1600
          permuted = KeccakF1600.Permutation.keccakF1600 stateAsBitVector
       in unpack permuted

-- ============================================================================
-- Step 4: trunc . squeeze . absorb . pad (message → digest)
-- ============================================================================

-- | sponge4 = trunc . sponge3
-- Take the first digest bits from the squeezed rate block (single block case).
sponge4 ::
  forall digest msgBits n.
  ( KnownNat digest, KnownNat msgBits, KnownNat n
  , digest <= 1088
  , n ~ PaddedBlocks 1088 msgBits
  , msgBits + 2 <= n * 1088
  , msgBits + 4 <= n * 1088
  ) =>
  BitString msgBits ->     -- Message (without suffix)
  BitString digest         -- Digest (e.g., 256 bits for SHA3-256)
sponge4 msg =
  let squeezedBlocks = sponge3 @msgBits @n msg  -- Step 3
      firstBlock = head squeezedBlocks              -- Only need the first 1088-bit block
   in leToPlusKN @digest @1088 takeI firstBlock     -- Truncate to digest width
