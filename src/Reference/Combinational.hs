{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Reference sponge implementation with exposed intermediate steps
-- Extracted from SHA3.hs for testing
module Reference.Combinational (pad, absorb, squeeze, truncate) where

import Clash.Prelude hiding (truncate)
import Reference.SHA3 qualified as SHA3

-- | Type alias for BitString (Vec of Bit)
type BitString n = Vec n Bit

-- | Step 1: pad (message → padded blocks)
--
-- Reference: Pad message to rate-sized blocks
pad ::
  forall msgBits n.
  ( KnownNat msgBits,
    KnownNat n,
    msgBits + 2 <= n * 1088,
    msgBits + 4 <= n * 1088 -- Need room for msg + suffix (2) + pad start (1) + pad end (1)
  ) =>
  BitString msgBits -> -- Message (without suffix)
  Vec n (BitString 1088) -- Padded blocks
pad msg =
  let msgWithSuffix = msg ++ unpack (0b01 :: BitVector 2)
   in unconcatI @n @1088
        $ msgWithSuffix
        ++ singleton 1
        ++ repeat @(n * 1088 - (msgBits + 4)) 0
        ++ singleton 1

-- ============================================================================
-- Step 2: absorb . pad (message → absorbed state)
-- ============================================================================

-- | absorbed = absorb . padded
-- Absorb padded blocks into state using SHA3.keccakf
absorb ::
  forall msgBits n.
  ( KnownNat msgBits,
    KnownNat n,
    msgBits + 2 <= n * 1088,
    msgBits + 4 <= n * 1088
  ) =>
  BitString msgBits -> -- Message (without suffix)
  BitString 1600 -- Absorbed state
absorb msg =
  let blocks = pad @msgBits @n msg -- Step 1: pad
   in foldl g (repeat 0) blocks
   where
          g :: BitString 1600 -> BitString 1088 -> BitString 1600
          g s block = SHA3.keccakf (zipWith xor s (block ++ repeat @512 0))

-- ============================================================================
-- Step 3: squeeze . absorb . pad (message → squeezed blocks)
-- ============================================================================

-- | squeezed = squeeze . absorbed
-- Squeeze output blocks from absorbed state using SHA3.keccakf
squeeze ::
  forall msgBits n.
  ( KnownNat msgBits,
    KnownNat n,
    msgBits + 2 <= n * 1088,
    msgBits + 4 <= n * 1088
  ) =>
  BitString msgBits -> -- Message (without suffix)
  Vec 1 (BitString 1088) -- Squeezed blocks (1 block for SHA3-256)
squeeze msg =
  let state = absorb @msgBits @n msg -- Step 2: absorb . pad
  -- squeeze: map takeI . iterateI SHA3.keccakf
   in map (leToPlusKN @1088 @1600 takeI) $ iterateI SHA3.keccakf state

-- ============================================================================
-- Step 4: trunc . squeeze . absorb . pad (message → digest)
-- ============================================================================

-- | truncated = trunc . squeezed
-- Take the first digest bits from the squeezed rate block (single block case).
truncate ::
  forall digest msgBits n.
  ( KnownNat digest,
    KnownNat msgBits,
    KnownNat n,
    digest <= 1088,
    msgBits + 2 <= n * 1088,
    msgBits + 4 <= n * 1088
  ) =>
  BitString msgBits -> -- Message (without suffix)
  BitString digest -- Digest (e.g., 256 bits for SHA3-256)
truncate msg =
  let squeezedBlocks = squeeze @msgBits @n msg -- Step 3
      firstBlock = head squeezedBlocks -- Only need the first 1088-bit block
   in leToPlusKN @digest @1088 takeI firstBlock -- Truncate to digest width
