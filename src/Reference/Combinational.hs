{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Reference sponge implementation with exposed intermediate steps
-- Extracted from SHA3.hs for testing
module Reference.Combinational where

import Clash.Prelude
import qualified Reference.SHA3 as SHA3

-- | Type alias for BitString (Vec of Bit)
type BitString n = Vec n Bit

-- | Step 1: pad (message → padded blocks)
--
-- Reference: Pad message to rate-sized blocks
padded ::
  forall msgBits n.
  ( KnownNat msgBits, KnownNat n
  , msgBits + 2 <= n * 1088
  , msgBits + 4 <= n * 1088  -- Need room for msg + suffix (2) + pad start (1) + pad end (1)
  ) =>
  BitString msgBits ->   -- Message (without suffix)
  Vec n (BitString 1088) -- Padded blocks
padded msg =
  let msgWithSuffix = msg ++ unpack (0b01 :: BitVector 2)
   in unconcatI @n @1088 $
        msgWithSuffix ++ singleton 1 ++ repeat @(n * 1088 - (msgBits + 4)) 0 ++ singleton 1

-- ============================================================================
-- Step 2: absorb . pad (message → absorbed state)
-- ============================================================================

-- | absorbed = absorb . padded
-- Absorb padded blocks into state using SHA3.keccakf
absorbed ::
  forall msgBits n.
  ( KnownNat msgBits, KnownNat n
  , msgBits + 2 <= n * 1088
  , msgBits + 4 <= n * 1088
  ) =>
  BitString msgBits ->   -- Message (without suffix)
  BitString 1600         -- Absorbed state
absorbed msg =
  let blocks = padded @msgBits @n msg  -- Step 1: pad
      -- absorb: foldl (SHA3.keccakf . xor) (repeat 0) blocks
      absorb :: Vec n (BitString 1088) -> BitString 1600
      absorb = foldl g (repeat 0)
        where
          g :: BitString 1600 -> BitString 1088 -> BitString 1600
          g s block = SHA3.keccakf (zipWith xor s (block ++ repeat @512 0))
   in absorb blocks

-- ============================================================================
-- Step 3: squeeze . absorb . pad (message → squeezed blocks)
-- ============================================================================

-- | squeezed = squeeze . absorbed
-- Squeeze output blocks from absorbed state using SHA3.keccakf
squeezed ::
  forall msgBits n.
  ( KnownNat msgBits, KnownNat n
  , msgBits + 2 <= n * 1088
  , msgBits + 4 <= n * 1088
  ) =>
  BitString msgBits ->         -- Message (without suffix)
  Vec 1 (BitString 1088)       -- Squeezed blocks (1 block for SHA3-256)
squeezed msg =
  let state = absorbed @msgBits @n msg  -- Step 2: absorb . pad
      -- squeeze: map takeI . iterateI SHA3.keccakf
   in map (leToPlusKN @1088 @1600 takeI) $ iterateI SHA3.keccakf state

-- ============================================================================
-- Step 4: trunc . squeeze . absorb . pad (message → digest)
-- ============================================================================

-- | truncated = trunc . squeezed
-- Take the first digest bits from the squeezed rate block (single block case).
truncated ::
  forall digest msgBits n.
  ( KnownNat digest, KnownNat msgBits, KnownNat n
  , digest <= 1088
  , msgBits + 2 <= n * 1088
  , msgBits + 4 <= n * 1088
  ) =>
  BitString msgBits ->     -- Message (without suffix)
  BitString digest         -- Digest (e.g., 256 bits for SHA3-256)
truncated msg =
  let squeezedBlocks = squeezed @msgBits @n msg  -- Step 3
      firstBlock = head squeezedBlocks              -- Only need the first 1088-bit block
   in leToPlusKN @digest @1088 takeI firstBlock     -- Truncate to digest width

-- | Absorption function extracted from SHA3.sponge
--
-- Reference: absorb = foldl g $ repeat 0 where g s = f . zipWith xor s . flip (++) (repeat @(b - r) 0)
refAbsorb ::
  forall b rate n.
  ( KnownNat b, KnownNat rate, KnownNat n
  , rate <= b
  ) =>
  (BitString b -> BitString b) ->  -- Permutation function
  Vec n (BitString rate) ->         -- Padded blocks
  BitString b                       -- Final state
refAbsorb f = foldl g (repeat 0)
  where
    g :: BitString b -> BitString rate -> BitString b
    g s block = f (zipWith xor s (block ++ repeat @(b - rate) 0))

-- | Squeezing function extracted from SHA3.sponge
--
-- Reference: squeeze = map (leToPlusKN @r @b takeI) . iterateI f
refSqueeze ::
  forall b rate k.
  ( KnownNat b, KnownNat rate, KnownNat k
  , rate <= b
  ) =>
  (BitString b -> BitString b) ->  -- Permutation function
  BitString b ->                    -- State after absorption
  Vec (k + 1) (BitString rate)      -- Squeezed blocks
refSqueeze f = map (leToPlusKN @rate @b takeI) . iterateI f

-- | Truncation function extracted from SHA3.sponge
--
-- Reference: trunc = leToPlusKN @d @((k + 1) * r) takeI . concat
refTrunc ::
  forall digest k rate.
  ( KnownNat digest, KnownNat k, KnownNat rate
  , digest <= (k + 1) * rate
  ) =>
  Vec (k + 1) (BitString rate) ->  -- Squeezed blocks
  BitString digest                  -- Final digest
refTrunc = leToPlusKN @digest @((k + 1) * rate) takeI . concat

-- | Full sponge using reference components
--
-- This should match SHA3.keccak exactly
refSponge ::
  forall b rate digest msgBits n k.
  ( KnownNat b, KnownNat rate, KnownNat digest, KnownNat msgBits
  , KnownNat n, KnownNat k
  , rate <= b, digest <= rate, digest <= b
  , msgBits + 2 <= n * rate
  , 2 <= n * rate - msgBits  -- Room for pad10*1
  , digest <= (k + 1) * rate
  ) =>
  (BitString b -> BitString b) ->  -- Permutation function
  BitString msgBits ->              -- Message with suffix
  BitString digest                  -- Digest
refSponge perm msg =
  let pad x = unconcatI @n @rate $
        x ++ singleton 1 ++ repeat @(n * rate - msgBits - 2) 0 ++ singleton 1
   in refTrunc @digest @k @rate $
        refSqueeze @b @rate @k perm $
          refAbsorb @b @rate @n perm $
            pad msg
