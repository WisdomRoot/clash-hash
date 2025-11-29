{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Reference sponge implementation with exposed intermediate steps
-- Extracted from SHA3.hs for testing
module Sponge.Reference where

import Clash.Prelude
import qualified SHA3

-- | Type alias for BitString (Vec of Bit)
type BitString n = Vec n Bit

-- | Step 1: keccakf . xor (BUILT ON SHA3.keccakf)
--
-- Reference: XOR first padded block with zero state, then apply SHA3.keccakf
refSponge1 ::
  BitString 1088 ->   -- First padded block (rate-sized)
  BitString 1600      -- State after XOR and SHA3.keccakf
refSponge1 block =
  let zeroState = repeat @1600 0 :: BitString 1600
      -- XOR: block goes into rate portion, capacity stays zero
      xorState = block ++ repeat @512 0 :: BitString 1600
   in SHA3.keccakf xorState

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
