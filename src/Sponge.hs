{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Sponge
  ( SpongeParameter,
    sponge,
  )
where

import Clash.Prelude

-- | Constraints for the generic Keccak sponge construction.
--
-- The parameters follow the notation from the specification: @b@ is the
-- permutation width, @r@ the rate, @c = b - r@ the capacity, @n@ the number of
-- rate-sized blocks after padding, @m@ the message length, and @d@ the desired
-- output length.
type SpongeParameter b r n m k d =
  ( KnownNat b,
    KnownNat r,
    KnownNat n,
    KnownNat m,
    KnownNat k,
    KnownNat d,
    1 <= b,
    1 <= r,
    r <= b - 1,
    r <= b,
    1 <= n,
    n ~ (m + r + 1) `Div` r,
    m + 2 <= n * r,
    n * r <= m + r + 1,
    k ~ d `Div` r,
    d <= (k + 1) * r
  )

-- | Generic Keccak sponge function operating on bit strings. The caller
-- supplies the permutation and the type-level parameters fix the padding and
-- fold sizes.
sponge :: forall b r n m k d.
          (SpongeParameter b r n m k d)
       => (BitVector b -> BitVector b)
       -> BitVector m
       -> BitVector d
sponge f = trunc . squeeze . absorb . pad
  where
    pad :: BitVector m -> Vec n (BitVector r)
    pad x = unconcatBitVector# @n @r paddedBits
      where
        paddedBits :: BitVector (n * r)
        paddedBits = x ++# (1 :: BitVector 1) ++# (0 :: BitVector (n * r - (m + 2))) ++# (1 :: BitVector 1)

    absorb :: Vec n (BitVector r) -> BitVector b
    absorb = foldl g 0
      where
        g s block = f (s `xor` ((0 :: BitVector (b - r)) ++# block))

    squeeze :: BitVector b -> Vec (k + 1) (BitVector r)
    squeeze = map (leToPlusKN @r @b truncateB) . iterateI f

    trunc :: Vec (k + 1) (BitVector r) -> BitVector d
    trunc blocks =
      let fullBits = concatBitVector# @(k + 1) @r blocks
          -- Shift right to drop the extra bits at the bottom, then truncate to get top d bits
      in leToPlusKN @d @((k + 1) * r) (\bv -> truncateB (bv `shiftR` (natToNum @((k + 1) * r - d)))) fullBits
