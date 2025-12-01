{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Incremental stateful SHA3-256 implementation
-- Starting from Hash.Combinational and making it stateful step by step
module Hash.Stateful (stateful0, stateful1, stateful2) where

import Clash.Prelude
import qualified Hash.Combinational

-- | Number of rate-blocks needed for padded message
type PaddedBlocks rate msgBits = DivRU (msgBits + 2) rate

-- ============================================================================
-- Step 0: Baseline - Copy Hash.Combinational.truncated
-- ============================================================================

-- | Step 0: Direct copy of truncated function
-- This should pass tests immediately - it's the proven working code
stateful0 ::
  forall digest msgBits n.
  ( KnownNat digest, KnownNat msgBits, KnownNat n
  , digest <= 1088
  , n ~ PaddedBlocks 1088 msgBits
  , msgBits + 2 <= n * 1088
  , msgBits + 4 <= n * 1088
  ) =>
  Vec msgBits Bit ->     -- Message (without suffix)
  Vec digest Bit         -- Digest (e.g., 256 bits for SHA3-256)
stateful0 = Hash.Combinational.truncated @digest @msgBits @n

-- ============================================================================
-- Step 1: Make it Registered (Add register, keep pure function)
-- ============================================================================

-- | Step 1: Add clock domain but keep same pure computation
-- The register adds 1-cycle delay
stateful1 ::
  forall dom digest msgBits n.
  ( HiddenClockResetEnable dom
  , KnownNat digest, KnownNat msgBits, KnownNat n
  , digest <= 1088
  , n ~ PaddedBlocks 1088 msgBits
  , msgBits + 2 <= n * 1088
  , msgBits + 4 <= n * 1088
  ) =>
  Signal dom (Vec msgBits Bit) ->
  Signal dom (Vec digest Bit)
stateful1 msgSig = register (repeat 0) digestSig
  where
    digestSig = fmap (Hash.Combinational.truncated @digest @msgBits @n) msgSig

-- ============================================================================
-- Step 2: Convert to Mealy Machine (with trivial empty state)
-- ============================================================================

-- | Step 2: Use Mealy machine form, but with trivial () state
-- Same functionality as stateful1, just different structure
stateful2 ::
  forall dom digest msgBits n.
  ( HiddenClockResetEnable dom
  , KnownNat digest, KnownNat msgBits, KnownNat n
  , digest <= 1088
  , n ~ PaddedBlocks 1088 msgBits
  , msgBits + 2 <= n * 1088
  , msgBits + 4 <= n * 1088
  ) =>
  Signal dom (Vec msgBits Bit) ->
  Signal dom (Vec digest Bit)
stateful2 = mealy step ()
  where
    step :: () -> Vec msgBits Bit -> ((), Vec digest Bit)
    step () msg = ((), Hash.Combinational.truncated @digest @msgBits @n msg)
