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
-- Step 1: keccakf . xor (BUILT ON keccakF1600)
-- ============================================================================

-- | XOR first padded block with zero state, then apply keccakF1600
sponge1 ::
  BitString 1088 ->   -- First padded block (rate-sized)
  BitString 1600      -- State after XOR and keccakF1600
sponge1 block =
  let zeroState = repeat @1600 0 :: BitString 1600
      -- XOR: block goes into rate portion, capacity stays zero
      xorState = block ++ repeat @512 0 :: BitString 1600
      stateAsBitVector = pack xorState :: BitVector 1600
      permuted = KeccakF1600.Permutation.keccakF1600 stateAsBitVector
   in unpack permuted

-- ============================================================================
-- Step 2: sponge1 . pad (BUILT ON sponge1)
-- ============================================================================

-- | sponge2 = sponge1 . pad
sponge2 ::
  BitString 26 ->   -- Message with suffix (for "abc")
  BitString 1600    -- State after pad, xor, keccakF1600
sponge2 msgWithSuffix =
  let -- pad
      padStart = singleton 1 :: Vec 1 Bit
      padEnd = singleton 1 :: Vec 1 Bit
      padZeros = repeat @(1088 - 26 - 2) 0 :: Vec (1088 - 26 - 2) Bit
      firstBlock = msgWithSuffix ++ padStart ++ padZeros ++ padEnd :: Vec 1088 Bit
   in sponge1 firstBlock
