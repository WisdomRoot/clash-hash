{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Combinational SHA3-256 implementation
module Hash.Combinational (pad, absorb, squeeze, truncate, hash, topEntity) where

import Clash.Prelude hiding (truncate)
import Permutation qualified

-- | Number of rate-blocks needed for padded message
-- The message already has suffix (2 bits), so we need room for:
--   msgBits + pad start (1 bit) + pad end (1 bit) = msgBits + 2
type PaddedBlocks rate msgBits = DivRU (msgBits + 2) rate

-- ============================================================================
-- Step 1: pad (message → padded blocks)
-- ============================================================================

-- | Pad message to rate-sized blocks
pad ::
  forall msgBits n.
  ( KnownNat msgBits,
    KnownNat n,
    n ~ PaddedBlocks 1088 msgBits,
    msgBits + 2 <= n * 1088,
    msgBits + 4 <= n * 1088 -- Need room for msg + suffix (2) + pad start (1) + pad end (1)
  ) =>
  Vec msgBits Bit -> -- Message (without suffix)
  Vec n (Vec 1088 Bit) -- Padded blocks
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

-- | Absorb padded blocks into state using keccakF1600
absorb ::
  forall msgBits n.
  ( KnownNat msgBits,
    KnownNat n,
    n ~ PaddedBlocks 1088 msgBits,
    msgBits + 2 <= n * 1088,
    msgBits + 4 <= n * 1088
  ) =>
  Vec msgBits Bit -> -- Message (without suffix)
  Vec 1600 Bit -- Absorbed state
absorb msg =
  let blocks = pad msg -- Step 1: pad
   in foldl g (repeat 0) blocks
  where
    -- absorb: foldl (keccakF1600 . xor) (repeat 0) blocks
    g :: Vec 1600 Bit -> Vec 1088 Bit -> Vec 1600 Bit
    g s block =
      let xorState = zipWith xor s (block ++ repeat @512 0)
          stateAsBitVector = pack xorState :: BitVector 1600
          permuted = Permutation.keccakF1600Reversed24 stateAsBitVector
       in unpack permuted

-- ============================================================================
-- Step 3: squeeze . absorb . pad (message → squeezed blocks)
-- ============================================================================

-- | Squeeze output blocks from absorbed state using keccakF1600
squeeze ::
  forall msgBits n.
  ( KnownNat msgBits,
    KnownNat n,
    n ~ PaddedBlocks 1088 msgBits,
    msgBits + 2 <= n * 1088,
    msgBits + 4 <= n * 1088
  ) =>
  Vec msgBits Bit -> -- Message (without suffix)
  Vec 1 (Vec 1088 Bit) -- Squeezed blocks (1 block for SHA3-256)
squeeze msg =
  let state = absorb msg -- Step 2: absorb . pad
  -- squeeze: map takeI . iterateI keccakF1600
  -- For SHA3-256: need 1 block (256 bits fits in 1088-bit rate)
   in map (leToPlusKN @1088 @1600 takeI) $ iterateI keccakF1600Wrapper state
  where
    keccakF1600Wrapper :: Vec 1600 Bit -> Vec 1600 Bit
    keccakF1600Wrapper s =
      let stateAsBitVector = pack s :: BitVector 1600
          permuted = Permutation.keccakF1600Reversed24 stateAsBitVector
       in unpack permuted

-- ============================================================================
-- Step 4: trunc . squeeze . absorb . pad (message → digest)
-- ============================================================================

-- | Take the first digest bits from the squeezed rate block (single block case).
truncate ::
  forall digest msgBits n.
  ( KnownNat digest,
    KnownNat msgBits,
    KnownNat n,
    digest <= 1088,
    n ~ PaddedBlocks 1088 msgBits,
    msgBits + 2 <= n * 1088,
    msgBits + 4 <= n * 1088
  ) =>
  Vec msgBits Bit -> -- Message (without suffix)
  Vec digest Bit -- Digest (e.g., 256 bits for SHA3-256)
truncate msg =
  let squeezedBlocks = squeeze @msgBits @n msg -- Step 3
      firstBlock = head squeezedBlocks -- Only need the first 1088-bit block
   in leToPlusKN @digest @1088 takeI firstBlock -- Truncate to digest width

-- | Fixed-width digest: SHA3-256 over a 64-bit message (no suffix needed).
-- You can change 'MsgBits' to specialize for other fixed-width messages.
type MsgBits = 64

-- | Convenience wrapper returning a 256-bit digest for a fixed-size message.
hash :: BitVector MsgBits -> BitVector 256
hash msgBV =
  let msgBits = unpack msgBV :: Vec MsgBits Bit
      digestBits = truncate @256 @MsgBits msgBits
   in pack digestBits

-- | Clash topEntity for synthesis/simulation.
topEntity :: BitVector MsgBits -> BitVector 256
topEntity = hash
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "combinational",
        t_inputs = [PortName "msg"],
        t_output = PortName "digest"
      }
  )
  #-}
