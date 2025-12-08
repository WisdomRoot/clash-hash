{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Sponge.Stateful6
  ( -- * Stateful6 Sponge
    sponge,
  )
where

import Clash.Prelude hiding (permute)

type MsgBits = 64

data State
  = Absorb (Index 17) (BitVector 1600)
  | Permute (Index 24) (BitVector 1600)
  deriving (Show, Eq, Generic, NFDataX)

-- | XOR a 64-bit block into state at beat position (beatCounter * 64)
--   module                                            area (µm²)   seq area (µm²)    seq %
--   --------------------------------------------------------------------------------------
--   Hash_Stateful5_topEntity_keccakF1600Round          15559.138            0.000     0.00%
--   Hash_Stateful5_topEntity_spongeFSM                 17579.940         8543.920    48.60%
--   Stateful5_SHA3                                     33139.078         8543.920    25.78%

-- [bench] Time/Mem: load 4.23s | compile 9.23s | synth 208.09s | mem 8912.86 MB
partialXOR :: BitVector 1600 -> BitVector 64 -> Index 17 -> BitVector 1600
partialXOR state block beatCounter =
  let -- Compute startIdx in wider type to avoid modulo wrapping
      startIdx = fromIntegral (fromIntegral beatCounter :: Unsigned 11) * 64 :: Index 1600

      blockVec = unpack block :: Vec 64 Bit -- MSB-first
      stateVec' = imap applyXor (unpack state :: Vec 1600 Bit)
        where
          applyXor :: Index 1600 -> Bit -> Bit
          applyXor i b
            | i >= startIdx && i < startIdx + 64 =
                let offset :: Index 1600
                    offset = i - startIdx
                    offset64 :: Index 64
                    offset64 = resize offset
                    blockBit = blockVec !! offset64
                 in b `xor` blockBit
            | otherwise = b
   in pack stateVec'

-- | Optimized XOR using shift-and-mask (BitVector operations only)
--   module                                            area (µm²)   seq area (µm²)    seq %
--   --------------------------------------------------------------------------------------
--   Hash_Stateful5_topEntity_keccakF1600Round          15574.566            0.000     0.00%
--   Hash_Stateful5_topEntity_spongeFSM                 18839.184         8543.920    45.35%
--   Stateful5_SHA3                                     34413.750         8543.920    24.83%
shiftAndMask :: BitVector 1600 -> BitVector 64 -> Index 17 -> BitVector 1600
shiftAndMask state block beatCounter =
  let -- Calculate bit position: beatCounter * 64
      bitPos = fromIntegral beatCounter * 64 :: Int

      -- Resize block to 1600 bits (zero-extended) and shift to position
      blockMask :: BitVector 1600
      blockMask = resize block `shiftL` bitPos
   in -- XOR the block into state at the correct position
      state `xor` blockMask

-- | Case-based XOR using Vec indexing and replace
-- This approach should generate case statements in Verilog
-- Expected pattern: extract via case, XOR 64 bits, write back via case
-- module                                            area (µm²)   seq area (µm²)    seq %
-- --------------------------------------------------------------------------------------
-- Hash_Stateful5_topEntity_keccakF1600Round          15558.340            0.000     0.00%
-- Hash_Stateful5_topEntity_spongeFSM                 17841.950         8543.920    47.89%
-- Stateful5_SHA3                                     33400.290         8543.920    25.58%
caseBasedXOR :: BitVector 1600 -> BitVector 64 -> Index 17 -> BitVector 1600
caseBasedXOR state block beatCounter =
  let stateVec = bitCoerce state :: Vec 25 (BitVector 64)
      xored = stateVec !! beatCounter `xor` block
      stateVec' = replace beatCounter xored stateVec
   in bitCoerce stateVec'

-- | Static case-based XOR using only BitVector slices.
-- Generates a 64-bit slice XOR and a case tree for writing back.
-- module                                            area (µm²)   seq area (µm²)    seq %
-- --------------------------------------------------------------------------------------
-- Hash_Stateful5_topEntity_keccakF1600Round          15047.886            0.000     0.00%
-- Hash_Stateful5_topEntity_spongeFSM                 16291.436         8543.920    52.44%
-- Stateful5_SHA3                                     31339.322         8543.920    27.26%
staticXOR :: BitVector 1600 -> BitVector 64 -> Index 17 -> BitVector 1600
staticXOR state block beatCounter =
  case beatCounter of
    0  -> setSlice d63 d0 (slice d63 d0 state `xor` block) state
    1  -> setSlice d127 d64 (slice d127 d64 state `xor` block) state
    2  -> setSlice d191 d128 (slice d191 d128 state `xor` block) state
    3  -> setSlice d255 d192 (slice d255 d192 state `xor` block) state
    4  -> setSlice d319 d256 (slice d319 d256 state `xor` block) state
    5  -> setSlice d383 d320 (slice d383 d320 state `xor` block) state
    6  -> setSlice d447 d384 (slice d447 d384 state `xor` block) state
    7  -> setSlice d511 d448 (slice d511 d448 state `xor` block) state
    8  -> setSlice d575 d512 (slice d575 d512 state `xor` block) state
    9  -> setSlice d639 d576 (slice d639 d576 state `xor` block) state
    10 -> setSlice d703 d640 (slice d703 d640 state `xor` block) state
    11 -> setSlice d767 d704 (slice d767 d704 state `xor` block) state
    12 -> setSlice d831 d768 (slice d831 d768 state `xor` block) state
    13 -> setSlice d895 d832 (slice d895 d832 state `xor` block) state
    14 -> setSlice d959 d896 (slice d959 d896 state `xor` block) state
    15 -> setSlice d1023 d960 (slice d1023 d960 state `xor` block) state
    16 -> setSlice (SNat @1087) (SNat @1024) (slice (SNat @1087) (SNat @1024) state `xor` block) state
    _  -> state

-- | Chunk-based XOR using slice operations
-- This approach extracts, modifies, and reassembles specific bit ranges
--   module                                            area (µm²)   seq area (µm²)    seq %
--   --------------------------------------------------------------------------------------
--   Hash_Stateful5_topEntity_keccakF1600Round          15559.138            0.000     0.00%
--   Hash_Stateful5_topEntity_spongeFSM                 19673.892         8543.920    43.43%
--   Stateful5_SHA3                                     35233.030         8543.920    24.25%
chunkBasedXOR :: BitVector 1600 -> BitVector 64 -> Index 17 -> BitVector 1600
chunkBasedXOR state block beatCounter =
  let -- Calculate bit position: beatCounter * 64
      bitPos = fromIntegral beatCounter * 64 :: Int

      -- Extract the 64-bit chunk at bitPos, XOR it, then put it back
      -- Method: shift right to align, truncate to get 64 bits, XOR, shift back
      stateTarget = truncateB (state `shiftR` bitPos) :: BitVector 64
      stateTarget' = stateTarget `xor` block

      -- Create a mask for clearing the target 64 bits
      clearMask :: BitVector 1600
      clearMask = complement (resize (maxBound :: BitVector 64) `shiftL` bitPos)
   in -- Clear target bits, then OR in the new value
      (state .&. clearMask) .|. (resize stateTarget' `shiftL` bitPos)

-- | Stateful sponge, no streaming interface, fixed 1084-bit input / 256-bit output
{-# OPAQUE sponge #-}
sponge ::
  forall dom digest n.
  ( HiddenClockResetEnable dom,
    KnownNat digest,
    KnownNat n,
    digest <= 1088,
    n ~ DivRU (MsgBits + 2) 1088,
    MsgBits + 2 <= n * 1088,
    MsgBits + 4 <= n * 1088
  ) =>
  (Index 24 -> BitVector 1600 -> BitVector 1600) -> -- Permutation function
  Signal dom (BitVector MsgBits) -> -- Input message
  Signal dom (BitVector digest) -- Output digest
sponge permute = mealy step (Absorb 0 0)
  where
    step :: State -> BitVector MsgBits -> (State, BitVector digest)
    step (Absorb counter state) msg
      | counter < 16 =
          let state' = staticXOR state msg counter
           in (Absorb (counter + 1) state', 0)
      | otherwise =
          -- Beat 16: Extract 60 bits, pad with 4 bits, then XOR at position 1024 (16 * 64)
          let msg60 :: Vec 60 Bit
              msg60 = take d60 (unpack msg) -- low 60 bits carry the remaining message
              -- SHA3 padding bits (msb..lsb) = 0,1,1,1 live in the least-significant nibble
              padding = (0b0111 :: BitVector 4)
              paddedBlock = pack msg60 ++# padding
              state' = staticXOR state paddedBlock 16
           in (Permute 0 state', 0)
    step (Permute roundCount state) _msg =
      -- Run one round of permutation INSIDE step
      let permuted = permute roundCount state
          rateBlock = leToPlusKN @1088 @1600 takeI (unpack permuted) :: Vec 1088 Bit
          digest = leToPlusKN @digest @1088 takeI rateBlock :: Vec digest Bit
       in (Permute (roundCount + 1) permuted, pack digest)
