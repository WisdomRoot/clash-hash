{-# LANGUAGE TypeApplications #-}

module Sponge.Stateful5
  ( -- * Stateful5 Sponge
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
          let state' = partialXOR state msg counter
           in (Absorb (counter + 1) state', 0)
      | otherwise =
          -- Beat 16: Extract 60 bits, pad with 4 bits, then XOR at position 1024 (16 * 64)
          let msg60 :: Vec 60 Bit
              msg60 = take d60 (unpack msg) -- low 60 bits carry the remaining message
              -- SHA3 padding bits (msb..lsb) = 0,1,1,1 live in the least-significant nibble
              padding = (0b0111 :: BitVector 4)
              paddedBlock = pack msg60 ++# padding
              state' = partialXOR state paddedBlock 16
           in (Permute 0 state', 0)
    step (Permute roundCount state) _msg =
      -- Run one round of permutation INSIDE step
      let permuted = permute roundCount state
          rateBlock = leToPlusKN @1088 @1600 takeI (unpack permuted) :: Vec 1088 Bit
          digest = leToPlusKN @digest @1088 takeI rateBlock :: Vec digest Bit
       in (Permute (roundCount + 1) permuted, pack digest)
