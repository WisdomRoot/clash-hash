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

-- | XOR a 64-bit block into state at bit positions [startIdx, startIdx+64)
partialXOR :: BitVector 1600 -> BitVector 64 -> Index 1600 -> BitVector 1600
partialXOR state block startIdx =
  let stateVec = unpack state :: Vec 1600 Bit
      blockVec = unpack block :: Vec 64 Bit

      stateVec' = imap applyXor stateVec
        where
          applyXor :: Index 1600 -> Bit -> Bit
          applyXor i b
            | i >= startIdx && i < startIdx + 64 =
                let offset :: Index 1600
                    offset = i - startIdx
                    offset64 :: Index 64
                    offset64 = resize offset
                 in b `xor` (blockVec !! offset64)
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
          let state' = partialXOR state msg (resize (counter * 64))
           in (Absorb (counter + 1) state', 0)
      | otherwise =
          -- Beat 16: Extract 60 bits, pad with 4 bits, then XOR at position 1024
          let msg60 :: BitVector 60
              _unused :: BitVector 4
              (msg60, _unused) = split msg
              -- SHA3 padding: suffix 0b01 + pad start + pad end = 1,0,1,1
              padding = (0b1011 :: BitVector 4)
              paddedBlock = msg60 ++# padding
              state' = partialXOR state paddedBlock 1024
           in (Permute 0 state', 0)
    step (Permute roundCount state) _msg =
      -- Run one round of permutation INSIDE step
      let permuted = permute roundCount state
          rateBlock = leToPlusKN @1088 @1600 takeI (unpack permuted) :: Vec 1088 Bit
          digest = leToPlusKN @digest @1088 takeI rateBlock :: Vec digest Bit
       in (Permute (roundCount + 1) permuted, pack digest)
