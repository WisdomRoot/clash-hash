{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Sponge.Stateful6
  ( -- * Stateful6 Sponge
    sponge,
  )
where

import Clash.Prelude hiding (permute)
import Sponge.Stateful5 qualified as S5

type MsgBits = 64

type DigestBits = 64

-- | Phases of the sponge operation
data Phase
  = Absorb (Index 17)
  | Permute (Index 24)
  | Squeeze (Index 4)
  deriving
    ( Show,
      Eq,
      Generic,
      NFDataX
    )

-- | Internal state of the sponge
--   Note: separating `Phase` from the BitVector state would significantly reduce the size of the multiplexers
data State
  = State Phase (BitVector 1600)
  deriving
    ( Show,
      Eq,
      Generic,
      NFDataX
    )

-- | Stateful sponge, no streaming interface, fixed 1084-bit input / 256-bit output
{-# OPAQUE sponge #-}
sponge ::
  forall dom n.
  ( HiddenClockResetEnable dom,
    KnownNat n,
    n ~ DivRU (MsgBits + 2) 1088,
    MsgBits + 2 <= n * 1088,
    MsgBits + 4 <= n * 1088
  ) =>
  (Index 24 -> BitVector 1600 -> BitVector 1600) -> -- Permutation function
  Signal dom (BitVector MsgBits) -> -- Input message
  Signal dom (BitVector DigestBits) -- Output digest
sponge permute = mealy step (State (Absorb 0) 0)
  where
    step :: State -> BitVector MsgBits -> (State, BitVector DigestBits)
    step (State (Absorb counter) state) msg
      | counter < 16 =
          let state' = S5.staticXOR state msg counter
           in (State (Absorb (counter + 1)) state', 0)
      | otherwise =
          -- Beat 16: Extract 60 bits, pad with 4 bits, then XOR at position 1024 (16 * 64)
          let msg60 :: Vec 60 Bit
              msg60 = take d60 (unpack msg) -- low 60 bits carry the remaining message
              -- SHA3 padding bits (msb..lsb) = 0,1,1,1 live in the least-significant nibble
              padding = (0b0111 :: BitVector 4)
              paddedBlock = pack msg60 ++# padding
              state' = S5.staticXOR state paddedBlock 16
           in (State (Permute 0) state', 0)
    step (State (Permute 23) state) _msg = (State (Squeeze 0) (permute 23 state), 0)
    step (State (Permute count) state) _msg = (State (Permute (count + 1)) (permute count state), 0)
    step (State (Squeeze 0) state) _msg = (State (Squeeze 1) state, slice (SNat @1599) (SNat @1536) state)
    step (State (Squeeze 1) state) _msg = (State (Squeeze 2) state, slice (SNat @1535) (SNat @1472) state)
    step (State (Squeeze 2) state) _msg = (State (Squeeze 3) state, slice (SNat @1471) (SNat @1408) state)
    step (State (Squeeze _) state) _msg = (State (Absorb 0) state, slice (SNat @1407) (SNat @1344) state)
