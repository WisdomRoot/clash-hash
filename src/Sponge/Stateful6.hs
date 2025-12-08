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

data State
  = Absorb (Index 17) (BitVector 1600)
  | Permute (Index 24) (BitVector 1600)
  | Squeeze (Index 4) (BitVector 1600)
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
sponge permute = mealy step (Absorb 0 0)
  where
    step :: State -> BitVector MsgBits -> (State, BitVector DigestBits)
    step (Absorb counter state) msg
      | counter < 16 =
          let state' = S5.staticXOR state msg counter
           in (Absorb (counter + 1) state', 0)
      | otherwise =
          -- Beat 16: Extract 60 bits, pad with 4 bits, then XOR at position 1024 (16 * 64)
          let msg60 :: Vec 60 Bit
              msg60 = take d60 (unpack msg) -- low 60 bits carry the remaining message
              -- SHA3 padding bits (msb..lsb) = 0,1,1,1 live in the least-significant nibble
              padding = (0b0111 :: BitVector 4)
              paddedBlock = pack msg60 ++# padding
              state' = S5.staticXOR state paddedBlock 16
           in (Permute 0 state', 0)
    step (Permute 23 state) _msg = (Squeeze 0 (permute 23 state), 0)
    step (Permute count state) _msg = (Permute (count + 1) (permute count state), 0)
    step (Squeeze 0 state) _msg = (Squeeze 1 state, slice (SNat @1599) (SNat @1536) state)
    step (Squeeze 1 state) _msg = (Squeeze 2 state, slice (SNat @1535) (SNat @1472) state)
    step (Squeeze 2 state) _msg = (Squeeze 3 state, slice (SNat @1471) (SNat @1408) state)
    step (Squeeze _ state) _msg = (Absorb 0 state, slice (SNat @1407) (SNat @1344) state)
