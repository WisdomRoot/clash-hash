{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Sponge.Stateful6
  ( -- * Stateful6 Sponge
    sponge,
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (permute, tlast)
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
  Signal dom (AXI4Stream DigestBits) -- Output digest (AXI4-Stream)
sponge permute = mealy step (State (Absorb 0) 0)
  where
    step :: State -> BitVector MsgBits -> (State, AXI4Stream DigestBits)
    step (State (Absorb counter) state) msg
      | counter < 16 =
          let state' = S5.staticXOR state msg counter
              idleStream = AXI4Stream {tdata = 0, tvalid = False, tready = False, tlast = False}
           in (State (Absorb (counter + 1)) state', idleStream)
      | otherwise =
          -- Beat 16: Extract 60 bits, pad with 4 bits, then XOR at position 1024 (16 * 64)
          let msg60 :: Vec 60 Bit
              msg60 = take d60 (unpack msg) -- low 60 bits carry the remaining message
              -- SHA3 padding bits (msb..lsb) = 0,1,1,1 live in the least-significant nibble
              padding = (0b0111 :: BitVector 4)
              paddedBlock = pack msg60 ++# padding
              state' = S5.staticXOR state paddedBlock 16
              idleStream = AXI4Stream {tdata = 0, tvalid = False, tready = False, tlast = False}
           in (State (Permute 0) state', idleStream)
    step (State (Permute 23) state) _msg =
      let idleStream = AXI4Stream {tdata = 0, tvalid = False, tready = False, tlast = False}
       in (State (Squeeze 0) (permute 23 state), idleStream)
    step (State (Permute count) state) _msg =
      let idleStream = AXI4Stream {tdata = 0, tvalid = False, tready = False, tlast = False}
       in (State (Permute (count + 1)) (permute count state), idleStream)
    step (State (Squeeze 0) state) _msg =
      let outStream = AXI4Stream {tdata = slice (SNat @1599) (SNat @1536) state, tvalid = True, tready = False, tlast = False}
       in (State (Squeeze 1) state, outStream)
    step (State (Squeeze 1) state) _msg =
      let outStream = AXI4Stream {tdata = slice (SNat @1535) (SNat @1472) state, tvalid = True, tready = False, tlast = False}
       in (State (Squeeze 2) state, outStream)
    step (State (Squeeze 2) state) _msg =
      let outStream = AXI4Stream {tdata = slice (SNat @1471) (SNat @1408) state, tvalid = True, tready = False, tlast = False}
       in (State (Squeeze 3) state, outStream)
    step (State (Squeeze _) state) _msg =
      let outStream = AXI4Stream {tdata = slice (SNat @1407) (SNat @1344) state, tvalid = True, tready = False, tlast = True}
       in (State (Absorb 0) state, outStream)
