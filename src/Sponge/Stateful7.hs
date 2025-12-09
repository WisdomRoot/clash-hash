{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Sponge.Stateful7
  ( -- * Stateful7 Sponge
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

-- | Stateful sponge with AXI4-Stream backpressure support
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
  Signal dom Bool -> -- tready input (backpressure)
  Signal dom (BitVector MsgBits) -> -- Input message
  Signal dom (AXI4Stream DigestBits) -- Output digest (AXI4-Stream)
sponge permute treadySig msgSig = mealy step (State (Absorb 0) 0) (bundle (treadySig, msgSig))
  where
    step :: State -> (Bool, BitVector MsgBits) -> (State, AXI4Stream DigestBits)
    step (State (Absorb counter) state) (_tready, msg)
      | counter < 16 =
          let state' = S5.staticXOR state msg counter
           in (State (Absorb (counter + 1)) state', idleStream)
      | otherwise =
          -- Beat 16: Extract 60 bits, pad with 4 bits, then XOR at position 1024 (16 * 64)
          let msg60 :: Vec 60 Bit
              msg60 = take d60 (unpack msg) -- low 60 bits carry the remaining message
              -- SHA3 padding bits (msb..lsb) = 0,1,1,1 live in the least-significant nibble
              padding = (0b0111 :: BitVector 4)
              paddedBlock = pack msg60 ++# padding
              state' = S5.staticXOR state paddedBlock 16
           in (State (Permute 0) state', idleStream)
    step (State (Permute 23) state) (_tready, _msg) = (State (Squeeze 0) (permute 23 state), idleStream)
    step (State (Permute count) state) (_tready, _msg) = (State (Permute (count + 1)) (permute count state), idleStream)
    -- Squeeze phase with backpressure: only advance if tready is True
    step (State (Squeeze 0) state) (tready, _msg) =
      let outStream = AXI4Stream {tdata = slice (SNat @1599) (SNat @1536) state, tvalid = True, tlast = False}
          nextState = if tready then State (Squeeze 1) state else State (Squeeze 0) state
       in (nextState, outStream)
    step (State (Squeeze 1) state) (tready, _msg) =
      let outStream = AXI4Stream {tdata = slice (SNat @1535) (SNat @1472) state, tvalid = True, tlast = False}
          nextState = if tready then State (Squeeze 2) state else State (Squeeze 1) state
       in (nextState, outStream)
    step (State (Squeeze 2) state) (tready, _msg) =
      let outStream = AXI4Stream {tdata = slice (SNat @1471) (SNat @1408) state, tvalid = True, tlast = False}
          nextState = if tready then State (Squeeze 3) state else State (Squeeze 2) state
       in (nextState, outStream)
    step (State (Squeeze _) state) (tready, _msg) =
      let outStream = AXI4Stream {tdata = slice (SNat @1407) (SNat @1344) state, tvalid = True, tlast = True}
          nextState = if tready then State (Absorb 0) state else State (Squeeze 3) state
       in (nextState, outStream)

idleStream :: AXI4Stream DigestBits
idleStream = AXI4Stream {tdata = 0, tvalid = False, tlast = False}
