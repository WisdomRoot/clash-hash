{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Sponge.NonPipelined.SHA3256N
  ( sponge,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import Sponge.NonPipelinedN

-- | Padding function + XOR, flips 3 bits depending on the current beatCounter
pad :: Index 1 -> BitVector 1600 -> BitVector 1600
pad _ = complementAt 512 . complementAt 1597 . complementAt 1598 -- whole 1088-bit padding

-- | Squeeze phase bit slicing helper: extracts 64-bit chunks from the Keccak state
squeezeSlice :: BitVector 1600 -> BitVector 1088
squeezeSlice = slice (SNat @1599) (SNat @512)

xorFullRate :: BitVector 1600 -> BitVector 1088 -> Index 1 -> BitVector 1600
xorFullRate state block _ =
  let sliceState = slice (SNat @1599) (SNat @512) state
      updated = sliceState `xor` block
   in setSlice (SNat @1599) (SNat @512) updated state

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
  Signal dom (AXI4Stream MsgBits, Bool, Bool) -> -- Input message, output tready, flush signal
  Signal dom (AXI4Stream DigestBits, Bool) -- Output digest (AXI4-Stream), input tready
sponge permModule = mealy step (State (Absorb 0) 0)
  where
    step :: State 1 (Index 1) -> (AXI4Stream MsgBits, Bool, Bool) -> (State 1 (Index 1), (AXI4Stream DigestBits, Bool))
    step (State (Absorb counter) state) (input, _tready, flush) = absorb pad xorFullRate counter state input flush
    step (State (Permute counter seenTLAST) state) (_msg, tready, _flush) = permute permModule pad counter seenTLAST state tready
    step (State (Squeeze _counter) state) (_msg, tready, _flush) =
      let outStream = AXI4Stream {tdata = squeezeSlice state, tvalid = True, tlast = True}
          nextState = if tready then State (Absorb 0) 0 else State (Squeeze 0) state
       in (nextState, (outStream, False))
