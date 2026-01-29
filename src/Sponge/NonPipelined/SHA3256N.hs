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

-- | Squeeze phase bit slicing helper: extracts 544-bit chunks from the Keccak state
squeezeSlice :: Index 2 -> BitVector 1600 -> BitVector 544
squeezeSlice 0 state = rev $ slice (SNat @1599) (SNat @1056) state
squeezeSlice _ state = rev $ slice (SNat @1055) (SNat @512) state

xorFullRate :: BitVector 1600 -> BitVector 1088 -> Index 1 -> BitVector 1600
xorFullRate state block _ =
  let sliceState = rev (slice (SNat @1599) (SNat @512) state)
      updated = sliceState `xor` block
   in setSlice (SNat @1599) (SNat @512) (rev updated) state

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
    step :: State 1 (Index 2) -> (AXI4Stream MsgBits, Bool, Bool) -> (State 1 (Index 2), (AXI4Stream DigestBits, Bool))
    step (State (Absorb counter) state) (input, _tready, flush) = absorb pad xorFullRate counter state input flush
    step (State (Permute counter seenTLAST) state) (_msg, tready, _flush) = permute permModule pad counter seenTLAST state tready
    step (State (Squeeze counter) state) (_msg, tready, _flush)
      | counter == maxBound =
          let outStream = AXI4Stream {tdata = squeezeSlice counter state, tvalid = True, tlast = True}
              nextState = if tready then State (Absorb 0) 0 else State (Squeeze counter) state
           in (nextState, (outStream, False))
      | otherwise =
          let outStream = AXI4Stream {tdata = squeezeSlice counter state, tvalid = True, tlast = False}
              nextState = if tready then State (Squeeze (counter + 1)) state else State (Squeeze counter) state
           in (nextState, (outStream, False))
