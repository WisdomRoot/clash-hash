{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Sponge.NonPipelined.SHA3512
  ( sponge,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import Sponge.NonPipelined
import Sponge.XOR qualified as XOR

type PadBeats = 9

type SqueezeBeats = 8

-- | Padding function + XOR, flips 3 bits depending on the current beatCounter.
pad :: Index PadBeats -> BitVector 1600 -> BitVector 1600
pad 0 = complementAt 1024 . complementAt 1533 . complementAt 1534
pad 1 = complementAt 1024 . complementAt 1469 . complementAt 1470
pad 2 = complementAt 1024 . complementAt 1405 . complementAt 1406
pad 3 = complementAt 1024 . complementAt 1341 . complementAt 1342
pad 4 = complementAt 1024 . complementAt 1277 . complementAt 1278
pad 5 = complementAt 1024 . complementAt 1213 . complementAt 1214
pad 6 = complementAt 1024 . complementAt 1149 . complementAt 1150
pad 7 = complementAt 1024 . complementAt 1085 . complementAt 1086
pad _ = complementAt 1024 . complementAt 1597 . complementAt 1598 -- special case for a whole 576-bit padding

-- | Squeeze phase bit slicing helper: extracts 64-bit chunks from the Keccak state.
squeezeSlice :: Index SqueezeBeats -> BitVector 1600 -> BitVector 64
squeezeSlice 0 state = slice (SNat @1599) (SNat @1536) state
squeezeSlice 1 state = slice (SNat @1535) (SNat @1472) state
squeezeSlice 2 state = slice (SNat @1471) (SNat @1408) state
squeezeSlice 3 state = slice (SNat @1407) (SNat @1344) state
squeezeSlice 4 state = slice (SNat @1343) (SNat @1280) state
squeezeSlice 5 state = slice (SNat @1279) (SNat @1216) state
squeezeSlice 6 state = slice (SNat @1215) (SNat @1152) state
squeezeSlice _ state = slice (SNat @1151) (SNat @1088) state

-- | Stateful sponge with AXI4-Stream backpressure support.
{-# OPAQUE sponge #-}
sponge ::
  forall dom n.
  ( HiddenClockResetEnable dom,
    KnownNat n,
    n ~ DivRU (MsgBits + 2) 576,
    MsgBits + 2 <= n * 576,
    MsgBits + 4 <= n * 576
  ) =>
  (Index 24 -> BitVector 1600 -> BitVector 1600) -> -- Permutation function
  Signal dom (AXI4Stream MsgBits, Bool, Bool) -> -- Input message, output tready, flush signal
  Signal dom (AXI4Stream DigestBits, Bool) -- Output digest (AXI4-Stream), input tready
sponge permModule = mealy step (State (Absorb 0) 0)
  where
    step ::
      State PadBeats (Index SqueezeBeats) ->
      (AXI4Stream MsgBits, Bool, Bool) ->
      (State PadBeats (Index SqueezeBeats), (AXI4Stream DigestBits, Bool))
    step (State (Absorb counter) state) (input, _tready, flush) = absorb pad XOR.staticXOR512 counter state input flush
    step (State (Permute counter seenTLAST) state) (_msg, tready, _flush) = permute permModule pad counter seenTLAST state tready
    step (State (Squeeze counter) state) (_msg, tready, _flush)
      | counter == maxBound =
          let outStream = AXI4Stream {tdata = squeezeSlice counter state, tvalid = True, tlast = True}
              nextState =
                if tready
                  then State (Absorb 0) 0
                  else State (Squeeze maxBound) state
           in (nextState, (outStream, False))
      | otherwise =
          let outStream = AXI4Stream {tdata = squeezeSlice counter state, tvalid = True, tlast = False}
              nextState =
                if tready
                  then State (Squeeze (counter + 1)) state
                  else State (Squeeze counter) state
           in (nextState, (outStream, False))
