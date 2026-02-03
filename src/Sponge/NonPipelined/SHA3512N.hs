{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Sponge.NonPipelined.SHA3512N
  ( sponge,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import TH (mkRead)
import Sponge.NonPipelinedN
import Sponge.XOR qualified as XOR

type PadBeats = 9

type SqueezeBeats = 8

-- | Padding function + XOR, flips 3 bits depending on the current beatCounter.
pad :: Index PadBeats -> BitVector 1600 -> BitVector 1600
pad 0 = complementAt 575 . complementAt 66 . complementAt 65
pad 1 = complementAt 575 . complementAt 130 . complementAt 129
pad 2 = complementAt 575 . complementAt 194 . complementAt 193
pad 3 = complementAt 575 . complementAt 258 . complementAt 257
pad 4 = complementAt 575 . complementAt 322 . complementAt 321
pad 5 = complementAt 575 . complementAt 386 . complementAt 385
pad 6 = complementAt 575 . complementAt 450 . complementAt 449
pad 7 = complementAt 575 . complementAt 514 . complementAt 513
pad _ = complementAt 575 . complementAt 2 . complementAt 1 -- special case for a whole 576-bit padding

-- | Squeeze phase bit slicing helper: extracts 64-bit chunks from the Keccak state.
$(mkRead "squeezeSlice" 1600
    [ (0, 0, 64), (1, 64, 64), (2, 128, 64), (3, 192, 64)
    , (4, 256, 64), (5, 320, 64), (6, 384, 64), (7, 448, 64)
    ])

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
    step (State (Absorb counter) state) (input, _tready, flush) = absorb pad XOR.staticXOR512' counter state input flush
    step (State (Permute counter seenTLAST) state) (_msg, tready, _flush) = permute permModule pad counter seenTLAST state tready
    step (State (Squeeze counter) state) (_msg, tready, _flush)
      | counter == maxBound =
          let outStream = AXI4Stream {tdata = squeezeSlice state counter, tvalid = True, tlast = True}
              nextState =
                if tready
                  then State (Absorb 0) 0
                  else State (Squeeze maxBound) state
           in (nextState, (outStream, False))
      | otherwise =
          let outStream = AXI4Stream {tdata = squeezeSlice state counter, tvalid = True, tlast = False}
              nextState =
                if tready
                  then State (Squeeze (counter + 1)) state
                  else State (Squeeze counter) state
           in (nextState, (outStream, False))
