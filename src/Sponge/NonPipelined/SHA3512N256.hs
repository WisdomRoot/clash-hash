{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Sponge.NonPipelined.SHA3512N256
  ( sponge,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import Parameter
import Sponge.NonPipelinedN256
import Sponge.XOR qualified as XOR
import TH (mkRead)

type PadBeats = 3

type SqueezeBeats = 2

-- | Padding function + XOR, flips 3 bits depending on the current beatCounter.
pad :: Index PadBeats -> BitVector 1600 -> BitVector 1600
pad 0 = complementAt 575 . complementAt 266 . complementAt 265 . complementAt 257
pad 1 = complementAt 575 . complementAt 514 . complementAt 513
pad _ = complementAt 575 . complementAt 2 . complementAt 1 -- special case for a whole 576-bit padding

-- | Squeeze phase bit slicing helper: extracts 256-bit chunks from the Keccak state.
$( mkRead
     "squeezeSlice"
     1600
     [ (0, 0, 256),
       (1, 256, 256)
     ]
 )

{-# INLINE squeezeSlice #-}

-- | Stateful sponge with AXI4-Stream backpressure support.
{-# OPAQUE sponge #-}
sponge ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  MLKEM ->
  (Index 24 -> BitVector 1600 -> BitVector 1600) -> -- Permutation function
  Signal dom (AXI4Stream 256, Bool, Bool) -> -- Input message, output tready, flush signal
  Signal dom (AXI4Stream 256, Bool) -- Output digest (AXI4-Stream), input tready
sponge mlkem permModule = mealy step (State (Absorb 0) 0)
  where
    step ::
      State PadBeats (Index SqueezeBeats) ->
      (AXI4Stream 256, Bool, Bool) ->
      (State PadBeats (Index SqueezeBeats), (AXI4Stream 256, Bool))
    step (State (Absorb counter) state) (input, _tready, flush) = absorb pad XOR.staticXOR512_256 counter state input flush
    step (State (Permute counter seenTLAST) state) (_msg, tready, _flush) = permute permModule pad (`squeezeSlice` 0) counter seenTLAST state tready
    step (State (Squeeze counter) state) (_msg, tready, _flush) = squeeze squeezeSlice counter state tready