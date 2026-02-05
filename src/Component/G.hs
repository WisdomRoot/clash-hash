{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Component.G
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
-- | k is encoded by which bit near the (k || 0x1F) byte is flipped in the first beat:
-- | k=2 -> flip bit 257; k=3 -> flip bit 256; k=4 -> flip bit 258.
pad512 :: Index PadBeats -> BitVector 1600 -> BitVector 1600
pad512 0 = complementAt 575 . complementAt 266 . complementAt 265 . complementAt 257
pad512 1 = complementAt 575 . complementAt 514 . complementAt 513
pad512 _ = complementAt 575 . complementAt 2 . complementAt 1 -- special case for a whole 576-bit padding

-- | Padding function + XOR for k = 3.
pad768 :: Index PadBeats -> BitVector 1600 -> BitVector 1600
pad768 0 = complementAt 575 . complementAt 266 . complementAt 265 . complementAt 257 . complementAt 256
pad768 1 = complementAt 575 . complementAt 514 . complementAt 513
pad768 _ = complementAt 575 . complementAt 2 . complementAt 1 -- special case for a whole 576-bit padding

-- | Padding function + XOR for k = 4.
pad1024 :: Index PadBeats -> BitVector 1600 -> BitVector 1600
pad1024 0 = complementAt 575 . complementAt 266 . complementAt 265 . complementAt 258
pad1024 1 = complementAt 575 . complementAt 514 . complementAt 513
pad1024 _ = complementAt 575 . complementAt 2 . complementAt 1 -- special case for a whole 576-bit padding

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
    padFn = case mlkem of
      MLKEM512 -> pad512
      MLKEM768 -> pad768
      MLKEM1024 -> pad1024
    step ::
      State PadBeats (Index SqueezeBeats) ->
      (AXI4Stream 256, Bool, Bool) ->
      (State PadBeats (Index SqueezeBeats), (AXI4Stream 256, Bool))
    step (State (Absorb counter) state) (input, _tready, flush) = absorb padFn XOR.staticXOR512_256 counter state input flush
    step (State (Permute counter seenTLAST) state) (_msg, tready, _flush) = permute permModule padFn (`squeezeSlice` 0) counter seenTLAST state tready
    step (State (Squeeze counter) state) (_msg, tready, _flush) = squeeze squeezeSlice counter state tready
