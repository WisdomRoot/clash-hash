{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Component.G.Common
  ( PadBeats,
    SqueezeBeats,
    Phase (..),
    State (..),
    absorb32WithMLKEM,
    stepCore,
    core,
    squeezeSlice,
    sponge,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import Parameter
import Permutation qualified
import Sponge.NonPipelinedN256 qualified as Sponge
import Sponge.XOR qualified as XOR
import TH (mkRead)

type PadBeats = 3

type SqueezeBeats = 2

data Phase
  = Absorb
  | Permute (Index 24)
  | Squeeze (Index SqueezeBeats)
  deriving (Show, Eq, Generic, NFDataX)

data State = State Phase (BitVector 1600)
  deriving (Show, Eq, Generic, NFDataX)

kByte :: MLKEM -> BitVector 8
kByte MLKEM512 = 2
kByte MLKEM768 = 3
kByte MLKEM1024 = 4

absorb32WithMLKEM :: MLKEM -> BitVector 256 -> BitVector 1600
absorb32WithMLKEM mlkem msg256 =
  let msg264 = kByte mlkem ++# msg256
      placed = (0 :: BitVector 1336) ++# msg264
   in Sponge.complementAt 575 . Sponge.complementAt 266 . Sponge.complementAt 265 $ placed

-- | Padding function + XOR, flips 3 bits depending on the current beatCounter.
-- | k is encoded by which bit near the (k || 0x1F) byte is flipped in the first beat:
-- | k=2 -> flip bit 257; k=3 -> flip bit 256; k=4 -> flip bit 258.
pad512 :: Index PadBeats -> BitVector 1600 -> BitVector 1600
pad512 0 = Sponge.complementAt 575 . Sponge.complementAt 266 . Sponge.complementAt 265 . Sponge.complementAt 257
pad512 1 = Sponge.complementAt 575 . Sponge.complementAt 514 . Sponge.complementAt 513
pad512 _ = Sponge.complementAt 575 . Sponge.complementAt 2 . Sponge.complementAt 1 -- special case for a whole 576-bit padding

-- | Padding function + XOR for k = 3.
pad768 :: Index PadBeats -> BitVector 1600 -> BitVector 1600
pad768 0 = Sponge.complementAt 575 . Sponge.complementAt 266 . Sponge.complementAt 265 . Sponge.complementAt 257 . Sponge.complementAt 256
pad768 1 = Sponge.complementAt 575 . Sponge.complementAt 514 . Sponge.complementAt 513
pad768 _ = Sponge.complementAt 575 . Sponge.complementAt 2 . Sponge.complementAt 1 -- special case for a whole 576-bit padding

-- | Padding function + XOR for k = 4.
pad1024 :: Index PadBeats -> BitVector 1600 -> BitVector 1600
pad1024 0 = Sponge.complementAt 575 . Sponge.complementAt 266 . Sponge.complementAt 265 . Sponge.complementAt 258
pad1024 1 = Sponge.complementAt 575 . Sponge.complementAt 514 . Sponge.complementAt 513
pad1024 _ = Sponge.complementAt 575 . Sponge.complementAt 2 . Sponge.complementAt 1 -- special case for a whole 576-bit padding

-- | Squeeze phase bit slicing helper: extracts 256-bit chunks from the Keccak state.
$( mkRead
     "squeezeSlice"
     1600
     [ (0, 0, 256),
       (1, 256, 256)
     ]
 )

{-# INLINE squeezeSlice #-}

stepCore ::
  KnownNat n =>
  (BitVector n -> BitVector 1600) ->
  State ->
  (Bool, AXI4Stream n) ->
  (State, (Bool, AXI4Stream 256))
stepCore absorbFn (State phase state) (outReady, input) =
  case phase of
    Absorb ->
      if tvalid input
        then (State (Permute 0) (absorbFn (tdata input)), (False, idleAXI4Stream))
        else (State Absorb state, (True, idleAXI4Stream))
    Permute roundIdx ->
      let state' = Permutation.keccakF1600 roundIdx state
       in if roundIdx == maxBound
            then
              let out0 = validBeat (squeezeSlice state' 0) False
                  nextPhase = if outReady then Squeeze 1 else Squeeze 0
               in (State nextPhase state', (False, out0))
            else (State (Permute (roundIdx + 1)) state', (False, idleAXI4Stream))
    Squeeze idx ->
      let isLast = idx == maxBound
          outStream =
            AXI4Stream
              { tdata = squeezeSlice state idx,
                tvalid = True,
                tlast = isLast
              }
          nextState
            | not outReady = State (Squeeze idx) state
            | isLast = State Absorb 0
            | otherwise = State (Squeeze (idx + 1)) state
       in (nextState, (False, outStream))

core ::
  (HiddenClockResetEnable dom, KnownNat n) =>
  (BitVector n -> BitVector 1600) ->
  Pipe dom n 256
core absorbFn (outReady, inStream) =
  mealyB (stepCore absorbFn) (State Absorb 0) (outReady, inStream)

-- | Stateful sponge with AXI4-Stream backpressure support.
{-# OPAQUE sponge #-}
sponge ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  MLKEM ->
  (Index 24 -> BitVector 1600 -> BitVector 1600) -> -- Permutation function
  Signal dom (AXI4Stream 256, Bool, Bool) -> -- Input message, output tready, flush signal
  Signal dom (AXI4Stream 256, Bool) -- Output digest (AXI4-Stream), input tready
sponge mlkem permModule = mealy step (Sponge.State (Sponge.Absorb 0) 0)
  where
    padFn = case mlkem of
      MLKEM512 -> pad512
      MLKEM768 -> pad768
      MLKEM1024 -> pad1024
    step ::
      Sponge.State PadBeats (Index SqueezeBeats) ->
      (AXI4Stream 256, Bool, Bool) ->
      (Sponge.State PadBeats (Index SqueezeBeats), (AXI4Stream 256, Bool))
    step (Sponge.State (Sponge.Absorb counter) state) (input, _tready, flush) = Sponge.absorb padFn XOR.staticXOR512_256 counter state input flush
    step (Sponge.State (Sponge.Permute counter seenTLAST) state) (_msg, tready, _flush) = Sponge.permute permModule padFn (`squeezeSlice` 0) counter seenTLAST state tready
    step (Sponge.State (Sponge.Squeeze counter) state) (_msg, tready, _flush) = Sponge.squeeze squeezeSlice counter state tready
