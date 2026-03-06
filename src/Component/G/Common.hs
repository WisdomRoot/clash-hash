{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Component.G.Common
  ( SqueezeBeats,
    Phase (..),
    State (..),
    absorb274WithKInBand,
    absorb32WithMLKEM,
    stepCore,
    core,
    squeezeSlice,
  )
where

import AXI4Stream
import Clash.Prelude hiding (tlast)
import Parameter
import Permutation qualified
import Sponge.NonPipelinedN256 (complementAt)
import TH (mkRead)

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

absorb274WithKInBand :: BitVector 274 -> BitVector 1600
absorb274WithKInBand msg274 =
  let msg264 = slice (SNat @263) (SNat @0) msg274
      placed = (0 :: BitVector 1336) ++# msg264
   in complementAt 575 . complementAt 266 . complementAt 265 $ placed

absorb32WithMLKEM :: MLKEM -> BitVector 256 -> BitVector 1600
absorb32WithMLKEM mlkem msg256 =
  let msg264 = kByte mlkem ++# msg256
      placed = (0 :: BitVector 1336) ++# msg264
   in complementAt 575 . complementAt 266 . complementAt 265 $ placed

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
