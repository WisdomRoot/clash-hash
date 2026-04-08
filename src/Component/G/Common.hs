{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Component.G.Common
  ( Phase (..),
    State (..),
    absorb272WithMLKEM,
    absorb32WithMLKEM,
    stepCore,
    core,
    stepCore512,
    core512,
    stepCore512X2,
    core512X2,
    stepCore512X3,
    core512X3,
    stepCore512X4,
    core512X4,
    stepCore512X6,
    core512X6,
    stepCore512X8,
    core512X8,
    squeezeSlice,
    squeezeSlice512,
  )
where

import AXI4Stream
import Clash.Prelude hiding (tlast)
import Parameter
import Permutation qualified
import Sponge.NonPipelinedN256 (complementAt)
import TH (mkRead)

data Phase
  = Absorb
  | Permute (Index 24)
  | Squeeze (Index 2)
  deriving (Show, Eq, Generic, NFDataX)

data State = State Phase (BitVector 1600)
  deriving (Show, Eq, Generic, NFDataX)

kByte :: MLKEM -> BitVector 8
kByte MLKEM512 = 2
kByte MLKEM768 = 3
kByte MLKEM1024 = 4

msg264FromMode :: Maybe MLKEM -> BitVector 272 -> BitVector 264
msg264FromMode Nothing msg272 = slice (SNat @263) (SNat @0) msg272
msg264FromMode (Just mlkem) msg272 =
  let msg256 = slice (SNat @255) (SNat @0) msg272
   in kByte mlkem ++# msg256

absorb272WithMLKEM :: Maybe MLKEM -> BitVector 272 -> BitVector 1600
absorb272WithMLKEM mode msg272 =
  let msg264 = msg264FromMode mode msg272
      placed = (0 :: BitVector 1336) ++# msg264
   in complementAt 575 . complementAt 266 . complementAt 265 $ placed

absorb32WithMLKEM :: Maybe MLKEM -> BitVector 256 -> BitVector 1600
absorb32WithMLKEM mlkem msg256 =
  absorb272WithMLKEM mlkem ((0 :: BitVector 16) ++# msg256)

-- | Squeeze phase bit slicing helper: extracts 256-bit chunks from the Keccak state.
$( mkRead
     "squeezeSlice"
     1600
     [ (0, 0, 256),
       (1, 256, 256)
     ]
 )

{-# INLINE squeezeSlice #-}

$(mkRead "squeezeSlice512" 1600 [(0, 0, 512)])

{-# INLINE squeezeSlice512 #-}

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

stepCore512 ::
  KnownNat n =>
  (BitVector n -> BitVector 1600) ->
  State ->
  (Bool, AXI4Stream n) ->
  (State, (Bool, AXI4Stream 512))
stepCore512 absorbFn (State phase state) (outReady, input) =
  case phase of
    Absorb ->
      if tvalid input
        then (State (Permute 0) (absorbFn (tdata input)), (False, idleAXI4Stream))
        else (State Absorb state, (True, idleAXI4Stream))
    Permute roundIdx ->
      let state' = Permutation.keccakF1600 roundIdx state
       in if roundIdx == maxBound
            then
              let outStream = validBeat (squeezeSlice512 state' 0) True
                  nextPhase = if outReady then Absorb else Squeeze 0
               in (State nextPhase state', (False, outStream))
            else (State (Permute (roundIdx + 1)) state', (False, idleAXI4Stream))
    Squeeze _ ->
      let outStream = validBeat (squeezeSlice512 state 0) True
          nextPhase = if outReady then Absorb else Squeeze 0
       in (State nextPhase state, (False, outStream))

core512 ::
  (HiddenClockResetEnable dom, KnownNat n) =>
  (BitVector n -> BitVector 1600) ->
  Pipe dom n 512
core512 absorbFn (outReady, inStream) =
  mealyB (stepCore512 absorbFn) (State Absorb 0) (outReady, inStream)

stepCore512X2 ::
  KnownNat n =>
  (BitVector n -> BitVector 1600) ->
  State ->
  (Bool, AXI4Stream n) ->
  (State, (Bool, AXI4Stream 512))
stepCore512X2 absorbFn (State phase state) (outReady, input) =
  case phase of
    Absorb ->
      if tvalid input
        then (State (Permute 0) (absorbFn (tdata input)), (False, idleAXI4Stream))
        else (State Absorb state, (True, idleAXI4Stream))
    Permute roundIdx ->
      let state' = Permutation.keccakF1600 roundIdx state
          state'' = Permutation.keccakF1600 (roundIdx + 1) state'
       in if roundIdx == 22
            then
              let outStream = validBeat (squeezeSlice512 state'' 0) True
                  nextPhase = if outReady then Absorb else Squeeze 0
               in (State nextPhase state'', (False, outStream))
            else (State (Permute (roundIdx + 2)) state'', (False, idleAXI4Stream))
    Squeeze _ ->
      let outStream = validBeat (squeezeSlice512 state 0) True
          nextPhase = if outReady then Absorb else Squeeze 0
       in (State nextPhase state, (False, outStream))

core512X2 ::
  (HiddenClockResetEnable dom, KnownNat n) =>
  (BitVector n -> BitVector 1600) ->
  Pipe dom n 512
core512X2 absorbFn (outReady, inStream) =
  mealyB (stepCore512X2 absorbFn) (State Absorb 0) (outReady, inStream)

stepCore512X3 ::
  KnownNat n =>
  (BitVector n -> BitVector 1600) ->
  State ->
  (Bool, AXI4Stream n) ->
  (State, (Bool, AXI4Stream 512))
stepCore512X3 absorbFn (State phase state) (outReady, input) =
  case phase of
    Absorb ->
      if tvalid input
        then (State (Permute 0) (absorbFn (tdata input)), (False, idleAXI4Stream))
        else (State Absorb state, (True, idleAXI4Stream))
    Permute roundIdx ->
      let state' = Permutation.keccakF1600 roundIdx state
          state'' = Permutation.keccakF1600 (roundIdx + 1) state'
          state''' = Permutation.keccakF1600 (roundIdx + 2) state''
       in if roundIdx == 21
            then
              let outStream = validBeat (squeezeSlice512 state''' 0) True
                  nextPhase = if outReady then Absorb else Squeeze 0
               in (State nextPhase state''', (False, outStream))
            else (State (Permute (roundIdx + 3)) state''', (False, idleAXI4Stream))
    Squeeze _ ->
      let outStream = validBeat (squeezeSlice512 state 0) True
          nextPhase = if outReady then Absorb else Squeeze 0
       in (State nextPhase state, (False, outStream))

core512X3 ::
  (HiddenClockResetEnable dom, KnownNat n) =>
  (BitVector n -> BitVector 1600) ->
  Pipe dom n 512
core512X3 absorbFn (outReady, inStream) =
  mealyB (stepCore512X3 absorbFn) (State Absorb 0) (outReady, inStream)

stepCore512X4 ::
  KnownNat n =>
  (BitVector n -> BitVector 1600) ->
  State ->
  (Bool, AXI4Stream n) ->
  (State, (Bool, AXI4Stream 512))
stepCore512X4 absorbFn (State phase state) (outReady, input) =
  case phase of
    Absorb ->
      if tvalid input
        then (State (Permute 0) (absorbFn (tdata input)), (False, idleAXI4Stream))
        else (State Absorb state, (True, idleAXI4Stream))
    Permute roundIdx ->
      let state' = Permutation.keccakF1600 roundIdx state
          state'' = Permutation.keccakF1600 (roundIdx + 1) state'
          state''' = Permutation.keccakF1600 (roundIdx + 2) state''
          state'''' = Permutation.keccakF1600 (roundIdx + 3) state'''
       in if roundIdx == 20
            then
              let outStream = validBeat (squeezeSlice512 state'''' 0) True
                  nextPhase = if outReady then Absorb else Squeeze 0
               in (State nextPhase state'''', (False, outStream))
            else (State (Permute (roundIdx + 4)) state'''', (False, idleAXI4Stream))
    Squeeze _ ->
      let outStream = validBeat (squeezeSlice512 state 0) True
          nextPhase = if outReady then Absorb else Squeeze 0
       in (State nextPhase state, (False, outStream))

core512X4 ::
  (HiddenClockResetEnable dom, KnownNat n) =>
  (BitVector n -> BitVector 1600) ->
  Pipe dom n 512
core512X4 absorbFn (outReady, inStream) =
  mealyB (stepCore512X4 absorbFn) (State Absorb 0) (outReady, inStream)

stepCore512X6 ::
  KnownNat n =>
  (BitVector n -> BitVector 1600) ->
  State ->
  (Bool, AXI4Stream n) ->
  (State, (Bool, AXI4Stream 512))
stepCore512X6 absorbFn (State phase state) (outReady, input) =
  case phase of
    Absorb ->
      if tvalid input
        then (State (Permute 0) (absorbFn (tdata input)), (False, idleAXI4Stream))
        else (State Absorb state, (True, idleAXI4Stream))
    Permute roundIdx ->
      let state1 = Permutation.keccakF1600 roundIdx state
          state2 = Permutation.keccakF1600 (roundIdx + 1) state1
          state3 = Permutation.keccakF1600 (roundIdx + 2) state2
          state4 = Permutation.keccakF1600 (roundIdx + 3) state3
          state5 = Permutation.keccakF1600 (roundIdx + 4) state4
          state6 = Permutation.keccakF1600 (roundIdx + 5) state5
       in if roundIdx == 18
            then
              let outStream = validBeat (squeezeSlice512 state6 0) True
                  nextPhase = if outReady then Absorb else Squeeze 0
               in (State nextPhase state6, (False, outStream))
            else (State (Permute (roundIdx + 6)) state6, (False, idleAXI4Stream))
    Squeeze _ ->
      let outStream = validBeat (squeezeSlice512 state 0) True
          nextPhase = if outReady then Absorb else Squeeze 0
       in (State nextPhase state, (False, outStream))

core512X6 ::
  (HiddenClockResetEnable dom, KnownNat n) =>
  (BitVector n -> BitVector 1600) ->
  Pipe dom n 512
core512X6 absorbFn (outReady, inStream) =
  mealyB (stepCore512X6 absorbFn) (State Absorb 0) (outReady, inStream)

stepCore512X8 ::
  KnownNat n =>
  (BitVector n -> BitVector 1600) ->
  State ->
  (Bool, AXI4Stream n) ->
  (State, (Bool, AXI4Stream 512))
stepCore512X8 absorbFn (State phase state) (outReady, input) =
  case phase of
    Absorb ->
      if tvalid input
        then (State (Permute 0) (absorbFn (tdata input)), (False, idleAXI4Stream))
        else (State Absorb state, (True, idleAXI4Stream))
    Permute roundIdx ->
      let state1 = Permutation.keccakF1600 roundIdx state
          state2 = Permutation.keccakF1600 (roundIdx + 1) state1
          state3 = Permutation.keccakF1600 (roundIdx + 2) state2
          state4 = Permutation.keccakF1600 (roundIdx + 3) state3
          state5 = Permutation.keccakF1600 (roundIdx + 4) state4
          state6 = Permutation.keccakF1600 (roundIdx + 5) state5
          state7 = Permutation.keccakF1600 (roundIdx + 6) state6
          state8 = Permutation.keccakF1600 (roundIdx + 7) state7
       in if roundIdx == 16
            then
              let outStream = validBeat (squeezeSlice512 state8 0) True
                  nextPhase = if outReady then Absorb else Squeeze 0
               in (State nextPhase state8, (False, outStream))
            else (State (Permute (roundIdx + 8)) state8, (False, idleAXI4Stream))
    Squeeze _ ->
      let outStream = validBeat (squeezeSlice512 state 0) True
          nextPhase = if outReady then Absorb else Squeeze 0
       in (State nextPhase state, (False, outStream))

core512X8 ::
  (HiddenClockResetEnable dom, KnownNat n) =>
  (BitVector n -> BitVector 1600) ->
  Pipe dom n 512
core512X8 absorbFn (outReady, inStream) =
  mealyB (stepCore512X8 absorbFn) (State Absorb 0) (outReady, inStream)
