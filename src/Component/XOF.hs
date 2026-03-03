{-# LANGUAGE TemplateHaskell #-}

module Component.XOF
  ( i272o48,
    topEntity,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import Permutation qualified
import Sponge.NonPipelined (complementAt)
import TH (mkRead)

type InputBits = 274
type OutputBits = 48

data Phase
  = Absorb
  | Permute (Index 24)
  | Squeeze (Index 28)
  deriving (Show, Eq, Generic, NFDataX)

data State = State Phase (BitVector 1600)
  deriving (Show, Eq, Generic, NFDataX)

$( mkRead
     "squeezeChunk48"
     1600
     [(i, i * 48, 48) | i <- [0 .. 27]]
 )

{-# INLINE squeezeChunk48 #-}

step ::
  State ->
  (Bool, AXI4Stream InputBits) ->
  (State, (Bool, AXI4Stream OutputBits))
step (State phase state) (tready, AXI4Stream inputMsg msgValid _) =
  case phase of
    Absorb ->
      if msgValid
        then (State (Permute 0) (absorbInput inputMsg), (False, idleAXI4Stream))
        else (State Absorb state, (True, idleAXI4Stream))
    Permute counter ->
      let state' = Permutation.keccakF1600 counter state
       in if counter == maxBound
            then (State (Squeeze 0) state', (False, idleAXI4Stream))
            else (State (Permute (counter + 1)) state', (False, idleAXI4Stream))
    Squeeze counter ->
      let chunk = squeezeChunk48 state counter
          outStream = validBeat chunk False
          nextState =
            if tready
              then
                if counter == maxBound
                  then State (Permute 0) state
                  else State (Squeeze (counter + 1)) state
              else State (Squeeze counter) state
       in (nextState, (False, outStream))

i272o48 ::
  HiddenClockResetEnable dom =>
  Stage dom InputBits OutputBits
i272o48 (outputReady, inputStream) =
  mealyB step (State Absorb 0) (outputReady, inputStream)

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream InputBits, Bool) ->
  Signal System (AXI4Stream OutputBits, Bool)
topEntity = stageTop i272o48

absorbInput :: BitVector InputBits -> BitVector 1600
absorbInput = padInput . placeMsg
  where
    placeMsg :: BitVector InputBits -> BitVector 1600
    placeMsg msg =
      let seed = slice (SNat @271) (SNat @0) msg
       in (0 :: BitVector 1328) ++# seed

    padInput :: BitVector 1600 -> BitVector 1600
    padInput =
      complementAt 1343
        . complementAt 272
        . complementAt 273
        . complementAt 274
        . complementAt 275
        . complementAt 276
