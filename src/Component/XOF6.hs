{-# LANGUAGE TemplateHaskell #-}

module Component.XOF6
  ( Phase (..),
    State (..),
    step,
    i272o72,
    i272o72Core,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import Permutation qualified
import Sponge.NonPipelined (complementAt)
import TH (mkRead)

type InputBits = 272
type OutputBits = 72

data Phase
  = Absorb
  | Permute0 (Index 24)
  | Permute24 (BitVector 24) (Index 24)
  | Permute48 (BitVector 48) (Index 24)
  | Squeeze0 (Index 18)
  | Squeeze24First (BitVector 24)
  | Squeeze24Rest (Index 18)
  | Squeeze48First (BitVector 48)
  | Squeeze48Rest (Index 18)
  deriving (Show, Eq, Generic, NFDataX)

data State = State Phase (BitVector 1600)
  deriving (Show, Eq, Generic, NFDataX)

$(mkRead "squeeze72Offset0" 1600 [(i, i * 72, 72) | i <- [0 .. 17]])
$(mkRead "squeeze72Offset24" 1600 [(i, 24 + (i * 72), 72) | i <- [0 .. 17]])
$(mkRead "squeeze72Offset48" 1600 [(i, 48 + (i * 72), 72) | i <- [0 .. 17]])

step ::
  State ->
  (Bool, AXI4Stream InputBits) ->
  (State, (Bool, AXI4Stream OutputBits))
step (State phase state) (tready, AXI4Stream inputMsg msgValid _) =
  case phase of
    Absorb ->
      if msgValid
        then (State (Permute0 0) (absorbInput inputMsg), (False, idleAXI4Stream))
        else (State Absorb state, (True, idleAXI4Stream))
    Permute0 counter ->
      stepPermute (State (Squeeze0 0)) counter
    Permute24 carry24 counter ->
      stepPermute (State (Squeeze24First carry24)) counter
    Permute48 carry48 counter ->
      stepPermute (State (Squeeze48First carry48)) counter
    Squeeze0 counter ->
      let chunk = squeeze72Offset0 state counter
          nextPhase =
            if counter == maxBound
              then
                let carry48 = slice (SNat @1343) (SNat @1296) state
                 in State (Permute48 carry48 0) state
              else State (Squeeze0 (counter + 1)) state
       in stepSqueeze chunk nextPhase
    Squeeze24First carry24 ->
      let chunk = slice d47 d0 state ++# carry24
       in stepSqueeze chunk (State (Squeeze48Rest 0) state)
    Squeeze24Rest counter ->
      let chunk = squeeze72Offset24 state counter
          nextPhase =
            if counter == maxBound
              then
                let carry24 = slice (SNat @1343) (SNat @1320) state
                 in State (Permute24 carry24 0) state
              else State (Squeeze24Rest (counter + 1)) state
       in stepSqueeze chunk nextPhase
    Squeeze48First carry48 ->
      let chunk = slice d23 d0 state ++# carry48
       in stepSqueeze chunk (State (Squeeze24Rest 0) state)
    Squeeze48Rest counter ->
      let chunk = squeeze72Offset48 state counter
          nextPhase =
            if counter == maxBound
              then State (Permute0 0) state
              else State (Squeeze48Rest (counter + 1)) state
       in stepSqueeze chunk nextPhase
  where
    stepPermute mkNext counter =
      let state' = Permutation.keccakF1600 counter state
       in if counter == maxBound
            then (mkNext state', (False, idleAXI4Stream))
            else (State (advancePermute phase (counter + 1)) state', (False, idleAXI4Stream))

    advancePermute current nextCounter = case current of
      Permute0 _ -> Permute0 nextCounter
      Permute24 carry24 _ -> Permute24 carry24 nextCounter
      Permute48 carry48 _ -> Permute48 carry48 nextCounter
      _ -> error "Component.XOF6.advancePermute: invalid phase"

    stepSqueeze chunk nextState =
      let outStream = validBeat chunk False
       in chunk `deepseqX` if tready
            then (nextState, (False, outStream))
            else (State phase state, (False, outStream))
{-# INLINE step #-}

i272o72Core ::
  Pipe2 dom InputBits OutputBits
i272o72Core =
  Pipe2 step (State Absorb 0)
{-# INLINE i272o72Core #-}

{-# ANN
  i272o72
  ( Synthesize
      { t_name = "dut",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortProduct
              ""
              [ PortProduct "MSG" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
                PortName "XOF_TREADY"
              ]
          ],
        t_output =
          PortProduct
            ""
            [ PortProduct "XOF" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
              PortName "MSG_TREADY"
            ]
      }
  )
  #-}
{-# NOINLINE i272o72 #-}
i272o72 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream InputBits, Bool) ->
  Signal System (AXI4Stream OutputBits, Bool)
i272o72 = toDUT2 i272o72Core

absorbInput :: BitVector InputBits -> BitVector 1600
absorbInput = padInput . placeMsg
  where
    placeMsg :: BitVector InputBits -> BitVector 1600
    placeMsg msg = (0 :: BitVector 1328) ++# msg

    padInput :: BitVector 1600 -> BitVector 1600
    padInput =
      complementAt 1343
        . complementAt 272
        . complementAt 273
        . complementAt 274
        . complementAt 275
        . complementAt 276
