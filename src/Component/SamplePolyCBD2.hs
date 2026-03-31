{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Component.SamplePolyCBD2
  ( i264o12,
    i264o12Core,
    i264o24,
    i264o24Core,
  )
where

import AXI4Stream
import Clash.Prelude hiding (tlast)
import Component.SamplePolyCBD.Common (absorb33, cbd2)
import Permutation qualified
import TH (mkRead)

-- | Extract 4-bit chunks for eta=2 path.
$(mkRead "read4Block0" 1600 [(i, i * 4, 4) | i <- [0 .. 255]])

data State
  = Absorb
  | Eta2Permute (Index 24) (Index 256) (BitVector 1600)
  | Eta2Squeeze (Index 256) (BitVector 1600)
  | Done
  deriving (Show, Eq, Generic, NFDataX)

data State24
  = Absorb24
  | Eta2Permute24 (Index 24) (Index 128) (BitVector 1600)
  | Eta2Squeeze24 (Index 128) (BitVector 1600)
  | Done24
  deriving (Show, Eq, Generic, NFDataX)

stepI264o12 ::
  State ->
  (Bool, AXI4Stream 264) ->
  (State, (Bool, AXI4Stream 12))
stepI264o12 st (outReady, inStream) =
  case st of
    Absorb ->
      if tvalid inStream
        then
          let initState = absorb33 (tdata inStream)
           in (Eta2Permute 0 0 initState, (False, idleAXI4Stream))
        else (Absorb, (True, idleAXI4Stream))
    Eta2Permute roundIdx coeffIdx state ->
      let state' = Permutation.keccakF1600 roundIdx state
       in if roundIdx == maxBound
            then (Eta2Squeeze coeffIdx state', (False, idleAXI4Stream))
            else (Eta2Permute (roundIdx + 1) coeffIdx state', (False, idleAXI4Stream))
    Eta2Squeeze coeffIdx state ->
      let bits4 = read4Block0 state coeffIdx
          coeff = cbd2 bits4
          isLast = coeffIdx == maxBound
          outStream = validBeat coeff isLast
          nextState
            | outReady && isLast = Done
            | outReady = Eta2Squeeze (coeffIdx + 1) state
            | otherwise = Eta2Squeeze coeffIdx state
       in (nextState, (False, outStream))
    Done -> (Done, (False, idleAXI4Stream))

stepI264o24 ::
  State24 ->
  (Bool, AXI4Stream 264) ->
  (State24, (Bool, AXI4Stream 24))
stepI264o24 st (outReady, inStream) =
  case st of
    Absorb24 ->
      if tvalid inStream
        then
          let initState = absorb33 (tdata inStream)
           in (Eta2Permute24 0 0 initState, (False, idleAXI4Stream))
        else (Absorb24, (True, idleAXI4Stream))
    Eta2Permute24 roundIdx pairIdx state ->
      let state' = Permutation.keccakF1600 roundIdx state
       in if roundIdx == maxBound
            then (Eta2Squeeze24 pairIdx state', (False, idleAXI4Stream))
            else (Eta2Permute24 (roundIdx + 1) pairIdx state', (False, idleAXI4Stream))
    Eta2Squeeze24 pairIdx state ->
      let pairIdxU = fromIntegral pairIdx :: Unsigned 8
          idx0 = fromIntegral (pairIdxU * 2) :: Index 256
          idx1 = fromIntegral (pairIdxU * 2 + 1) :: Index 256
          coeff0 = cbd2 (read4Block0 state idx0)
          coeff1 = cbd2 (read4Block0 state idx1)
          isLast = pairIdx == maxBound
          outStream = validBeat (coeff1 ++# coeff0) isLast
          nextState
            | outReady && isLast = Done24
            | outReady = Eta2Squeeze24 (pairIdx + 1) state
            | otherwise = Eta2Squeeze24 pairIdx state
       in (nextState, (False, outStream))
    Done24 -> (Done24, (False, idleAXI4Stream))

i264o12Core ::
  HiddenClockResetEnable dom =>
  Pipe dom 264 12
i264o12Core (outReady, inStream) =
  mealyB stepI264o12 Absorb (outReady, inStream)

i264o24Core ::
  HiddenClockResetEnable dom =>
  Pipe dom 264 24
i264o24Core (outReady, inStream) =
  mealyB stepI264o24 Absorb24 (outReady, inStream)

{-# ANN
  i264o12
  ( Synthesize
      { t_name = "dut",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortProduct
              ""
              [ PortProduct "MSG" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
                PortName "DIGEST_TREADY"
              ]
          ],
        t_output =
          PortProduct
            ""
            [ PortProduct "DIGEST" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
              PortName "MSG_TREADY"
            ]
      }
  )
  #-}
{-# NOINLINE i264o12 #-}
i264o12 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 264, Bool) ->
  Signal System (AXI4Stream 12, Bool)
i264o12 = toDUT i264o12Core

{-# ANN
  i264o24
  ( Synthesize
      { t_name = "dut",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortProduct
              ""
              [ PortProduct "MSG" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
                PortName "DIGEST_TREADY"
              ]
          ],
        t_output =
          PortProduct
            ""
            [ PortProduct "DIGEST" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
              PortName "MSG_TREADY"
            ]
      }
  )
  #-}
{-# NOINLINE i264o24 #-}
i264o24 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 264, Bool) ->
  Signal System (AXI4Stream 24, Bool)
i264o24 = toDUT i264o24Core
