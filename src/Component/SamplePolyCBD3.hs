{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Component.SamplePolyCBD3
  ( i264o12,
    i264o12Core,
    i264o24,
    i264o24Core,
  )
where

import AXI4Stream
import Clash.Prelude hiding (tlast)
import Component.SamplePolyCBD.Common (absorb33, cbd3)
import Permutation qualified
import TH (mkRead)

-- | Extract 6-bit chunks for eta=3 path, first block.
$(mkRead "read6Block0" 1600 [(i, i * 6, 6) | i <- [0 .. 180]])

-- | Extract 6-bit chunks for eta=3 path, second block.
$(mkRead "read6Block1" 1600 [(i, 4 + i * 6, 6) | i <- [0 .. 73]])

data Eta3Phase
  = Eta3FirstBlock
  | Eta3SecondBlock (BitVector 2)
  deriving (Show, Eq, Generic, NFDataX)

data State
  = Absorb
  | Eta3Permute (Index 24) (Index 256) Eta3Phase (BitVector 1600)
  | Eta3Squeeze (Index 256) Eta3Phase (BitVector 1600)
  | Done
  deriving (Show, Eq, Generic, NFDataX)

data Eta3PairPhase
  = Eta3PairFirstBlock
  | Eta3PairSecondBlock (BitVector 8)
  deriving (Show, Eq, Generic, NFDataX)

data State24
  = Absorb24
  | Eta3Permute24 (Index 24) (Index 128) Eta3PairPhase (BitVector 1600)
  | Eta3Squeeze24 (Index 128) Eta3PairPhase (BitVector 1600)
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
           in (Eta3Permute 0 0 Eta3FirstBlock initState, (False, idleAXI4Stream))
        else (Absorb, (True, idleAXI4Stream))
    Eta3Permute roundIdx coeffIdx phase state ->
      let state' = Permutation.keccakF1600 roundIdx state
       in if roundIdx == maxBound
            then (Eta3Squeeze coeffIdx phase state', (False, idleAXI4Stream))
            else (Eta3Permute (roundIdx + 1) coeffIdx phase state', (False, idleAXI4Stream))
    Eta3Squeeze coeffIdx phase block ->
      case phase of
        Eta3FirstBlock ->
          if coeffIdx >= 181
            then
              let tail2 = slice (SNat @1087) (SNat @1086) block
               in (Eta3Permute 0 coeffIdx (Eta3SecondBlock tail2) block, (False, idleAXI4Stream))
            else
              let bits6 = read6Block0 block (fromIntegral coeffIdx)
                  coeff = cbd3 bits6
                  outStream = validBeat coeff False
                  nextState
                    | outReady = Eta3Squeeze (coeffIdx + 1) Eta3FirstBlock block
                    | otherwise = Eta3Squeeze coeffIdx Eta3FirstBlock block
               in (nextState, (False, outStream))
        Eta3SecondBlock tail2 ->
          let bits6 =
                if coeffIdx == 181
                  then
                    let head4 = slice d3 d0 block
                     in head4 ++# tail2
                  else read6Block1 block (fromIntegral (coeffIdx - 182))
              coeff = cbd3 bits6
              isLast = coeffIdx == maxBound
              outStream = validBeat coeff isLast
              nextState
                | outReady && isLast = Done
                | outReady = Eta3Squeeze (coeffIdx + 1) (Eta3SecondBlock tail2) block
                | otherwise = Eta3Squeeze coeffIdx (Eta3SecondBlock tail2) block
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
           in (Eta3Permute24 0 0 Eta3PairFirstBlock initState, (False, idleAXI4Stream))
        else (Absorb24, (True, idleAXI4Stream))
    Eta3Permute24 roundIdx pairIdx phase state ->
      let state' = Permutation.keccakF1600 roundIdx state
       in if roundIdx == maxBound
            then (Eta3Squeeze24 pairIdx phase state', (False, idleAXI4Stream))
            else (Eta3Permute24 (roundIdx + 1) pairIdx phase state', (False, idleAXI4Stream))
    Eta3Squeeze24 pairIdx phase block ->
      case phase of
        Eta3PairFirstBlock ->
          if pairIdx >= 90
            then
              let bits8 = slice (SNat @1087) (SNat @1080) block
               in (Eta3Permute24 0 pairIdx (Eta3PairSecondBlock bits8) block, (False, idleAXI4Stream))
            else
              let pairIdxU = fromIntegral pairIdx :: Unsigned 8
                  idx0 = fromIntegral (pairIdxU * 2) :: Index 181
                  idx1 = fromIntegral (pairIdxU * 2 + 1) :: Index 181
                  coeff0 = cbd3 (read6Block0 block idx0)
                  coeff1 = cbd3 (read6Block0 block idx1)
                  outStream = validBeat (coeff1 ++# coeff0) False
                  nextState
                    | outReady = Eta3Squeeze24 (pairIdx + 1) Eta3PairFirstBlock block
                    | otherwise = Eta3Squeeze24 pairIdx Eta3PairFirstBlock block
               in (nextState, (False, outStream))
        Eta3PairSecondBlock bits8 ->
          let (outData, isLast) =
                if pairIdx == 90
                  then
                    let tail2 = slice d7 d6 bits8
                        coeff180Bits = slice d5 d0 bits8
                        coeff180 = cbd3 coeff180Bits
                        head4 = slice d3 d0 block
                        coeff181 = cbd3 (head4 ++# tail2)
                     in (coeff181 ++# coeff180, False)
                  else
                    let pairIdxU = fromIntegral pairIdx :: Unsigned 8
                        coeff0U = pairIdxU * 2
                        coeff1U = coeff0U + 1
                        idx0 = fromIntegral (coeff0U - 182) :: Index 74
                        idx1 = fromIntegral (coeff1U - 182) :: Index 74
                        coeff0 = cbd3 (read6Block1 block idx0)
                        coeff1 = cbd3 (read6Block1 block idx1)
                        lastPair = pairIdx == maxBound
                     in (coeff1 ++# coeff0, lastPair)
              outStream = validBeat outData isLast
              nextState
                | outReady && isLast = Done24
                | outReady = Eta3Squeeze24 (pairIdx + 1) (Eta3PairSecondBlock bits8) block
                | otherwise = Eta3Squeeze24 pairIdx (Eta3PairSecondBlock bits8) block
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
