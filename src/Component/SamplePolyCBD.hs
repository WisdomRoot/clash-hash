{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Component.SamplePolyCBD
  ( i272o12,
    i272o24,
    i272o12Core,
    i272o24Core
  )
where

import AXI4Stream
import Clash.Prelude hiding (tlast)
import Component.SamplePolyCBD.Common (absorb33, cbd2, cbd3)
import Permutation qualified
import TH (mkRead)

-- | Extract 4-bit chunks for eta=2 path.
$(mkRead "read4Block0" 1600 [(i, i * 4, 4) | i <- [0 .. 255]])

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
  | Eta2Permute (Index 24) (Index 256) (BitVector 1600)
  | Eta2Squeeze (Index 256) (BitVector 1600)
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
  | Eta2Permute24 (Index 24) (Index 128) (BitVector 1600)
  | Eta2Squeeze24 (Index 128) (BitVector 1600)
  | Eta3Permute24 (Index 24) (Index 128) Eta3PairPhase (BitVector 1600)
  | Eta3Squeeze24 (Index 128) Eta3PairPhase (BitVector 1600)
  | Done24
  deriving (Show, Eq, Generic, NFDataX)

isEta3 :: BitVector 8 -> Bool
isEta3 etaByte = etaByte == 3

stepI272o12 ::
  State ->
  (Bool, AXI4Stream 272) ->
  (State, (Bool, AXI4Stream 12))
stepI272o12 st (outReady, inStream) =
  case st of
    Absorb ->
      if tvalid inStream
        then
          let msg272 = tdata inStream
              etaByte = slice (SNat @271) (SNat @264) msg272
              msg264 = slice (SNat @263) (SNat @0) msg272
              initState = absorb33 msg264
           in if isEta3 etaByte
                then (Eta3Permute 0 0 Eta3FirstBlock initState, (False, idleAXI4Stream))
                else (Eta2Permute 0 0 initState, (False, idleAXI4Stream))
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

i272o12Core ::
  HiddenClockResetEnable dom =>
  Pipe dom 272 12
i272o12Core (outReady, inStream) =
  mealyB stepI272o12 Absorb (outReady, inStream)

i272o24Core ::
  HiddenClockResetEnable dom =>
  Pipe dom 272 24
i272o24Core (outReady, inStream) =
  mealyB stepI272o24 Absorb24 (outReady, inStream)

stepI272o24 ::
  State24 ->
  (Bool, AXI4Stream 272) ->
  (State24, (Bool, AXI4Stream 24))
stepI272o24 st (outReady, inStream) =
  case st of
    Absorb24 ->
      if tvalid inStream
        then
          let msg272 = tdata inStream
              etaByte = slice (SNat @271) (SNat @264) msg272
              msg264 = slice (SNat @263) (SNat @0) msg272
              initState = absorb33 msg264
           in if isEta3 etaByte
                then (Eta3Permute24 0 0 Eta3PairFirstBlock initState, (False, idleAXI4Stream))
                else (Eta2Permute24 0 0 initState, (False, idleAXI4Stream))
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

{-# ANN
  i272o12
  ( Synthesize
      { t_name = "dut",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortProduct
              ""
              [ PortProduct "MSG" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
                PortName "COEFF_TREADY"
              ]
          ],
        t_output =
          PortProduct
            ""
            [ PortProduct "COEFF" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
              PortName "MSG_TREADY"
            ]
      }
  )
  #-}
{-# NOINLINE i272o12 #-}
i272o12 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 12, Bool)
i272o12 = toDUT i272o12Core

{-# ANN
  i272o24
  ( Synthesize
      { t_name = "dut",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortProduct
              ""
              [ PortProduct "MSG" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
                PortName "COEFF_TREADY"
              ]
          ],
        t_output =
          PortProduct
            ""
            [ PortProduct "COEFF" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
              PortName "MSG_TREADY"
            ]
      }
  )
  #-}
{-# NOINLINE i272o24 #-}
i272o24 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 24, Bool)
i272o24 = toDUT i272o24Core
