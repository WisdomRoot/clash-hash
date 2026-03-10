{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Component.SamplePolyCBD
  ( i272o12
  )
where

import AXI4Stream
import Clash.Prelude hiding (tlast)
import Component.SamplePolyCBD.Common
  ( cbd3
  )
import Permutation qualified
import Sponge.NonPipelined (complementAt)
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

isEta3 :: BitVector 8 -> Bool
isEta3 etaByte = etaByte == 3

cbd2Ref :: BitVector 4 -> BitVector 12
cbd2Ref bits =
  let b0 = resize (unpack (slice d0 d0 bits) :: Unsigned 1) :: Unsigned 2
      b1 = resize (unpack (slice d1 d1 bits) :: Unsigned 1) :: Unsigned 2
      b2 = resize (unpack (slice d2 d2 bits) :: Unsigned 1) :: Unsigned 2
      b3 = resize (unpack (slice d3 d3 bits) :: Unsigned 1) :: Unsigned 2
      a = b0 + b1
      b = b2 + b3
   in if a >= b
        then resize (pack (a - b))
        else 3329 - resize (pack (b - a))

absorb33Normal :: BitVector 264 -> BitVector 1600
absorb33Normal = pad33Bytes . placeMsg
  where
    placeMsg msg = (0 :: BitVector 1336) ++# msg
    pad33Bytes =
      complementAt 1087
        . complementAt 264
        . complementAt 265
        . complementAt 266
        . complementAt 267
        . complementAt 268

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
           in if isEta3 etaByte
                then
                  let initState = absorb33Normal msg264
                   in (Eta3Permute 0 0 Eta3FirstBlock initState, (False, idleAXI4Stream))
                else
                  let initState = absorb33Normal msg264
                   in (Eta2Permute 0 0 initState, (False, idleAXI4Stream))
        else (Absorb, (True, idleAXI4Stream))
    Eta2Permute roundIdx coeffIdx state ->
      let state' = Permutation.keccakF1600 roundIdx state
       in if roundIdx == maxBound
            then (Eta2Squeeze coeffIdx state', (False, idleAXI4Stream))
            else (Eta2Permute (roundIdx + 1) coeffIdx state', (False, idleAXI4Stream))
    Eta2Squeeze coeffIdx state ->
      let bits4 = read4Block0 state coeffIdx
          coeff = cbd2Ref bits4
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
