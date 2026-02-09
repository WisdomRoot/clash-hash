{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Component.SamplePolyCBD512
  ( i264o12,
  )
where

import AXI4Stream
import Clash.Prelude hiding (tlast)
import Component.SamplePolyCBD.Common (cbd3)
import Permutation qualified
import Sponge.NonPipelined (complementAt)
import TH (mkRead)

type MsgBits = 33 * 8

-- | Extract 6-bit chunks from the first sponge block.
$(mkRead "read6Block0" 1600 [(i, i * 6, 6) | i <- [0 .. 180]])

-- | Extract 6-bit chunks from the second sponge block starting at bit 4.
$(mkRead "read6Block1" 1600 [(i, 4 + i * 6, 6) | i <- [0 .. 73]])

{-# ANN
  i264o12
  ( Synthesize
      { t_name = "Component_SamplePolyCBD512",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "MSG_33B",
            PortName "DIGEST_TREADY"
          ],
        t_output =
          PortProduct
            ""
            [ PortName "DIGEST_TDATA",
              PortName "DIGEST_TVALID",
              PortName "DIGEST_TLAST"
            ]
      }
  )
  #-}
{-# NOINLINE i264o12 #-}
i264o12 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector MsgBits) ->
  Signal System Bool ->
  Signal System (AXI4Stream 12, Bool)
i264o12 clk rst en msgSig treadySig =
  withClockResetEnable clk rst en (samplePolyCBD512 msgSig treadySig)

data Permutation = FirstBlock | SecondBlock (BitVector 2) -- the final 2 bits of digest from the first block
  deriving (Show, Eq, Generic, NFDataX)

-- | State machine for SamplePolyCBD512 (eta=3).
--     256 coefficients => 256 * 6 bit of digest = 2 permutation
data State
  = Absorb
  | Permute
      (Index 24) -- round index
      (Index 256) -- coefficient count (0-256)
      (BitVector 1600) -- state being permuted
      Permutation -- which block we are permuting
  | Squeeze
      (Index 256) -- coefficient count (0-256)
      (BitVector 1600) -- current block
      Permutation -- which block we are squeezing
  | Done
  deriving (Show, Eq, Generic, NFDataX)

samplePolyCBD512 ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  Signal dom (BitVector MsgBits) ->
  Signal dom Bool ->
  Signal dom (AXI4Stream 12, Bool)
samplePolyCBD512 msgSig treadySig = mealy step Absorb (bundle (msgSig, treadySig))
  where
    step ::
      State ->
      (BitVector MsgBits, Bool) ->
      (State, (AXI4Stream 12, Bool))
    step st (msg, tready) =
      case st of
        Absorb ->
          let initState = absorb33Normal msg
           in (Permute 0 0 initState FirstBlock, (idleAXI4Stream, True))
        Permute roundIdx coeffIdx state whichBlock ->
          let state' = Permutation.keccakF1600 roundIdx state
           in if roundIdx == maxBound
                then (Squeeze coeffIdx state' whichBlock, (idleAXI4Stream, False))
                else
                  (Permute (roundIdx + 1) coeffIdx state' whichBlock, (idleAXI4Stream, False))
        Squeeze coeffIdx block whichBlock ->
          case whichBlock of
            FirstBlock ->
              if coeffIdx >= 181
                then
                  let tail2 = slice (SNat @1087) (SNat @1086) block
                   in (Permute 0 coeffIdx block (SecondBlock tail2), (idleAXI4Stream, False))
                else
                  let bits6 = read6Block0 block (fromIntegral coeffIdx)
                      coeffVal = cbd3 bits6
                      outStream =
                        AXI4Stream
                          { tdata = coeffVal,
                            tvalid = True,
                            tlast = False
                          }
                      nextState
                        | tready = Squeeze (coeffIdx + 1) block FirstBlock
                        | otherwise = Squeeze coeffIdx block FirstBlock
                   in (nextState, (outStream, False))
            SecondBlock tail2 ->
              let isLast = coeffIdx == 255
                  bits6 =
                    if coeffIdx == 181
                      then
                        let head4 = slice d3 d0 block
                         in head4 ++# tail2
                      else read6Block1 block (fromIntegral (coeffIdx - 182))
                  coeffVal = cbd3 bits6
                  outStream =
                    AXI4Stream
                      { tdata = coeffVal,
                        tvalid = True,
                        tlast = isLast
                      }
                  nextState
                    | tready && isLast = Done
                    | tready = Squeeze (coeffIdx + 1) block (SecondBlock tail2)
                    | otherwise = Squeeze coeffIdx block (SecondBlock tail2)
               in (nextState, (outStream, False))
        Done -> (Done, (idleAXI4Stream, False))

-- | Absorb 33 bytes: place message and apply SHAKE256 padding (normal order).
absorb33Normal :: BitVector MsgBits -> BitVector 1600
absorb33Normal = pad33Bytes . placeMsg
  where
    placeMsg :: BitVector MsgBits -> BitVector 1600
    placeMsg msg = (0 :: BitVector 1336) ++# msg

    pad33Bytes :: BitVector 1600 -> BitVector 1600
    pad33Bytes =
      complementAt 1087
        . complementAt 264
        . complementAt 265
        . complementAt 266
        . complementAt 267
        . complementAt 268
