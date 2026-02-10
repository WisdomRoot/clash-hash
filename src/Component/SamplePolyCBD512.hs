{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Component.SamplePolyCBD512
  ( i264o12,
    i264o24,
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

{-# ANN
  i264o24
  ( Synthesize
      { t_name = "SamplePolyCBD512_I264_O24",
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
{-# NOINLINE i264o24 #-}
i264o24 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector MsgBits) ->
  Signal System Bool ->
  Signal System (AXI4Stream 24, Bool)
i264o24 clk rst en msgSig treadySig =
  withClockResetEnable clk rst en (samplePolyCBD512_24 msgSig treadySig)

data Phase k = FirstBlock | SecondBlock (BitVector k) -- remaining bits from block0
  deriving (Show, Eq, Generic, NFDataX)

-- | State machine for SamplePolyCBD512 (eta=3).
--     256 coefficients => 256 * 6 bit of digest = 2 permutation
data State n phase
  = Absorb
  | Permute
      (Index 24) -- round index
      (Index n) -- coefficient count
      (BitVector 1600) -- state being permuted
      phase -- which block we are permuting
  | Squeeze
      (Index n) -- coefficient count
      (BitVector 1600) -- current block
      phase -- which block we are squeezing
  | Done
  deriving (Show, Eq, Generic, NFDataX)

samplePolyCBD512 ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  Signal dom (BitVector MsgBits) ->
  Signal dom Bool ->
  Signal dom (AXI4Stream 12, Bool)
samplePolyCBD512 = samplePolyCBD512With boundary12 emit12
  where
    boundary12 :: Index 256 -> BitVector 1600 -> Phase 2 -> Maybe (Phase 2)
    boundary12 coeffIdx block whichBlock =
      case whichBlock of
        FirstBlock ->
          if coeffIdx >= 181
            then
              let tail2 = slice (SNat @1087) (SNat @1086) block
               in Just (SecondBlock tail2)
            else Nothing
        SecondBlock _ -> Nothing

    emit12 :: Index 256 -> BitVector 1600 -> Phase 2 -> (BitVector 12, Bool, Phase 2, Phase 2)
    emit12 coeffIdx block whichBlock =
      case whichBlock of
        FirstBlock ->
          let bits6 = read6Block0 block (fromIntegral coeffIdx)
              coeffVal = cbd3 bits6
           in (coeffVal, False, FirstBlock, FirstBlock)
        SecondBlock tail2 ->
          let isLast = coeffIdx == 255
              bits6 =
                if coeffIdx == 181
                  then
                    let head4 = slice d3 d0 block
                     in head4 ++# tail2
                  else read6Block1 block (fromIntegral (coeffIdx - 182))
              coeffVal = cbd3 bits6
           in (coeffVal, isLast, SecondBlock tail2, SecondBlock tail2)

samplePolyCBD512_24 ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  Signal dom (BitVector MsgBits) ->
  Signal dom Bool ->
  Signal dom (AXI4Stream 24, Bool)
samplePolyCBD512_24 = samplePolyCBD512With boundary24 emit24
  where
    boundary24 :: Index 128 -> BitVector 1600 -> Phase 8 -> Maybe (Phase 8)
    boundary24 pairIdx block phase =
      case phase of
        FirstBlock ->
          if pairIdx >= pairBoundary
            then
              let bits8 = slice (SNat @1087) (SNat @1080) block
               in Just (SecondBlock bits8)
            else Nothing
        SecondBlock _ -> Nothing

    emit24 :: Index 128 -> BitVector 1600 -> Phase 8 -> (BitVector 24, Bool, Phase 8, Phase 8)
    emit24 pairIdx block phase =
      case phase of
        FirstBlock ->
          let pairIdxU = (fromIntegral pairIdx :: Unsigned 8)
              idx0 = fromIntegral (pairIdxU * 2) :: Index 181
              idx1 = fromIntegral (pairIdxU * 2 + 1) :: Index 181
              coeff0 = cbd3 (read6Block0 block idx0)
              coeff1 = cbd3 (read6Block0 block idx1)
           in (coeff1 ++# coeff0, False, FirstBlock, FirstBlock)
        SecondBlock bits8 ->
          if pairIdx == pairBoundary
            then
              let tail2 = slice d7 d6 bits8
                  coeff180Bits = slice d5 d0 bits8
                  coeff180 = cbd3 coeff180Bits
                  head4 = slice d3 d0 block
                  coeff181 = cbd3 (head4 ++# tail2)
               in (coeff181 ++# coeff180, False, SecondBlock bits8, SecondBlock bits8)
            else
              let pairIdxU = (fromIntegral pairIdx :: Unsigned 8)
                  coeff0U = pairIdxU * 2
                  coeff1U = coeff0U + 1
                  idx0 = fromIntegral (coeff0U - 182) :: Index 74
                  idx1 = fromIntegral (coeff1U - 182) :: Index 74
                  coeff0 = cbd3 (read6Block1 block idx0)
                  coeff1 = cbd3 (read6Block1 block idx1)
                  isLast = pairIdx == maxBound
               in (coeff1 ++# coeff0, isLast, SecondBlock bits8, SecondBlock bits8)
    pairBoundary :: Index 128
    pairBoundary = 90

samplePolyCBD512With ::
  forall dom n k m.
  (HiddenClockResetEnable dom, KnownNat n, KnownNat m, KnownNat k) =>
  (Index n -> BitVector 1600 -> Phase k -> Maybe (Phase k)) ->
  (Index n -> BitVector 1600 -> Phase k -> (BitVector m, Bool, Phase k, Phase k)) ->
  Signal dom (BitVector MsgBits) ->
  Signal dom Bool ->
  Signal dom (AXI4Stream m, Bool)
samplePolyCBD512With boundaryPhase emitPhase msgSig treadySig =
  mealy step Absorb (bundle (msgSig, treadySig))
  where
    step ::
      State n (Phase k) ->
      (BitVector MsgBits, Bool) ->
      (State n (Phase k), (AXI4Stream m, Bool))
    step st (msg, tready) =
      case st of
        Absorb ->
          let initState = absorb33Normal msg
           in (Permute 0 0 initState FirstBlock, (idleAXI4Stream, True))
        Permute roundIdx idx state phase ->
          let state' = Permutation.keccakF1600 roundIdx state
           in if roundIdx == maxBound
                then (Squeeze idx state' phase, (idleAXI4Stream, False))
                else
                  (Permute (roundIdx + 1) idx state' phase, (idleAXI4Stream, False))
        Squeeze idx block phase ->
          case boundaryPhase idx block phase of
            Just phase' -> (Permute 0 idx block phase', (idleAXI4Stream, False))
            Nothing ->
              let (tdataVal, isLast, phaseHold, phaseAdvance) = emitPhase idx block phase
                  outStream =
                    AXI4Stream
                      { tdata = tdataVal,
                        tvalid = True,
                        tlast = isLast
                      }
                  nextState
                    | tready && isLast = Done
                    | tready = Squeeze (idx + 1) block phaseAdvance
                    | otherwise = Squeeze idx block phaseHold
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
