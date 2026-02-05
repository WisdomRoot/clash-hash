{-# LANGUAGE TemplateHaskell #-}

module Component.SampleNTT
  ( topEntity,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import Permutation qualified
import Sponge.NonPipelined (complementAt)
import TH (mkRead)

data State
  = Idle
  | Permute (Index 24) (BitVector 1600)
  | Squeeze (Index 112) (BitVector 1600)
  deriving (Show, Eq, Generic, NFDataX)

-- | Extract 12-bit coefficient from state (pattern matched on all 112 indices)
$(mkRead "squeezeCoeff12" 1600 [(i, 1588 - (i * 12), 12) | i <- [0 .. 111]])

{-# INLINE squeezeCoeff12 #-}

-- | Clean hash function for fixed 34-byte input with AXI4-Stream input handshaking
hash ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  Signal dom (AXI4Stream 272, Bool) ->
  Signal dom (AXI4Stream 12, Bool)
hash = mealy step Idle
  where
    step ::
      State ->
      (AXI4Stream 272, Bool) ->
      (State, (AXI4Stream 12, Bool))
    step st (AXI4Stream inputMsg msgValid _, tready) =
      case st of
        Idle ->
          -- MSG_TREADY is True, waiting for MSG_TVALID
          if msgValid
            then (Permute 0 (absorb34 inputMsg), (idleAXI4Stream, False))
            else (Idle, (idleAXI4Stream, True))
        Permute roundIdx state ->
          let state' = Permutation.keccakF1600Reversed roundIdx state
           in if roundIdx == maxBound
                then (Squeeze 0 state', (idleAXI4Stream, False))
                else (Permute (roundIdx + 1) state', (idleAXI4Stream, False))
        Squeeze index state ->
          let coeff = squeezeCoeff12 state index
              coeffRev = pack (reverse (unpack coeff :: Vec 12 Bit))
              coeffVal = unpack coeffRev :: Unsigned 12
              outStream = AXI4Stream {tdata = coeff, tvalid = coeffVal < (3329 :: Unsigned 12), tlast = False}
              nextState =
                if tready
                  then
                    if index == maxBound
                      then Permute 0 state
                      else Squeeze (index + 1) state
                  else Squeeze index state
           in (nextState, (outStream, False))

-- | Absorb 34 bytes: place message and apply padding
absorb34 :: BitVector 272 -> BitVector 1600
absorb34 = pad34Bytes . placeMsg
  where
    --  Place 34-byte message at the start of state (no XOR needed since state starts at 0)
    placeMsg :: BitVector 272 -> BitVector 1600
    placeMsg msg = msg ++# (0 :: BitVector 1328)

    --  Padding function for fixed 34-byte input + SHAKE padding.
    pad34Bytes :: BitVector 1600 -> BitVector 1600
    pad34Bytes =
      complementAt 256 -- final pad bit (last bit of rate)
        . complementAt 1323 -- DS bit in byte 34
        . complementAt 1324
        . complementAt 1325
        . complementAt 1326
        . complementAt 1327

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "Component_SampleNTT",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortProduct
              ""
              [ PortProduct "SEED" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
                PortName "COEFF_TREADY"
              ]
          ],
        t_output =
          PortProduct
            ""
            [ PortProduct "COEFF" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
              PortName "SEED_TREADY"
            ]
      }
  )
  #-}
{-# NOINLINE topEntity #-}
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 12, Bool)
topEntity clk rst en inputSig =
  withClockResetEnable clk rst en (hash inputSig)
