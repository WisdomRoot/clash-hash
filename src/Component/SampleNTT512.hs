{-# LANGUAGE TemplateHaskell #-}

module Component.SampleNTT512
  ( i272o24,
    i272o24l1,
    Lookahead (..),
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import Permutation qualified
import Sponge.NonPipelined (complementAt)
import TH (mkRead)

data Lookahead = Lookahead0 | Lookahead1
  deriving (Show, Eq, Generic, NFDataX)


data State
  = Idle
  | Permute (Index 24) (BitVector 1600) (Maybe (BitVector 12))
  | Squeeze (Index 56) (BitVector 1600) (Maybe (BitVector 12))
  deriving (Show, Eq, Generic, NFDataX)

-- | Extract 24-bit coefficient pair from state (pattern matched on all 56 indices)
$( mkRead
     "squeezeCoeff24"
     1600
     [ (i, i * 24, 24) | i <- [0 .. 55] ]
 )

{-# INLINE squeezeCoeff24 #-}

-- | Clean hash function for fixed 34-byte input with AXI4-Stream input handshaking
hash ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  Signal dom (AXI4Stream 272, Bool) ->
  Signal dom (AXI4Stream 24, Bool)
hash = mealy step Idle
  where
    step ::
      State ->
      (AXI4Stream 272, Bool) ->
      (State, (AXI4Stream 24, Bool))
    step st (AXI4Stream inputMsg msgValid _, tready) =
      case st of
        Idle ->
          -- SEED_TREADY is True, waiting for SEED_TVALID
          if msgValid
            then (Permute 0 (absorb34 inputMsg) Nothing, (idleAXI4Stream, False))
            else (Idle, (idleAXI4Stream, True))
        Permute roundIdx state buffer ->
          let state' = Permutation.keccakF1600 roundIdx state
           in if roundIdx == maxBound
                then (Squeeze 0 state' buffer, (idleAXI4Stream, False))
                else (Permute (roundIdx + 1) state' buffer, (idleAXI4Stream, False))
        Squeeze index state buffer ->
          let coeffPair = squeezeCoeff24 state index
              coeff0 = slice (SNat @11) (SNat @0) coeffPair
              coeff1 = slice (SNat @23) (SNat @12) coeffPair
              coeff0Val = unpack coeff0 :: Unsigned 12
              coeff1Val = unpack coeff1 :: Unsigned 12
              valid0 = coeff0Val < (3329 :: Unsigned 12)
              valid1 = coeff1Val < (3329 :: Unsigned 12)
              (pairReady, tdataOut, nextBuffer) = case (buffer, valid0, valid1) of
                (Nothing, False, False) -> (False, 0, Nothing)
                (Nothing, True, False) -> (False, 0, Just coeff0)
                (Nothing, False, True) -> (False, 0, Just coeff1)
                (Nothing, True, True) -> (True, coeff1 ++# coeff0, Nothing)
                (Just coeffB, False, False) -> (False, 0, Just coeffB)
                (Just coeffB, True, False) -> (True, coeff0 ++# coeffB, Nothing)
                (Just coeffB, False, True) -> (True, coeff1 ++# coeffB, Nothing)
                (Just coeffB, True, True) -> (True, coeff0 ++# coeffB, Just coeff1)
              outStream =
                AXI4Stream
                  { tdata = tdataOut,
                    tvalid = pairReady,
                    tlast = False
                  }
              nextIndex = if index == maxBound then 0 else index + 1
              nextState =
                if tready
                  then
                    if index == maxBound
                      then Permute 0 state nextBuffer
                      else Squeeze nextIndex state nextBuffer
                  else Squeeze index state buffer
           in (nextState, (outStream, False))

{-# ANN
  i272o24
  ( Synthesize
      { t_name = "SampleNTT512_I272_O24",
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
{-# NOINLINE i272o24 #-}
i272o24 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 24, Bool)
i272o24 clk rst en inputSig =
  withClockResetEnable clk rst en (hash inputSig)

{-# ANN
  i272o24l1
  ( Synthesize
      { t_name = "SN512_I272_O24_L1",
        t_inputs =
          [ PortName "LOOKAHEAD",
            PortName "CLK",
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
{-# NOINLINE i272o24l1 #-}
i272o24l1 ::
  Lookahead ->
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 24, Bool)
i272o24l1 _ clk rst en inputSig =
  withClockResetEnable clk rst en (hash inputSig)

-- | Absorb 34 bytes: place message and apply padding
absorb34 :: BitVector 272 -> BitVector 1600
absorb34 = pad34Bytes . placeMsg
  where
    --  Place 34-byte message at the start of state (no XOR needed since state starts at 0)
    placeMsg :: BitVector 272 -> BitVector 1600
    placeMsg msg = (0 :: BitVector 1328) ++# msg

    --  Padding function for fixed 34-byte input + SHAKE padding.
    pad34Bytes :: BitVector 1600 -> BitVector 1600
    pad34Bytes =
      complementAt 1343 -- final pad bit (last bit of rate)
        . complementAt 272 -- DS bits in byte 34
        . complementAt 273
        . complementAt 274
        . complementAt 275
        . complementAt 276


--------------------------------------------------------------------------------

