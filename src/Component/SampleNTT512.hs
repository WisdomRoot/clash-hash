module Component.SampleNTT512
  ( i272o24,
    i272o24l1,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import Permutation qualified
import Sponge.NonPipelined (complementAt)

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
{-# NOINLINE i272o24l1 #-}
i272o24l1 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 24, Bool)
i272o24l1 clk rst en inputSig =
  withClockResetEnable clk rst en (hash inputSig)

data State
  = Idle
  | Permute (Index 24) (BitVector 1600) (Maybe (BitVector 12))
  | Squeeze (Index 56) (BitVector 1600) (Maybe (BitVector 12))
  deriving (Show, Eq, Generic, NFDataX)

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
          let coeffPair = squeezeCoeff24 index state
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

-- | Extract 24-bit coefficient pair from state (pattern matched on all 56 indices)
squeezeCoeff24 :: Index 56 -> BitVector 1600 -> BitVector 24
squeezeCoeff24 0 state = slice (SNat @23) (SNat @0) state
squeezeCoeff24 1 state = slice (SNat @47) (SNat @24) state
squeezeCoeff24 2 state = slice (SNat @71) (SNat @48) state
squeezeCoeff24 3 state = slice (SNat @95) (SNat @72) state
squeezeCoeff24 4 state = slice (SNat @119) (SNat @96) state
squeezeCoeff24 5 state = slice (SNat @143) (SNat @120) state
squeezeCoeff24 6 state = slice (SNat @167) (SNat @144) state
squeezeCoeff24 7 state = slice (SNat @191) (SNat @168) state
squeezeCoeff24 8 state = slice (SNat @215) (SNat @192) state
squeezeCoeff24 9 state = slice (SNat @239) (SNat @216) state
squeezeCoeff24 10 state = slice (SNat @263) (SNat @240) state
squeezeCoeff24 11 state = slice (SNat @287) (SNat @264) state
squeezeCoeff24 12 state = slice (SNat @311) (SNat @288) state
squeezeCoeff24 13 state = slice (SNat @335) (SNat @312) state
squeezeCoeff24 14 state = slice (SNat @359) (SNat @336) state
squeezeCoeff24 15 state = slice (SNat @383) (SNat @360) state
squeezeCoeff24 16 state = slice (SNat @407) (SNat @384) state
squeezeCoeff24 17 state = slice (SNat @431) (SNat @408) state
squeezeCoeff24 18 state = slice (SNat @455) (SNat @432) state
squeezeCoeff24 19 state = slice (SNat @479) (SNat @456) state
squeezeCoeff24 20 state = slice (SNat @503) (SNat @480) state
squeezeCoeff24 21 state = slice (SNat @527) (SNat @504) state
squeezeCoeff24 22 state = slice (SNat @551) (SNat @528) state
squeezeCoeff24 23 state = slice (SNat @575) (SNat @552) state
squeezeCoeff24 24 state = slice (SNat @599) (SNat @576) state
squeezeCoeff24 25 state = slice (SNat @623) (SNat @600) state
squeezeCoeff24 26 state = slice (SNat @647) (SNat @624) state
squeezeCoeff24 27 state = slice (SNat @671) (SNat @648) state
squeezeCoeff24 28 state = slice (SNat @695) (SNat @672) state
squeezeCoeff24 29 state = slice (SNat @719) (SNat @696) state
squeezeCoeff24 30 state = slice (SNat @743) (SNat @720) state
squeezeCoeff24 31 state = slice (SNat @767) (SNat @744) state
squeezeCoeff24 32 state = slice (SNat @791) (SNat @768) state
squeezeCoeff24 33 state = slice (SNat @815) (SNat @792) state
squeezeCoeff24 34 state = slice (SNat @839) (SNat @816) state
squeezeCoeff24 35 state = slice (SNat @863) (SNat @840) state
squeezeCoeff24 36 state = slice (SNat @887) (SNat @864) state
squeezeCoeff24 37 state = slice (SNat @911) (SNat @888) state
squeezeCoeff24 38 state = slice (SNat @935) (SNat @912) state
squeezeCoeff24 39 state = slice (SNat @959) (SNat @936) state
squeezeCoeff24 40 state = slice (SNat @983) (SNat @960) state
squeezeCoeff24 41 state = slice (SNat @1007) (SNat @984) state
squeezeCoeff24 42 state = slice (SNat @1031) (SNat @1008) state
squeezeCoeff24 43 state = slice (SNat @1055) (SNat @1032) state
squeezeCoeff24 44 state = slice (SNat @1079) (SNat @1056) state
squeezeCoeff24 45 state = slice (SNat @1103) (SNat @1080) state
squeezeCoeff24 46 state = slice (SNat @1127) (SNat @1104) state
squeezeCoeff24 47 state = slice (SNat @1151) (SNat @1128) state
squeezeCoeff24 48 state = slice (SNat @1175) (SNat @1152) state
squeezeCoeff24 49 state = slice (SNat @1199) (SNat @1176) state
squeezeCoeff24 50 state = slice (SNat @1223) (SNat @1200) state
squeezeCoeff24 51 state = slice (SNat @1247) (SNat @1224) state
squeezeCoeff24 52 state = slice (SNat @1271) (SNat @1248) state
squeezeCoeff24 53 state = slice (SNat @1295) (SNat @1272) state
squeezeCoeff24 54 state = slice (SNat @1319) (SNat @1296) state
squeezeCoeff24 _ state = slice (SNat @1343) (SNat @1320) state
