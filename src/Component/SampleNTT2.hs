module Component.SampleNTT2
  ( topEntity,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import Permutation qualified
import Sponge.NonPipelined (complementAt)

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "Component_SampleNTT2",
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
  Signal System (AXI4Stream 24, Bool)
topEntity clk rst en inputSig =
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
              coeff0 = slice (SNat @23) (SNat @12) coeffPair
              coeff1 = slice (SNat @11) (SNat @0) coeffPair
              coeff0Rev = pack (reverse (unpack coeff0 :: Vec 12 Bit))
              coeff1Rev = pack (reverse (unpack coeff1 :: Vec 12 Bit))
              coeff0Val = unpack coeff0Rev :: Unsigned 12
              coeff1Val = unpack coeff1Rev :: Unsigned 12
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

-- | Extract 24-bit coefficient pair from state (pattern matched on all 56 indices)
squeezeCoeff24 :: Index 56 -> BitVector 1600 -> BitVector 24
squeezeCoeff24 0 state = slice (SNat @1599) (SNat @1576) state
squeezeCoeff24 1 state = slice (SNat @1575) (SNat @1552) state
squeezeCoeff24 2 state = slice (SNat @1551) (SNat @1528) state
squeezeCoeff24 3 state = slice (SNat @1527) (SNat @1504) state
squeezeCoeff24 4 state = slice (SNat @1503) (SNat @1480) state
squeezeCoeff24 5 state = slice (SNat @1479) (SNat @1456) state
squeezeCoeff24 6 state = slice (SNat @1455) (SNat @1432) state
squeezeCoeff24 7 state = slice (SNat @1431) (SNat @1408) state
squeezeCoeff24 8 state = slice (SNat @1407) (SNat @1384) state
squeezeCoeff24 9 state = slice (SNat @1383) (SNat @1360) state
squeezeCoeff24 10 state = slice (SNat @1359) (SNat @1336) state
squeezeCoeff24 11 state = slice (SNat @1335) (SNat @1312) state
squeezeCoeff24 12 state = slice (SNat @1311) (SNat @1288) state
squeezeCoeff24 13 state = slice (SNat @1287) (SNat @1264) state
squeezeCoeff24 14 state = slice (SNat @1263) (SNat @1240) state
squeezeCoeff24 15 state = slice (SNat @1239) (SNat @1216) state
squeezeCoeff24 16 state = slice (SNat @1215) (SNat @1192) state
squeezeCoeff24 17 state = slice (SNat @1191) (SNat @1168) state
squeezeCoeff24 18 state = slice (SNat @1167) (SNat @1144) state
squeezeCoeff24 19 state = slice (SNat @1143) (SNat @1120) state
squeezeCoeff24 20 state = slice (SNat @1119) (SNat @1096) state
squeezeCoeff24 21 state = slice (SNat @1095) (SNat @1072) state
squeezeCoeff24 22 state = slice (SNat @1071) (SNat @1048) state
squeezeCoeff24 23 state = slice (SNat @1047) (SNat @1024) state
squeezeCoeff24 24 state = slice (SNat @1023) (SNat @1000) state
squeezeCoeff24 25 state = slice (SNat @999) (SNat @976) state
squeezeCoeff24 26 state = slice (SNat @975) (SNat @952) state
squeezeCoeff24 27 state = slice (SNat @951) (SNat @928) state
squeezeCoeff24 28 state = slice (SNat @927) (SNat @904) state
squeezeCoeff24 29 state = slice (SNat @903) (SNat @880) state
squeezeCoeff24 30 state = slice (SNat @879) (SNat @856) state
squeezeCoeff24 31 state = slice (SNat @855) (SNat @832) state
squeezeCoeff24 32 state = slice (SNat @831) (SNat @808) state
squeezeCoeff24 33 state = slice (SNat @807) (SNat @784) state
squeezeCoeff24 34 state = slice (SNat @783) (SNat @760) state
squeezeCoeff24 35 state = slice (SNat @759) (SNat @736) state
squeezeCoeff24 36 state = slice (SNat @735) (SNat @712) state
squeezeCoeff24 37 state = slice (SNat @711) (SNat @688) state
squeezeCoeff24 38 state = slice (SNat @687) (SNat @664) state
squeezeCoeff24 39 state = slice (SNat @663) (SNat @640) state
squeezeCoeff24 40 state = slice (SNat @639) (SNat @616) state
squeezeCoeff24 41 state = slice (SNat @615) (SNat @592) state
squeezeCoeff24 42 state = slice (SNat @591) (SNat @568) state
squeezeCoeff24 43 state = slice (SNat @567) (SNat @544) state
squeezeCoeff24 44 state = slice (SNat @543) (SNat @520) state
squeezeCoeff24 45 state = slice (SNat @519) (SNat @496) state
squeezeCoeff24 46 state = slice (SNat @495) (SNat @472) state
squeezeCoeff24 47 state = slice (SNat @471) (SNat @448) state
squeezeCoeff24 48 state = slice (SNat @447) (SNat @424) state
squeezeCoeff24 49 state = slice (SNat @423) (SNat @400) state
squeezeCoeff24 50 state = slice (SNat @399) (SNat @376) state
squeezeCoeff24 51 state = slice (SNat @375) (SNat @352) state
squeezeCoeff24 52 state = slice (SNat @351) (SNat @328) state
squeezeCoeff24 53 state = slice (SNat @327) (SNat @304) state
squeezeCoeff24 54 state = slice (SNat @303) (SNat @280) state
squeezeCoeff24 _ state = slice (SNat @279) (SNat @256) state
