module Component.SampleNTT
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
      { t_name = "Component_SampleNTT",
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
{-# NOINLINE topEntity #-}
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 12, Bool)
topEntity clk rst en inputSig =
  withClockResetEnable clk rst en (hash inputSig)

data State
  = Idle
  | Permute (Index 24) (BitVector 1600)
  | Squeeze (Index 112) (BitVector 1600)
  deriving (Show, Eq, Generic, NFDataX)

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
          let state' = Permutation.keccakF1600 roundIdx state
           in if roundIdx == maxBound
                then (Squeeze 0 state', (idleAXI4Stream, False))
                else (Permute (roundIdx + 1) state', (idleAXI4Stream, False))
        Squeeze index state ->
          let coeff = squeezeCoeff12 index state
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

-- | Extract 12-bit coefficient from state (pattern matched on all 112 indices)
squeezeCoeff12 :: Index 112 -> BitVector 1600 -> BitVector 12
squeezeCoeff12 0 state = slice (SNat @1599) (SNat @1588) state
squeezeCoeff12 1 state = slice (SNat @1587) (SNat @1576) state
squeezeCoeff12 2 state = slice (SNat @1575) (SNat @1564) state
squeezeCoeff12 3 state = slice (SNat @1563) (SNat @1552) state
squeezeCoeff12 4 state = slice (SNat @1551) (SNat @1540) state
squeezeCoeff12 5 state = slice (SNat @1539) (SNat @1528) state
squeezeCoeff12 6 state = slice (SNat @1527) (SNat @1516) state
squeezeCoeff12 7 state = slice (SNat @1515) (SNat @1504) state
squeezeCoeff12 8 state = slice (SNat @1503) (SNat @1492) state
squeezeCoeff12 9 state = slice (SNat @1491) (SNat @1480) state
squeezeCoeff12 10 state = slice (SNat @1479) (SNat @1468) state
squeezeCoeff12 11 state = slice (SNat @1467) (SNat @1456) state
squeezeCoeff12 12 state = slice (SNat @1455) (SNat @1444) state
squeezeCoeff12 13 state = slice (SNat @1443) (SNat @1432) state
squeezeCoeff12 14 state = slice (SNat @1431) (SNat @1420) state
squeezeCoeff12 15 state = slice (SNat @1419) (SNat @1408) state
squeezeCoeff12 16 state = slice (SNat @1407) (SNat @1396) state
squeezeCoeff12 17 state = slice (SNat @1395) (SNat @1384) state
squeezeCoeff12 18 state = slice (SNat @1383) (SNat @1372) state
squeezeCoeff12 19 state = slice (SNat @1371) (SNat @1360) state
squeezeCoeff12 20 state = slice (SNat @1359) (SNat @1348) state
squeezeCoeff12 21 state = slice (SNat @1347) (SNat @1336) state
squeezeCoeff12 22 state = slice (SNat @1335) (SNat @1324) state
squeezeCoeff12 23 state = slice (SNat @1323) (SNat @1312) state
squeezeCoeff12 24 state = slice (SNat @1311) (SNat @1300) state
squeezeCoeff12 25 state = slice (SNat @1299) (SNat @1288) state
squeezeCoeff12 26 state = slice (SNat @1287) (SNat @1276) state
squeezeCoeff12 27 state = slice (SNat @1275) (SNat @1264) state
squeezeCoeff12 28 state = slice (SNat @1263) (SNat @1252) state
squeezeCoeff12 29 state = slice (SNat @1251) (SNat @1240) state
squeezeCoeff12 30 state = slice (SNat @1239) (SNat @1228) state
squeezeCoeff12 31 state = slice (SNat @1227) (SNat @1216) state
squeezeCoeff12 32 state = slice (SNat @1215) (SNat @1204) state
squeezeCoeff12 33 state = slice (SNat @1203) (SNat @1192) state
squeezeCoeff12 34 state = slice (SNat @1191) (SNat @1180) state
squeezeCoeff12 35 state = slice (SNat @1179) (SNat @1168) state
squeezeCoeff12 36 state = slice (SNat @1167) (SNat @1156) state
squeezeCoeff12 37 state = slice (SNat @1155) (SNat @1144) state
squeezeCoeff12 38 state = slice (SNat @1143) (SNat @1132) state
squeezeCoeff12 39 state = slice (SNat @1131) (SNat @1120) state
squeezeCoeff12 40 state = slice (SNat @1119) (SNat @1108) state
squeezeCoeff12 41 state = slice (SNat @1107) (SNat @1096) state
squeezeCoeff12 42 state = slice (SNat @1095) (SNat @1084) state
squeezeCoeff12 43 state = slice (SNat @1083) (SNat @1072) state
squeezeCoeff12 44 state = slice (SNat @1071) (SNat @1060) state
squeezeCoeff12 45 state = slice (SNat @1059) (SNat @1048) state
squeezeCoeff12 46 state = slice (SNat @1047) (SNat @1036) state
squeezeCoeff12 47 state = slice (SNat @1035) (SNat @1024) state
squeezeCoeff12 48 state = slice (SNat @1023) (SNat @1012) state
squeezeCoeff12 49 state = slice (SNat @1011) (SNat @1000) state
squeezeCoeff12 50 state = slice (SNat @999) (SNat @988) state
squeezeCoeff12 51 state = slice (SNat @987) (SNat @976) state
squeezeCoeff12 52 state = slice (SNat @975) (SNat @964) state
squeezeCoeff12 53 state = slice (SNat @963) (SNat @952) state
squeezeCoeff12 54 state = slice (SNat @951) (SNat @940) state
squeezeCoeff12 55 state = slice (SNat @939) (SNat @928) state
squeezeCoeff12 56 state = slice (SNat @927) (SNat @916) state
squeezeCoeff12 57 state = slice (SNat @915) (SNat @904) state
squeezeCoeff12 58 state = slice (SNat @903) (SNat @892) state
squeezeCoeff12 59 state = slice (SNat @891) (SNat @880) state
squeezeCoeff12 60 state = slice (SNat @879) (SNat @868) state
squeezeCoeff12 61 state = slice (SNat @867) (SNat @856) state
squeezeCoeff12 62 state = slice (SNat @855) (SNat @844) state
squeezeCoeff12 63 state = slice (SNat @843) (SNat @832) state
squeezeCoeff12 64 state = slice (SNat @831) (SNat @820) state
squeezeCoeff12 65 state = slice (SNat @819) (SNat @808) state
squeezeCoeff12 66 state = slice (SNat @807) (SNat @796) state
squeezeCoeff12 67 state = slice (SNat @795) (SNat @784) state
squeezeCoeff12 68 state = slice (SNat @783) (SNat @772) state
squeezeCoeff12 69 state = slice (SNat @771) (SNat @760) state
squeezeCoeff12 70 state = slice (SNat @759) (SNat @748) state
squeezeCoeff12 71 state = slice (SNat @747) (SNat @736) state
squeezeCoeff12 72 state = slice (SNat @735) (SNat @724) state
squeezeCoeff12 73 state = slice (SNat @723) (SNat @712) state
squeezeCoeff12 74 state = slice (SNat @711) (SNat @700) state
squeezeCoeff12 75 state = slice (SNat @699) (SNat @688) state
squeezeCoeff12 76 state = slice (SNat @687) (SNat @676) state
squeezeCoeff12 77 state = slice (SNat @675) (SNat @664) state
squeezeCoeff12 78 state = slice (SNat @663) (SNat @652) state
squeezeCoeff12 79 state = slice (SNat @651) (SNat @640) state
squeezeCoeff12 80 state = slice (SNat @639) (SNat @628) state
squeezeCoeff12 81 state = slice (SNat @627) (SNat @616) state
squeezeCoeff12 82 state = slice (SNat @615) (SNat @604) state
squeezeCoeff12 83 state = slice (SNat @603) (SNat @592) state
squeezeCoeff12 84 state = slice (SNat @591) (SNat @580) state
squeezeCoeff12 85 state = slice (SNat @579) (SNat @568) state
squeezeCoeff12 86 state = slice (SNat @567) (SNat @556) state
squeezeCoeff12 87 state = slice (SNat @555) (SNat @544) state
squeezeCoeff12 88 state = slice (SNat @543) (SNat @532) state
squeezeCoeff12 89 state = slice (SNat @531) (SNat @520) state
squeezeCoeff12 90 state = slice (SNat @519) (SNat @508) state
squeezeCoeff12 91 state = slice (SNat @507) (SNat @496) state
squeezeCoeff12 92 state = slice (SNat @495) (SNat @484) state
squeezeCoeff12 93 state = slice (SNat @483) (SNat @472) state
squeezeCoeff12 94 state = slice (SNat @471) (SNat @460) state
squeezeCoeff12 95 state = slice (SNat @459) (SNat @448) state
squeezeCoeff12 96 state = slice (SNat @447) (SNat @436) state
squeezeCoeff12 97 state = slice (SNat @435) (SNat @424) state
squeezeCoeff12 98 state = slice (SNat @423) (SNat @412) state
squeezeCoeff12 99 state = slice (SNat @411) (SNat @400) state
squeezeCoeff12 100 state = slice (SNat @399) (SNat @388) state
squeezeCoeff12 101 state = slice (SNat @387) (SNat @376) state
squeezeCoeff12 102 state = slice (SNat @375) (SNat @364) state
squeezeCoeff12 103 state = slice (SNat @363) (SNat @352) state
squeezeCoeff12 104 state = slice (SNat @351) (SNat @340) state
squeezeCoeff12 105 state = slice (SNat @339) (SNat @328) state
squeezeCoeff12 106 state = slice (SNat @327) (SNat @316) state
squeezeCoeff12 107 state = slice (SNat @315) (SNat @304) state
squeezeCoeff12 108 state = slice (SNat @303) (SNat @292) state
squeezeCoeff12 109 state = slice (SNat @291) (SNat @280) state
squeezeCoeff12 110 state = slice (SNat @279) (SNat @268) state
squeezeCoeff12 _ state = slice (SNat @267) (SNat @256) state
