{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Sponge.XOR
  ( -- * XOR Implementations
    partialXOR,
    shiftAndMask,
    caseBasedXOR,
    chunkBasedXOR,
    staticXOR256,
    staticXOR256',
    staticXOR512,
    staticXOR512',
    staticXOR512_256,
    staticXOR128,
    staticXOR128',
    staticXOR128B,
  )
where

import Clash.Prelude hiding (permute)
import TH (mkMap)

type MsgBits = 64

-- | XOR a 64-bit block into state at beat position (beatCounter * 64)
--   module                                            area (µm²)   seq area (µm²)    seq %
--   --------------------------------------------------------------------------------------
--   Hash_Stateful5_topEntity_keccakF1600Round          15559.138            0.000     0.00%
--   Hash_Stateful5_topEntity_spongeFSM                 17579.940         8543.920    48.60%
--   Stateful5_SHA3                                     33139.078         8543.920    25.78%

-- [bench] Time/Mem: load 4.23s | compile 9.23s | synth 208.09s | mem 8912.86 MB
partialXOR :: BitVector 1600 -> BitVector 64 -> Index 17 -> BitVector 1600
partialXOR state block beatCounter =
  let -- Compute startIdx in wider type to avoid modulo wrapping
      startIdx = fromIntegral (fromIntegral beatCounter :: Unsigned 11) * 64 :: Index 1600

      blockVec = unpack block :: Vec 64 Bit -- MSB-first
      stateVec' = imap applyXor (unpack state :: Vec 1600 Bit)
        where
          applyXor :: Index 1600 -> Bit -> Bit
          applyXor i b
            | i >= startIdx && i < startIdx + 64 =
                let offset :: Index 1600
                    offset = i - startIdx
                    offset64 :: Index 64
                    offset64 = resize offset
                    blockBit = blockVec !! offset64
                 in b `xor` blockBit
            | otherwise = b
   in pack stateVec'

-- | Optimized XOR using shift-and-mask (BitVector operations only)
--   module                                            area (µm²)   seq area (µm²)    seq %
--   --------------------------------------------------------------------------------------
--   Hash_Stateful5_topEntity_keccakF1600Round          15574.566            0.000     0.00%
--   Hash_Stateful5_topEntity_spongeFSM                 18839.184         8543.920    45.35%
--   Stateful5_SHA3                                     34413.750         8543.920    24.83%
shiftAndMask :: BitVector 1600 -> BitVector 64 -> Index 17 -> BitVector 1600
shiftAndMask state block beatCounter =
  let -- Calculate bit position: beatCounter * 64
      bitPos = fromIntegral beatCounter * 64 :: Int

      -- Resize block to 1600 bits (zero-extended) and shift to position
      blockMask :: BitVector 1600
      blockMask = resize block `shiftL` bitPos
   in -- XOR the block into state at the correct position
      state `xor` blockMask

-- | Case-based XOR using Vec indexing and replace
-- This approach should generate case statements in Verilog
-- Expected pattern: extract via case, XOR 64 bits, write back via case
-- module                                            area (µm²)   seq area (µm²)    seq %
-- --------------------------------------------------------------------------------------
-- Hash_Stateful5_topEntity_keccakF1600Round          15558.340            0.000     0.00%
-- Hash_Stateful5_topEntity_spongeFSM                 17841.950         8543.920    47.89%
-- Stateful5_SHA3                                     33400.290         8543.920    25.58%
caseBasedXOR :: BitVector 1600 -> BitVector 64 -> Index 17 -> BitVector 1600
caseBasedXOR state block beatCounter =
  let stateVec = bitCoerce state :: Vec 25 (BitVector 64)
      xored = stateVec !! beatCounter `xor` block
      stateVec' = replace beatCounter xored stateVec
   in bitCoerce stateVec'

-- | Static case-based XOR using only BitVector slices.
-- Generates a 64-bit slice XOR and a case tree for writing back.
-- module                                            area (µm²)   seq area (µm²)    seq %
-- --------------------------------------------------------------------------------------
-- Hash_Stateful5_topEntity_keccakF1600Round          15047.886            0.000     0.00%
-- Hash_Stateful5_topEntity_spongeFSM                 16291.436         8543.920    52.44%
-- Stateful5_SHA3                                     31339.322         8543.920    27.26%
staticXOR256 :: BitVector 1600 -> BitVector 64 -> Index 17 -> BitVector 1600
staticXOR256 state block beatCounter =
  case beatCounter of
    0 -> setSlice (SNat @1599) (SNat @1536) (slice (SNat @1599) (SNat @1536) state `xor` block) state
    1 -> setSlice (SNat @1535) (SNat @1472) (slice (SNat @1535) (SNat @1472) state `xor` block) state
    2 -> setSlice (SNat @1471) (SNat @1408) (slice (SNat @1471) (SNat @1408) state `xor` block) state
    3 -> setSlice (SNat @1407) (SNat @1344) (slice (SNat @1407) (SNat @1344) state `xor` block) state
    4 -> setSlice (SNat @1343) (SNat @1280) (slice (SNat @1343) (SNat @1280) state `xor` block) state
    5 -> setSlice (SNat @1279) (SNat @1216) (slice (SNat @1279) (SNat @1216) state `xor` block) state
    6 -> setSlice (SNat @1215) (SNat @1152) (slice (SNat @1215) (SNat @1152) state `xor` block) state
    7 -> setSlice (SNat @1151) (SNat @1088) (slice (SNat @1151) (SNat @1088) state `xor` block) state
    8 -> setSlice (SNat @1087) (SNat @1024) (slice (SNat @1087) (SNat @1024) state `xor` block) state
    9 -> setSlice (SNat @1023) (SNat @960) (slice (SNat @1023) (SNat @960) state `xor` block) state
    10 -> setSlice (SNat @959) (SNat @896) (slice (SNat @959) (SNat @896) state `xor` block) state
    11 -> setSlice (SNat @895) (SNat @832) (slice (SNat @895) (SNat @832) state `xor` block) state
    12 -> setSlice (SNat @831) (SNat @768) (slice (SNat @831) (SNat @768) state `xor` block) state
    13 -> setSlice (SNat @767) (SNat @704) (slice (SNat @767) (SNat @704) state `xor` block) state
    14 -> setSlice (SNat @703) (SNat @640) (slice (SNat @703) (SNat @640) state `xor` block) state
    15 -> setSlice (SNat @639) (SNat @576) (slice (SNat @639) (SNat @576) state `xor` block) state
    16 -> setSlice (SNat @575) (SNat @512) (slice (SNat @575) (SNat @512) state `xor` block) state
    _ -> state

-- | Static XOR for 17 beats
--      0 -> state[63:0]    := state[63:0]    `xor` block
--      1 -> state[127:64]  := state[127:64]  `xor` block
--      2 -> state[191:128] := state[191:128] `xor` block
--      ...
-- Generated via Template Haskell.
$(mkMap "staticXOR256'" "xor" 1600
    [ (i, i * 64, 64) | i <- [0 :: Integer .. 16] ])

-- | Static XOR for 9 beats
--      0 -> state[63:0]    := state[63:0]    `xor` block
--      1 -> state[127:64]  := state[127:64]  `xor` block
--      2 -> state[191:128] := state[191:128] `xor` block
--      ...
-- Generated via Template Haskell.
$(mkMap "staticXOR512'" "xor" 1600
    [ (i, i * 64, 64) | i <- [0 :: Integer .. 8] ])

-- | Static XOR for 21 beats (normal-order SHAKE128 rate).
$(mkMap "staticXOR128'" "xor" 1600
    [ (i, i * 64, 64) | i <- [0 :: Integer .. 20] ])

-- | Static XOR for 3 beats of 256-bit input (SHA3-512 rate 576).
--   Beat 2 uses only the low 64 bits to stay within the 576-bit rate.
staticXOR512_256 :: BitVector 1600 -> BitVector 256 -> Index 3 -> BitVector 1600
staticXOR512_256 state block beatCounter =
  case beatCounter of
    0 ->
      setSlice
        (SNat @255)
        (SNat @0)
        (slice (SNat @255) (SNat @0) state `xor` block)
        state
    1 ->
      setSlice
        (SNat @511)
        (SNat @256)
        (slice (SNat @511) (SNat @256) state `xor` block)
        state
    _ ->
      let blockLo = slice (SNat @63) (SNat @0) block
       in setSlice
            (SNat @575)
            (SNat @512)
            (slice (SNat @575) (SNat @512) state `xor` blockLo)
            state

-- | Static case-based XOR covering SHA3-512 rate (9 beats).
staticXOR512 :: BitVector 1600 -> BitVector 64 -> Index 9 -> BitVector 1600
staticXOR512 state block beatCounter =
  case beatCounter of
    0 -> setSlice (SNat @1599) (SNat @1536) (slice (SNat @1599) (SNat @1536) state `xor` block) state
    1 -> setSlice (SNat @1535) (SNat @1472) (slice (SNat @1535) (SNat @1472) state `xor` block) state
    2 -> setSlice (SNat @1471) (SNat @1408) (slice (SNat @1471) (SNat @1408) state `xor` block) state
    3 -> setSlice (SNat @1407) (SNat @1344) (slice (SNat @1407) (SNat @1344) state `xor` block) state
    4 -> setSlice (SNat @1343) (SNat @1280) (slice (SNat @1343) (SNat @1280) state `xor` block) state
    5 -> setSlice (SNat @1279) (SNat @1216) (slice (SNat @1279) (SNat @1216) state `xor` block) state
    6 -> setSlice (SNat @1215) (SNat @1152) (slice (SNat @1215) (SNat @1152) state `xor` block) state
    7 -> setSlice (SNat @1151) (SNat @1088) (slice (SNat @1151) (SNat @1088) state `xor` block) state
    8 -> setSlice (SNat @1087) (SNat @1024) (slice (SNat @1087) (SNat @1024) state `xor` block) state
    _ -> state

-- | XOR helper covering the full SHAKE128 rate (21 beats).
staticXOR128 :: BitVector 1600 -> BitVector 64 -> Index 21 -> BitVector 1600
staticXOR128 state block beat =
  case beat of
    0 -> setSlice (SNat @1599) (SNat @1536) (slice (SNat @1599) (SNat @1536) state `xor` block) state
    1 -> setSlice (SNat @1535) (SNat @1472) (slice (SNat @1535) (SNat @1472) state `xor` block) state
    2 -> setSlice (SNat @1471) (SNat @1408) (slice (SNat @1471) (SNat @1408) state `xor` block) state
    3 -> setSlice (SNat @1407) (SNat @1344) (slice (SNat @1407) (SNat @1344) state `xor` block) state
    4 -> setSlice (SNat @1343) (SNat @1280) (slice (SNat @1343) (SNat @1280) state `xor` block) state
    5 -> setSlice (SNat @1279) (SNat @1216) (slice (SNat @1279) (SNat @1216) state `xor` block) state
    6 -> setSlice (SNat @1215) (SNat @1152) (slice (SNat @1215) (SNat @1152) state `xor` block) state
    7 -> setSlice (SNat @1151) (SNat @1088) (slice (SNat @1151) (SNat @1088) state `xor` block) state
    8 -> setSlice (SNat @1087) (SNat @1024) (slice (SNat @1087) (SNat @1024) state `xor` block) state
    9 -> setSlice (SNat @1023) (SNat @960) (slice (SNat @1023) (SNat @960) state `xor` block) state
    10 -> setSlice (SNat @959) (SNat @896) (slice (SNat @959) (SNat @896) state `xor` block) state
    11 -> setSlice (SNat @895) (SNat @832) (slice (SNat @895) (SNat @832) state `xor` block) state
    12 -> setSlice (SNat @831) (SNat @768) (slice (SNat @831) (SNat @768) state `xor` block) state
    13 -> setSlice (SNat @767) (SNat @704) (slice (SNat @767) (SNat @704) state `xor` block) state
    14 -> setSlice (SNat @703) (SNat @640) (slice (SNat @703) (SNat @640) state `xor` block) state
    15 -> setSlice (SNat @639) (SNat @576) (slice (SNat @639) (SNat @576) state `xor` block) state
    16 -> setSlice (SNat @575) (SNat @512) (slice (SNat @575) (SNat @512) state `xor` block) state
    17 -> setSlice (SNat @511) (SNat @448) (slice (SNat @511) (SNat @448) state `xor` block) state
    18 -> setSlice (SNat @447) (SNat @384) (slice (SNat @447) (SNat @384) state `xor` block) state
    19 -> setSlice (SNat @383) (SNat @320) (slice (SNat @383) (SNat @320) state `xor` block) state
    20 -> setSlice (SNat @319) (SNat @256) (slice (SNat @319) (SNat @256) state `xor` block) state
    _ -> state

-- | XOR helper covering the full SHAKE128 byte rate (168 beats).
staticXOR128B :: BitVector 1600 -> BitVector 8 -> Index 168 -> BitVector 1600
staticXOR128B state block beat =
  case beat of
    0 -> setSlice (SNat @1599) (SNat @1592) (slice (SNat @1599) (SNat @1592) state `xor` block) state
    1 -> setSlice (SNat @1591) (SNat @1584) (slice (SNat @1591) (SNat @1584) state `xor` block) state
    2 -> setSlice (SNat @1583) (SNat @1576) (slice (SNat @1583) (SNat @1576) state `xor` block) state
    3 -> setSlice (SNat @1575) (SNat @1568) (slice (SNat @1575) (SNat @1568) state `xor` block) state
    4 -> setSlice (SNat @1567) (SNat @1560) (slice (SNat @1567) (SNat @1560) state `xor` block) state
    5 -> setSlice (SNat @1559) (SNat @1552) (slice (SNat @1559) (SNat @1552) state `xor` block) state
    6 -> setSlice (SNat @1551) (SNat @1544) (slice (SNat @1551) (SNat @1544) state `xor` block) state
    7 -> setSlice (SNat @1543) (SNat @1536) (slice (SNat @1543) (SNat @1536) state `xor` block) state
    8 -> setSlice (SNat @1535) (SNat @1528) (slice (SNat @1535) (SNat @1528) state `xor` block) state
    9 -> setSlice (SNat @1527) (SNat @1520) (slice (SNat @1527) (SNat @1520) state `xor` block) state
    10 -> setSlice (SNat @1519) (SNat @1512) (slice (SNat @1519) (SNat @1512) state `xor` block) state
    11 -> setSlice (SNat @1511) (SNat @1504) (slice (SNat @1511) (SNat @1504) state `xor` block) state
    12 -> setSlice (SNat @1503) (SNat @1496) (slice (SNat @1503) (SNat @1496) state `xor` block) state
    13 -> setSlice (SNat @1495) (SNat @1488) (slice (SNat @1495) (SNat @1488) state `xor` block) state
    14 -> setSlice (SNat @1487) (SNat @1480) (slice (SNat @1487) (SNat @1480) state `xor` block) state
    15 -> setSlice (SNat @1479) (SNat @1472) (slice (SNat @1479) (SNat @1472) state `xor` block) state
    16 -> setSlice (SNat @1471) (SNat @1464) (slice (SNat @1471) (SNat @1464) state `xor` block) state
    17 -> setSlice (SNat @1463) (SNat @1456) (slice (SNat @1463) (SNat @1456) state `xor` block) state
    18 -> setSlice (SNat @1455) (SNat @1448) (slice (SNat @1455) (SNat @1448) state `xor` block) state
    19 -> setSlice (SNat @1447) (SNat @1440) (slice (SNat @1447) (SNat @1440) state `xor` block) state
    20 -> setSlice (SNat @1439) (SNat @1432) (slice (SNat @1439) (SNat @1432) state `xor` block) state
    21 -> setSlice (SNat @1431) (SNat @1424) (slice (SNat @1431) (SNat @1424) state `xor` block) state
    22 -> setSlice (SNat @1423) (SNat @1416) (slice (SNat @1423) (SNat @1416) state `xor` block) state
    23 -> setSlice (SNat @1415) (SNat @1408) (slice (SNat @1415) (SNat @1408) state `xor` block) state
    24 -> setSlice (SNat @1407) (SNat @1400) (slice (SNat @1407) (SNat @1400) state `xor` block) state
    25 -> setSlice (SNat @1399) (SNat @1392) (slice (SNat @1399) (SNat @1392) state `xor` block) state
    26 -> setSlice (SNat @1391) (SNat @1384) (slice (SNat @1391) (SNat @1384) state `xor` block) state
    27 -> setSlice (SNat @1383) (SNat @1376) (slice (SNat @1383) (SNat @1376) state `xor` block) state
    28 -> setSlice (SNat @1375) (SNat @1368) (slice (SNat @1375) (SNat @1368) state `xor` block) state
    29 -> setSlice (SNat @1367) (SNat @1360) (slice (SNat @1367) (SNat @1360) state `xor` block) state
    30 -> setSlice (SNat @1359) (SNat @1352) (slice (SNat @1359) (SNat @1352) state `xor` block) state
    31 -> setSlice (SNat @1351) (SNat @1344) (slice (SNat @1351) (SNat @1344) state `xor` block) state
    32 -> setSlice (SNat @1343) (SNat @1336) (slice (SNat @1343) (SNat @1336) state `xor` block) state
    33 -> setSlice (SNat @1335) (SNat @1328) (slice (SNat @1335) (SNat @1328) state `xor` block) state
    34 -> setSlice (SNat @1327) (SNat @1320) (slice (SNat @1327) (SNat @1320) state `xor` block) state
    35 -> setSlice (SNat @1319) (SNat @1312) (slice (SNat @1319) (SNat @1312) state `xor` block) state
    36 -> setSlice (SNat @1311) (SNat @1304) (slice (SNat @1311) (SNat @1304) state `xor` block) state
    37 -> setSlice (SNat @1303) (SNat @1296) (slice (SNat @1303) (SNat @1296) state `xor` block) state
    38 -> setSlice (SNat @1295) (SNat @1288) (slice (SNat @1295) (SNat @1288) state `xor` block) state
    39 -> setSlice (SNat @1287) (SNat @1280) (slice (SNat @1287) (SNat @1280) state `xor` block) state
    40 -> setSlice (SNat @1279) (SNat @1272) (slice (SNat @1279) (SNat @1272) state `xor` block) state
    41 -> setSlice (SNat @1271) (SNat @1264) (slice (SNat @1271) (SNat @1264) state `xor` block) state
    42 -> setSlice (SNat @1263) (SNat @1256) (slice (SNat @1263) (SNat @1256) state `xor` block) state
    43 -> setSlice (SNat @1255) (SNat @1248) (slice (SNat @1255) (SNat @1248) state `xor` block) state
    44 -> setSlice (SNat @1247) (SNat @1240) (slice (SNat @1247) (SNat @1240) state `xor` block) state
    45 -> setSlice (SNat @1239) (SNat @1232) (slice (SNat @1239) (SNat @1232) state `xor` block) state
    46 -> setSlice (SNat @1231) (SNat @1224) (slice (SNat @1231) (SNat @1224) state `xor` block) state
    47 -> setSlice (SNat @1223) (SNat @1216) (slice (SNat @1223) (SNat @1216) state `xor` block) state
    48 -> setSlice (SNat @1215) (SNat @1208) (slice (SNat @1215) (SNat @1208) state `xor` block) state
    49 -> setSlice (SNat @1207) (SNat @1200) (slice (SNat @1207) (SNat @1200) state `xor` block) state
    50 -> setSlice (SNat @1199) (SNat @1192) (slice (SNat @1199) (SNat @1192) state `xor` block) state
    51 -> setSlice (SNat @1191) (SNat @1184) (slice (SNat @1191) (SNat @1184) state `xor` block) state
    52 -> setSlice (SNat @1183) (SNat @1176) (slice (SNat @1183) (SNat @1176) state `xor` block) state
    53 -> setSlice (SNat @1175) (SNat @1168) (slice (SNat @1175) (SNat @1168) state `xor` block) state
    54 -> setSlice (SNat @1167) (SNat @1160) (slice (SNat @1167) (SNat @1160) state `xor` block) state
    55 -> setSlice (SNat @1159) (SNat @1152) (slice (SNat @1159) (SNat @1152) state `xor` block) state
    56 -> setSlice (SNat @1151) (SNat @1144) (slice (SNat @1151) (SNat @1144) state `xor` block) state
    57 -> setSlice (SNat @1143) (SNat @1136) (slice (SNat @1143) (SNat @1136) state `xor` block) state
    58 -> setSlice (SNat @1135) (SNat @1128) (slice (SNat @1135) (SNat @1128) state `xor` block) state
    59 -> setSlice (SNat @1127) (SNat @1120) (slice (SNat @1127) (SNat @1120) state `xor` block) state
    60 -> setSlice (SNat @1119) (SNat @1112) (slice (SNat @1119) (SNat @1112) state `xor` block) state
    61 -> setSlice (SNat @1111) (SNat @1104) (slice (SNat @1111) (SNat @1104) state `xor` block) state
    62 -> setSlice (SNat @1103) (SNat @1096) (slice (SNat @1103) (SNat @1096) state `xor` block) state
    63 -> setSlice (SNat @1095) (SNat @1088) (slice (SNat @1095) (SNat @1088) state `xor` block) state
    64 -> setSlice (SNat @1087) (SNat @1080) (slice (SNat @1087) (SNat @1080) state `xor` block) state
    65 -> setSlice (SNat @1079) (SNat @1072) (slice (SNat @1079) (SNat @1072) state `xor` block) state
    66 -> setSlice (SNat @1071) (SNat @1064) (slice (SNat @1071) (SNat @1064) state `xor` block) state
    67 -> setSlice (SNat @1063) (SNat @1056) (slice (SNat @1063) (SNat @1056) state `xor` block) state
    68 -> setSlice (SNat @1055) (SNat @1048) (slice (SNat @1055) (SNat @1048) state `xor` block) state
    69 -> setSlice (SNat @1047) (SNat @1040) (slice (SNat @1047) (SNat @1040) state `xor` block) state
    70 -> setSlice (SNat @1039) (SNat @1032) (slice (SNat @1039) (SNat @1032) state `xor` block) state
    71 -> setSlice (SNat @1031) (SNat @1024) (slice (SNat @1031) (SNat @1024) state `xor` block) state
    72 -> setSlice (SNat @1023) (SNat @1016) (slice (SNat @1023) (SNat @1016) state `xor` block) state
    73 -> setSlice (SNat @1015) (SNat @1008) (slice (SNat @1015) (SNat @1008) state `xor` block) state
    74 -> setSlice (SNat @1007) (SNat @1000) (slice (SNat @1007) (SNat @1000) state `xor` block) state
    75 -> setSlice (SNat @999) (SNat @992) (slice (SNat @999) (SNat @992) state `xor` block) state
    76 -> setSlice (SNat @991) (SNat @984) (slice (SNat @991) (SNat @984) state `xor` block) state
    77 -> setSlice (SNat @983) (SNat @976) (slice (SNat @983) (SNat @976) state `xor` block) state
    78 -> setSlice (SNat @975) (SNat @968) (slice (SNat @975) (SNat @968) state `xor` block) state
    79 -> setSlice (SNat @967) (SNat @960) (slice (SNat @967) (SNat @960) state `xor` block) state
    80 -> setSlice (SNat @959) (SNat @952) (slice (SNat @959) (SNat @952) state `xor` block) state
    81 -> setSlice (SNat @951) (SNat @944) (slice (SNat @951) (SNat @944) state `xor` block) state
    82 -> setSlice (SNat @943) (SNat @936) (slice (SNat @943) (SNat @936) state `xor` block) state
    83 -> setSlice (SNat @935) (SNat @928) (slice (SNat @935) (SNat @928) state `xor` block) state
    84 -> setSlice (SNat @927) (SNat @920) (slice (SNat @927) (SNat @920) state `xor` block) state
    85 -> setSlice (SNat @919) (SNat @912) (slice (SNat @919) (SNat @912) state `xor` block) state
    86 -> setSlice (SNat @911) (SNat @904) (slice (SNat @911) (SNat @904) state `xor` block) state
    87 -> setSlice (SNat @903) (SNat @896) (slice (SNat @903) (SNat @896) state `xor` block) state
    88 -> setSlice (SNat @895) (SNat @888) (slice (SNat @895) (SNat @888) state `xor` block) state
    89 -> setSlice (SNat @887) (SNat @880) (slice (SNat @887) (SNat @880) state `xor` block) state
    90 -> setSlice (SNat @879) (SNat @872) (slice (SNat @879) (SNat @872) state `xor` block) state
    91 -> setSlice (SNat @871) (SNat @864) (slice (SNat @871) (SNat @864) state `xor` block) state
    92 -> setSlice (SNat @863) (SNat @856) (slice (SNat @863) (SNat @856) state `xor` block) state
    93 -> setSlice (SNat @855) (SNat @848) (slice (SNat @855) (SNat @848) state `xor` block) state
    94 -> setSlice (SNat @847) (SNat @840) (slice (SNat @847) (SNat @840) state `xor` block) state
    95 -> setSlice (SNat @839) (SNat @832) (slice (SNat @839) (SNat @832) state `xor` block) state
    96 -> setSlice (SNat @831) (SNat @824) (slice (SNat @831) (SNat @824) state `xor` block) state
    97 -> setSlice (SNat @823) (SNat @816) (slice (SNat @823) (SNat @816) state `xor` block) state
    98 -> setSlice (SNat @815) (SNat @808) (slice (SNat @815) (SNat @808) state `xor` block) state
    99 -> setSlice (SNat @807) (SNat @800) (slice (SNat @807) (SNat @800) state `xor` block) state
    100 -> setSlice (SNat @799) (SNat @792) (slice (SNat @799) (SNat @792) state `xor` block) state
    101 -> setSlice (SNat @791) (SNat @784) (slice (SNat @791) (SNat @784) state `xor` block) state
    102 -> setSlice (SNat @783) (SNat @776) (slice (SNat @783) (SNat @776) state `xor` block) state
    103 -> setSlice (SNat @775) (SNat @768) (slice (SNat @775) (SNat @768) state `xor` block) state
    104 -> setSlice (SNat @767) (SNat @760) (slice (SNat @767) (SNat @760) state `xor` block) state
    105 -> setSlice (SNat @759) (SNat @752) (slice (SNat @759) (SNat @752) state `xor` block) state
    106 -> setSlice (SNat @751) (SNat @744) (slice (SNat @751) (SNat @744) state `xor` block) state
    107 -> setSlice (SNat @743) (SNat @736) (slice (SNat @743) (SNat @736) state `xor` block) state
    108 -> setSlice (SNat @735) (SNat @728) (slice (SNat @735) (SNat @728) state `xor` block) state
    109 -> setSlice (SNat @727) (SNat @720) (slice (SNat @727) (SNat @720) state `xor` block) state
    110 -> setSlice (SNat @719) (SNat @712) (slice (SNat @719) (SNat @712) state `xor` block) state
    111 -> setSlice (SNat @711) (SNat @704) (slice (SNat @711) (SNat @704) state `xor` block) state
    112 -> setSlice (SNat @703) (SNat @696) (slice (SNat @703) (SNat @696) state `xor` block) state
    113 -> setSlice (SNat @695) (SNat @688) (slice (SNat @695) (SNat @688) state `xor` block) state
    114 -> setSlice (SNat @687) (SNat @680) (slice (SNat @687) (SNat @680) state `xor` block) state
    115 -> setSlice (SNat @679) (SNat @672) (slice (SNat @679) (SNat @672) state `xor` block) state
    116 -> setSlice (SNat @671) (SNat @664) (slice (SNat @671) (SNat @664) state `xor` block) state
    117 -> setSlice (SNat @663) (SNat @656) (slice (SNat @663) (SNat @656) state `xor` block) state
    118 -> setSlice (SNat @655) (SNat @648) (slice (SNat @655) (SNat @648) state `xor` block) state
    119 -> setSlice (SNat @647) (SNat @640) (slice (SNat @647) (SNat @640) state `xor` block) state
    120 -> setSlice (SNat @639) (SNat @632) (slice (SNat @639) (SNat @632) state `xor` block) state
    121 -> setSlice (SNat @631) (SNat @624) (slice (SNat @631) (SNat @624) state `xor` block) state
    122 -> setSlice (SNat @623) (SNat @616) (slice (SNat @623) (SNat @616) state `xor` block) state
    123 -> setSlice (SNat @615) (SNat @608) (slice (SNat @615) (SNat @608) state `xor` block) state
    124 -> setSlice (SNat @607) (SNat @600) (slice (SNat @607) (SNat @600) state `xor` block) state
    125 -> setSlice (SNat @599) (SNat @592) (slice (SNat @599) (SNat @592) state `xor` block) state
    126 -> setSlice (SNat @591) (SNat @584) (slice (SNat @591) (SNat @584) state `xor` block) state
    127 -> setSlice (SNat @583) (SNat @576) (slice (SNat @583) (SNat @576) state `xor` block) state
    128 -> setSlice (SNat @575) (SNat @568) (slice (SNat @575) (SNat @568) state `xor` block) state
    129 -> setSlice (SNat @567) (SNat @560) (slice (SNat @567) (SNat @560) state `xor` block) state
    130 -> setSlice (SNat @559) (SNat @552) (slice (SNat @559) (SNat @552) state `xor` block) state
    131 -> setSlice (SNat @551) (SNat @544) (slice (SNat @551) (SNat @544) state `xor` block) state
    132 -> setSlice (SNat @543) (SNat @536) (slice (SNat @543) (SNat @536) state `xor` block) state
    133 -> setSlice (SNat @535) (SNat @528) (slice (SNat @535) (SNat @528) state `xor` block) state
    134 -> setSlice (SNat @527) (SNat @520) (slice (SNat @527) (SNat @520) state `xor` block) state
    135 -> setSlice (SNat @519) (SNat @512) (slice (SNat @519) (SNat @512) state `xor` block) state
    136 -> setSlice (SNat @511) (SNat @504) (slice (SNat @511) (SNat @504) state `xor` block) state
    137 -> setSlice (SNat @503) (SNat @496) (slice (SNat @503) (SNat @496) state `xor` block) state
    138 -> setSlice (SNat @495) (SNat @488) (slice (SNat @495) (SNat @488) state `xor` block) state
    139 -> setSlice (SNat @487) (SNat @480) (slice (SNat @487) (SNat @480) state `xor` block) state
    140 -> setSlice (SNat @479) (SNat @472) (slice (SNat @479) (SNat @472) state `xor` block) state
    141 -> setSlice (SNat @471) (SNat @464) (slice (SNat @471) (SNat @464) state `xor` block) state
    142 -> setSlice (SNat @463) (SNat @456) (slice (SNat @463) (SNat @456) state `xor` block) state
    143 -> setSlice (SNat @455) (SNat @448) (slice (SNat @455) (SNat @448) state `xor` block) state
    144 -> setSlice (SNat @447) (SNat @440) (slice (SNat @447) (SNat @440) state `xor` block) state
    145 -> setSlice (SNat @439) (SNat @432) (slice (SNat @439) (SNat @432) state `xor` block) state
    146 -> setSlice (SNat @431) (SNat @424) (slice (SNat @431) (SNat @424) state `xor` block) state
    147 -> setSlice (SNat @423) (SNat @416) (slice (SNat @423) (SNat @416) state `xor` block) state
    148 -> setSlice (SNat @415) (SNat @408) (slice (SNat @415) (SNat @408) state `xor` block) state
    149 -> setSlice (SNat @407) (SNat @400) (slice (SNat @407) (SNat @400) state `xor` block) state
    150 -> setSlice (SNat @399) (SNat @392) (slice (SNat @399) (SNat @392) state `xor` block) state
    151 -> setSlice (SNat @391) (SNat @384) (slice (SNat @391) (SNat @384) state `xor` block) state
    152 -> setSlice (SNat @383) (SNat @376) (slice (SNat @383) (SNat @376) state `xor` block) state
    153 -> setSlice (SNat @375) (SNat @368) (slice (SNat @375) (SNat @368) state `xor` block) state
    154 -> setSlice (SNat @367) (SNat @360) (slice (SNat @367) (SNat @360) state `xor` block) state
    155 -> setSlice (SNat @359) (SNat @352) (slice (SNat @359) (SNat @352) state `xor` block) state
    156 -> setSlice (SNat @351) (SNat @344) (slice (SNat @351) (SNat @344) state `xor` block) state
    157 -> setSlice (SNat @343) (SNat @336) (slice (SNat @343) (SNat @336) state `xor` block) state
    158 -> setSlice (SNat @335) (SNat @328) (slice (SNat @335) (SNat @328) state `xor` block) state
    159 -> setSlice (SNat @327) (SNat @320) (slice (SNat @327) (SNat @320) state `xor` block) state
    160 -> setSlice (SNat @319) (SNat @312) (slice (SNat @319) (SNat @312) state `xor` block) state
    161 -> setSlice (SNat @311) (SNat @304) (slice (SNat @311) (SNat @304) state `xor` block) state
    162 -> setSlice (SNat @303) (SNat @296) (slice (SNat @303) (SNat @296) state `xor` block) state
    163 -> setSlice (SNat @295) (SNat @288) (slice (SNat @295) (SNat @288) state `xor` block) state
    164 -> setSlice (SNat @287) (SNat @280) (slice (SNat @287) (SNat @280) state `xor` block) state
    165 -> setSlice (SNat @279) (SNat @272) (slice (SNat @279) (SNat @272) state `xor` block) state
    166 -> setSlice (SNat @271) (SNat @264) (slice (SNat @271) (SNat @264) state `xor` block) state
    167 -> setSlice (SNat @263) (SNat @256) (slice (SNat @263) (SNat @256) state `xor` block) state
    _ -> state

-- | Chunk-based XOR using slice operations
-- This approach extracts, modifies, and reassembles specific bit ranges
--   module                                            area (µm²)   seq area (µm²)    seq %
--   --------------------------------------------------------------------------------------
--   Hash_Stateful5_topEntity_keccakF1600Round          15559.138            0.000     0.00%
--   Hash_Stateful5_topEntity_spongeFSM                 19673.892         8543.920    43.43%
--   Stateful5_SHA3                                     35233.030         8543.920    24.25%
chunkBasedXOR :: BitVector 1600 -> BitVector 64 -> Index 17 -> BitVector 1600
chunkBasedXOR state block beatCounter =
  let -- Calculate bit position: beatCounter * 64
      bitPos = fromIntegral beatCounter * 64 :: Int

      -- Extract the 64-bit chunk at bitPos, XOR it, then put it back
      -- Method: shift right to align, truncate to get 64 bits, XOR, shift back
      stateTarget = truncateB (state `shiftR` bitPos) :: BitVector 64
      stateTarget' = stateTarget `xor` block

      -- Mask: clear the target 64-bit region
      mask = complement (resize (maxBound :: BitVector 64) `shiftL` bitPos) :: BitVector 1600

      -- Apply: clear region, then OR in new value
      stateCleared = state .&. mask
      stateNew = resize stateTarget' `shiftL` bitPos
   in stateCleared .|. stateNew
