module Component.SamplePolyCBD.Common
  ( absorb33,
    squeezeSlice,
    extractBits4,
    extractTop6,
    cbd2,
    cbd3,
    cbdDiff6,
  )
where

import Clash.Prelude
import Sponge.NonPipelined (complementAt)

-- | Absorb 33 bytes: place message into the first 5 beats and apply SHAKE256 padding.
absorb33 :: BitVector 264 -> BitVector 1600
absorb33 = pad33Bytes . placeMsg
  where
    placeMsg :: BitVector 264 -> BitVector 1600
    placeMsg msg = msg ++# (0 :: BitVector 1336)

    pad33Bytes :: BitVector 1600 -> BitVector 1600
    pad33Bytes =
      complementAt 512
        . complementAt 1331
        . complementAt 1332
        . complementAt 1333
        . complementAt 1334
        . complementAt 1335

-- | Extract 64-bit output words in SHAKE256 order.
squeezeSlice :: Index 17 -> BitVector 1600 -> BitVector 64
squeezeSlice 0 state = slice (SNat @1599) (SNat @1536) state
squeezeSlice 1 state = slice (SNat @1535) (SNat @1472) state
squeezeSlice 2 state = slice (SNat @1471) (SNat @1408) state
squeezeSlice 3 state = slice (SNat @1407) (SNat @1344) state
squeezeSlice 4 state = slice (SNat @1343) (SNat @1280) state
squeezeSlice 5 state = slice (SNat @1279) (SNat @1216) state
squeezeSlice 6 state = slice (SNat @1215) (SNat @1152) state
squeezeSlice 7 state = slice (SNat @1151) (SNat @1088) state
squeezeSlice 8 state = slice (SNat @1087) (SNat @1024) state
squeezeSlice 9 state = slice (SNat @1023) (SNat @960) state
squeezeSlice 10 state = slice (SNat @959) (SNat @896) state
squeezeSlice 11 state = slice (SNat @895) (SNat @832) state
squeezeSlice 12 state = slice (SNat @831) (SNat @768) state
squeezeSlice 13 state = slice (SNat @767) (SNat @704) state
squeezeSlice 14 state = slice (SNat @703) (SNat @640) state
squeezeSlice 15 state = slice (SNat @639) (SNat @576) state
squeezeSlice _ state = slice (SNat @575) (SNat @512) state

-- | Extract 4-bit chunk from a 64-bit word based on coefficient index (0-15).
extractBits4 :: Index 16 -> BitVector 64 -> BitVector 4
extractBits4 0 w = slice (SNat @63) (SNat @60) w
extractBits4 1 w = slice (SNat @59) (SNat @56) w
extractBits4 2 w = slice (SNat @55) (SNat @52) w
extractBits4 3 w = slice (SNat @51) (SNat @48) w
extractBits4 4 w = slice (SNat @47) (SNat @44) w
extractBits4 5 w = slice (SNat @43) (SNat @40) w
extractBits4 6 w = slice (SNat @39) (SNat @36) w
extractBits4 7 w = slice (SNat @35) (SNat @32) w
extractBits4 8 w = slice (SNat @31) (SNat @28) w
extractBits4 9 w = slice (SNat @27) (SNat @24) w
extractBits4 10 w = slice (SNat @23) (SNat @20) w
extractBits4 11 w = slice (SNat @19) (SNat @16) w
extractBits4 12 w = slice (SNat @15) (SNat @12) w
extractBits4 13 w = slice (SNat @11) (SNat @8) w
extractBits4 14 w = slice (SNat @7) (SNat @4) w
extractBits4 _ w = slice (SNat @3) (SNat @0) w

-- | Extract top 6 bits from the 128-bit buffer.
extractTop6 :: BitVector 128 -> BitVector 6
extractTop6 = slice (SNat @127) (SNat @122)

-- | CBD(eta=2): Convert 4 bits to a coefficient in [-2, 2] mod 3329.
cbd2 :: BitVector 4 -> BitVector 12
cbd2 bits =
  let b0 = resize (unpack (slice d3 d3 bits) :: Unsigned 1) :: Unsigned 2
      b1 = resize (unpack (slice d2 d2 bits) :: Unsigned 1) :: Unsigned 2
      b2 = resize (unpack (slice d1 d1 bits) :: Unsigned 1) :: Unsigned 2
      b3 = resize (unpack (slice d0 d0 bits) :: Unsigned 1) :: Unsigned 2
      a = b0 + b1 -- 0, 1, or 2
      b = b2 + b3 -- 0, 1, or 2
   in if a >= b
        then resize (pack (a - b))
        else 3329 - resize (pack (b - a))

-- | CBD(eta=3): Convert 6 bits to a coefficient in [-3, 3] mod 3329.
cbd3 :: BitVector 6 -> BitVector 12
cbd3 = cbdDiff6

-- | CBD(eta=3) helper: first 3 bits are a, last 3 bits are b', return a - b' mod 3329.
cbdDiff6 :: BitVector 6 -> BitVector 12
cbdDiff6 bits =
  let b0 = resize (unpack (slice d0 d0 bits) :: Unsigned 1) :: Unsigned 3
      b1 = resize (unpack (slice d1 d1 bits) :: Unsigned 1) :: Unsigned 3
      b2 = resize (unpack (slice d2 d2 bits) :: Unsigned 1) :: Unsigned 3
      b3 = resize (unpack (slice d3 d3 bits) :: Unsigned 1) :: Unsigned 3
      b4 = resize (unpack (slice d4 d4 bits) :: Unsigned 1) :: Unsigned 3
      b5 = resize (unpack (slice d5 d5 bits) :: Unsigned 1) :: Unsigned 3
      a = b0 + b1 + b2 -- 0, 1, 2, or 3
      b = b3 + b4 + b5 -- 0, 1, 2, or 3
   in if a >= b
        then resize (pack (a - b))
        else 3329 - resize (pack (b - a))
