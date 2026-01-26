module Component.SamplePolyCBD2
  ( topEntity,
  )
where

import AXI4Stream
import Clash.Prelude hiding (tlast)
import Permutation qualified
import Sponge.NonPipelined (complementAt)

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "Component_SamplePolyCBD2",
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
{-# NOINLINE topEntity #-}
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector 264) ->
  Signal System Bool ->
  Signal System (AXI4Stream 12, Bool)
topEntity clk rst en msgSig treadySig =
  withClockResetEnable clk rst en (samplePolyCBD2 msgSig treadySig)

-- | State machine for SamplePolyCBD2
-- PRF(eta=2) outputs 128 bytes = 16 64-bit words
-- Each 64-bit word produces 16 coefficients via CBD(eta=2)
-- Total: 16 * 16 = 256 coefficients
data State
  = Absorb
  | Permute (Index 24) (BitVector 1600)
  | Squeeze (Index 16) (Index 16) (BitVector 1600) -- wordIdx, coeffIdx within word, state
  | Done
  deriving (Show, Eq, Generic, NFDataX)

samplePolyCBD2 ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  Signal dom (BitVector 264) ->
  Signal dom Bool ->
  Signal dom (AXI4Stream 12, Bool)
samplePolyCBD2 msgSig treadySig = mealy step Absorb (bundle (msgSig, treadySig))
  where
    step ::
      State ->
      (BitVector 264, Bool) ->
      (State, (AXI4Stream 12, Bool))
    step st (msg, tready) =
      case st of
        Absorb ->
          let initState = absorb33 msg
           in (Permute 0 initState, (idleAXI4Stream, True))
        Permute roundIdx state ->
          let state' = Permutation.keccakF1600 roundIdx state
           in if roundIdx == maxBound
                then (Squeeze 0 0 state', (idleAXI4Stream, False))
                else (Permute (roundIdx + 1) state', (idleAXI4Stream, False))
        Squeeze wordIdx coeffIdx state ->
          let -- Extract 64-bit word from state
              word64 = squeezeSlice wordIdx state
              -- Extract 4-bit chunk for this coefficient
              bits4 = extractBits4 coeffIdx word64
              -- Apply CBD(eta=2)
              coeff = cbd2 bits4
              -- Check if this is the last coefficient (wordIdx=15, coeffIdx=15)
              isLast = wordIdx == 15 && coeffIdx == 15
              outStream =
                AXI4Stream
                  { tdata = coeff,
                    tvalid = True,
                    tlast = isLast
                  }
              nextState =
                if tready
                  then
                    if isLast
                      then Done
                      else
                        if coeffIdx == 15
                          then Squeeze (wordIdx + 1) 0 state
                          else Squeeze wordIdx (coeffIdx + 1) state
                  else Squeeze wordIdx coeffIdx state
           in (nextState, (outStream, False))
        Done -> (Done, (idleAXI4Stream, False))

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
squeezeSlice :: Index 16 -> BitVector 1600 -> BitVector 64
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
squeezeSlice _ state = slice (SNat @639) (SNat @576) state

-- | Extract 4-bit chunk from a 64-bit word based on coefficient index (0-15)
-- Based on PRF test harness wordToBits which outputs bits MSB-first.
-- Byte 0 uses bits 63:56, and its lower nibble (coeff 0) is bits 63:60.
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

-- | CBD(eta=2): Convert 4 bits to a coefficient in [-2, 2] mod 3329
-- After extraction from MSB side, the 4-bit chunk has:
--   bit 3 = original b0, bit 2 = b1, bit 1 = b2, bit 0 = b3
-- Formula:
--   a = popcount(b1, b0) = b0 + b1  (values 0, 1, or 2)
--   b = popcount(b3, b2) = b2 + b3  (values 0, 1, or 2)
--   result = (a - b) mod 3329
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
