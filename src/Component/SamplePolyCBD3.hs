module Component.SamplePolyCBD3
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
      { t_name = "Component_SamplePolyCBD3",
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
  withClockResetEnable clk rst en (samplePolyCBD3 msgSig treadySig)

-- | State machine for SamplePolyCBD3
-- PRF(eta=3) outputs 192 bytes = 24 64-bit words
-- This requires two Keccak blocks: 17 words from first, 7 from second
-- Each coefficient uses 6 bits via CBD(eta=3)
-- Total: 256 coefficients
--
-- We use a 128-bit buffer to handle 6-bit extraction across word boundaries
data State
  = Absorb
  | Permute
      (Index 24) -- round index
      (Unsigned 9) -- coefficient count (0-256)
      (Unsigned 5) -- word index (0-24)
      (Unsigned 1) -- block index (0 or 1)
      (BitVector 128) -- bit buffer
      (Unsigned 8) -- valid bits in buffer (0-128)
      (BitVector 1600) -- Keccak state
  | Squeeze
      (Unsigned 9) -- coefficient count (0-256)
      (Unsigned 5) -- word index (0-24)
      (Unsigned 1) -- block index (0 or 1)
      (BitVector 128) -- bit buffer
      (Unsigned 8) -- valid bits in buffer (0-128)
      (BitVector 1600) -- Keccak state
  | Done
  deriving (Show, Eq, Generic, NFDataX)

samplePolyCBD3 ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  Signal dom (BitVector 264) ->
  Signal dom Bool ->
  Signal dom (AXI4Stream 12, Bool)
samplePolyCBD3 msgSig treadySig = mealy step Absorb (bundle (msgSig, treadySig))
  where
    step ::
      State ->
      (BitVector 264, Bool) ->
      (State, (AXI4Stream 12, Bool))
    step st (msg, tready) =
      case st of
        Absorb ->
          let initState = absorb33 msg
           in (Permute 0 0 0 0 0 0 initState, (idleAXI4Stream, True))
        Permute roundIdx coeffIdx wordIdx blockIdx buffer validBits state ->
          let state' = Permutation.keccakF1600 roundIdx state
           in if roundIdx == maxBound
                then (Squeeze coeffIdx wordIdx blockIdx buffer validBits state', (idleAXI4Stream, False))
                else (Permute (roundIdx + 1) coeffIdx wordIdx blockIdx buffer validBits state', (idleAXI4Stream, False))
        Squeeze coeffIdx wordIdx blockIdx buffer validBits state ->
          let -- Can we output a coefficient?
              canOutput = validBits >= 6
              -- Extract top 6 bits from buffer
              bits6 = extractTop6 buffer
              coeffVal = cbd3 bits6
              isLast = coeffIdx == 255

              outStream =
                if canOutput
                  then
                    AXI4Stream
                      { tdata = coeffVal,
                        tvalid = True,
                        tlast = isLast
                      }
                  else idleAXI4Stream

              -- Update after potential output
              (coeffIdx', buffer', validBits') =
                if canOutput && tready
                  then (coeffIdx + 1, buffer `shiftL` 6, validBits - 6)
                  else (coeffIdx, buffer, validBits)

              -- Check if we need to load more bits
              needLoad = validBits' <= 64 && wordIdx < 24
              -- Need to permute when entering second block
              needPermute = wordIdx == 17 && blockIdx == 0 && needLoad
              needLoadFromState = needLoad && not needPermute

              -- Calculate in-block index for word extraction
              inBlockIdx :: Index 17
              inBlockIdx = fromIntegral (if wordIdx < 17 then wordIdx else wordIdx - 17)

              -- Determine next state
              nextState
                | canOutput && tready && isLast = Done
                | needPermute =
                    -- Go to Permute for second block
                    Permute 0 coeffIdx' wordIdx 1 buffer' validBits' state
                | needLoadFromState =
                    -- Load next word into buffer
                    let word = squeezeSlice inBlockIdx state
                        shiftAmt = 64 - validBits'
                        buffer'' = buffer' .|. (resize word `shiftL` fromIntegral shiftAmt)
                     in Squeeze coeffIdx' (wordIdx + 1) blockIdx buffer'' (validBits' + 64) state
                | otherwise = Squeeze coeffIdx' wordIdx blockIdx buffer' validBits' state
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

-- | Extract top 6 bits from the 128-bit buffer
extractTop6 :: BitVector 128 -> BitVector 6
extractTop6 buf = slice (SNat @127) (SNat @122) buf

-- | CBD(eta=3): Convert 6 bits to a coefficient in [-3, 3] mod 3329
-- After extraction from MSB side, the 6-bit chunk has:
--   bit 5 = original b0, bit 4 = b1, bit 3 = b2, bit 2 = b3, bit 1 = b4, bit 0 = b5
-- Formula:
--   a = popcount(b2, b1, b0) = b0 + b1 + b2  (values 0, 1, 2, or 3)
--   b = popcount(b5, b4, b3) = b3 + b4 + b5  (values 0, 1, 2, or 3)
--   result = (a - b) mod 3329
cbd3 :: BitVector 6 -> BitVector 12
cbd3 bits =
  let b0 = resize (unpack (slice d5 d5 bits) :: Unsigned 1) :: Unsigned 3
      b1 = resize (unpack (slice d4 d4 bits) :: Unsigned 1) :: Unsigned 3
      b2 = resize (unpack (slice d3 d3 bits) :: Unsigned 1) :: Unsigned 3
      b3 = resize (unpack (slice d2 d2 bits) :: Unsigned 1) :: Unsigned 3
      b4 = resize (unpack (slice d1 d1 bits) :: Unsigned 1) :: Unsigned 3
      b5 = resize (unpack (slice d0 d0 bits) :: Unsigned 1) :: Unsigned 3
      a = b0 + b1 + b2 -- 0, 1, 2, or 3
      b = b3 + b4 + b5 -- 0, 1, 2, or 3
   in if a >= b
        then resize (pack (a - b))
        else 3329 - resize (pack (b - a))
