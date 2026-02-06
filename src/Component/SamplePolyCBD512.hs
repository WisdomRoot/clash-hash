{-# LANGUAGE TemplateHaskell #-}

module Component.SamplePolyCBD512
  ( topEntity,
  )
where

import AXI4Stream
import Clash.Prelude hiding (tlast)
import Component.SamplePolyCBD.Common (cbd3)
import Permutation qualified
import Sponge.NonPipelined (complementAt)
import TH (mkRead)

-- | Extract 64-bit output words in SHAKE256 normal order.
$(mkRead "squeezeSliceNormal" 1600 [(0, 0, 64), (1, 64, 64), (2, 128, 64), (3, 192, 64), (4, 256, 64), (5, 320, 64), (6, 384, 64), (7, 448, 64), (8, 512, 64), (9, 576, 64), (10, 640, 64), (11, 704, 64), (12, 768, 64), (13, 832, 64), (14, 896, 64), (15, 960, 64), (16, 1024, 64)])


{-# ANN
  topEntity
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
{-# NOINLINE topEntity #-}
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector 264) ->
  Signal System Bool ->
  Signal System (AXI4Stream 12, Bool)
topEntity clk rst en msgSig treadySig =
  withClockResetEnable clk rst en (samplePolyCBD512 msgSig treadySig)

-- | State machine for SamplePolyCBD512
-- PRF(eta=3) outputs 192 bytes = 24 64-bit words
-- This requires two Keccak blocks: 17 words from first, 7 from second
-- Each coefficient uses 6 bits via CBD(eta=3)
-- Total: 256 coefficients
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

samplePolyCBD512 ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  Signal dom (BitVector 264) ->
  Signal dom Bool ->
  Signal dom (AXI4Stream 12, Bool)
samplePolyCBD512 msgSig treadySig = mealy step Absorb (bundle (msgSig, treadySig))
  where
    step ::
      State ->
      (BitVector 264, Bool) ->
      (State, (AXI4Stream 12, Bool))
    step st (msg, tready) =
      case st of
        Absorb ->
          let initState = absorb33Normal msg
           in (Permute 0 0 0 0 0 0 initState, (idleAXI4Stream, True))
        Permute roundIdx coeffIdx wordIdx blockIdx buffer validBits state ->
          let state' = Permutation.keccakF1600 roundIdx state
           in if roundIdx == maxBound
                then (Squeeze coeffIdx wordIdx blockIdx buffer validBits state', (idleAXI4Stream, False))
                else (Permute (roundIdx + 1) coeffIdx wordIdx blockIdx buffer validBits state', (idleAXI4Stream, False))
        Squeeze coeffIdx wordIdx blockIdx buffer validBits state ->
          let -- Can we output a coefficient?
              canOutput = validBits >= 6
              -- Extract bottom 6 bits from buffer
              bits6 = extractBits6Normal buffer
              bits6Rev = pack (reverse (unpack bits6 :: Vec 6 Bit))
              coeffVal = cbd3 bits6Rev
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

              (coeffIdx', buffer', validBits') =
                if canOutput && tready
                  then (coeffIdx + 1, buffer `shiftR` 6, validBits - 6)
                  else (coeffIdx, buffer, validBits)

              needLoad = validBits' <= 64 && wordIdx < 24
              needPermute = wordIdx == 17 && blockIdx == 0 && needLoad
              needLoadFromState = needLoad && not needPermute

              inBlockIdx :: Index 17
              inBlockIdx = fromIntegral (if wordIdx < 17 then wordIdx else wordIdx - 17)

              nextState
                | canOutput && tready && isLast = Done
                | needPermute = Permute 0 coeffIdx' wordIdx 1 buffer' validBits' state
                | needLoadFromState =
                    let word = squeezeSliceNormal state inBlockIdx
                        buffer'' = buffer' .|. (resize word `shiftL` fromIntegral validBits')
                     in Squeeze coeffIdx' (wordIdx + 1) blockIdx buffer'' (validBits' + 64) state
                | otherwise = Squeeze coeffIdx' wordIdx blockIdx buffer' validBits' state
           in (nextState, (outStream, False))
        Done -> (Done, (idleAXI4Stream, False))

-- | Absorb 33 bytes: place message and apply SHAKE256 padding (normal order).
absorb33Normal :: BitVector 264 -> BitVector 1600
absorb33Normal = pad33Bytes . placeMsg
  where
    placeMsg :: BitVector 264 -> BitVector 1600
    placeMsg msg = (0 :: BitVector 1336) ++# msg

    pad33Bytes :: BitVector 1600 -> BitVector 1600
    pad33Bytes =
      complementAt 1087
        . complementAt 264
        . complementAt 265
        . complementAt 266
        . complementAt 267
        . complementAt 268

extractBits6Normal :: BitVector 128 -> BitVector 6
extractBits6Normal = slice (SNat @5) (SNat @0)
