{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Component.SampleNTT
  ( topEntity,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import Debug.Trace (trace)
import Hash.NonPipelined.SHAKE128 qualified as SHAKE128
import Numeric (showHex)
import Prelude qualified as P

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "Component_SampleNTT",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "TREADY",
            PortProduct
              "MSG"
              [ PortName "MSG_TDATA",
                PortName "MSG_TVALID",
                PortName "MSG_TLAST"
              ]
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
  Signal System Bool ->
  Signal System (AXI4Stream 64) ->
  Signal System (AXI4Stream 12, Bool)
topEntity clk rst en treadySig inputSig =
  withClockResetEnable clk rst en $
    let (shakeStream, _shakeTready) = unbundle $ SHAKE128.hash (bundle (inputSig, sampleTready, pure False))
        (sampleStream, sampleTready) = unbundle $ sampleNTT (bundle (shakeStream, treadySig))
     in bundle (sampleStream, treadySig)

sampleNTT ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  Signal dom (AXI4Stream 64, Bool) ->
  Signal dom (AXI4Stream 12, Bool)
sampleNTT = mealy step (State 0 0 False 0)
  where
    step ::
      State ->
      (AXI4Stream 64, Bool) ->
      (State, (AXI4Stream 12, Bool))
    -- Extract 12-bit chunks from SHAKE128's 64-bit output.
    -- Each 64-bit word contains 5 complete 12-bit coefficients (60 bits used, 4 bits unused).
    -- IMPORTANT: latch a 64-bit word and emit all 5 slices before accepting the next word.
    step (State beat pointer haveWord wordReg) (input, ready) =
      let inValid = tvalid input
          inWord = tdata input
          inReady = not haveWord
          inputHandshake = inValid && inReady

          outValid = haveWord || inValid
          outWord = if haveWord then wordReg else inWord
          outputHandshake = outValid && ready

          -- Calculate current coefficient count (0-indexed, so 255 is the 256th coefficient)
          coeffCount :: Index 256
          coeffCount = resize beat * 5 + resize pointer
          isLastCoeff = coeffCount == 255

          -- Convert the 64-bit word to bytes in the same order as the SHAKE test harness:
          --   byte0 bit0 = word bit63, byte0 bit7 = word bit56, byte1 bit0 = word bit55, etc.
          bytes = bytesFromWord outWord

          coeffU :: Unsigned 12
          coeffU = coeffFromBytes bytes pointer
          coeff12 :: BitVector 12
          coeff12 = pack coeffU
          bytesU :: Vec 8 (Unsigned 8)
          bytesU = map (unpack :: BitVector 8 -> Unsigned 8) bytes
          traceBytes = outputHandshake && pointer == 0 && beat == 0
          tracedCoeff =
            if traceBytes
              then
                trace
                  ( "SHAKE tdata: 0x"
                      <> showHexWord64 outWord
                      <> " bytes: "
                      <> show (toList bytesU)
                  )
                  coeff12
              else trace ("SampleNTT raw12: " <> show coeffU) coeff12

          outStream =
            if outValid
              then
                AXI4Stream
                  { tdata = tracedCoeff,
                    tvalid = True,
                    tlast = isLastCoeff
                  }
              else idleAXI4Stream

          -- Capture a new word if we're ready and it is valid.
          wordReg' = if inputHandshake then inWord else wordReg
          have' = haveWord || inputHandshake

          -- Then, advance pointer/beat only when the output handshake completes.
          nextState =
            if outputHandshake
              then
                if pointer == 4
                  then State (beat + 1) 0 False wordReg'
                  else State beat (pointer + 1) True wordReg'
              else State beat pointer have' wordReg'
       in (nextState, (outStream, inReady))

data State
  = State
      (Index 256) -- beat
      (Index 16) -- slice index within a 64-bit word (0..4)
      Bool -- have a buffered 64-bit word
      (BitVector 64) -- buffered 64-bit word
  deriving (Show, Eq, Generic, NFDataX)

toU12 :: BitVector 8 -> Unsigned 12
toU12 b = resize (unpack b :: Unsigned 8)

bytesFromWord :: BitVector 64 -> Vec 8 (BitVector 8)
bytesFromWord w = map (byteFromWord w) indicesI

byteFromWord :: BitVector 64 -> Index 8 -> BitVector 8
byteFromWord w k =
  let base = fromIntegral k * 8
      bits :: Vec 8 Bit
      bits = map (\i -> boolToBit (testBit w (63 - (base + fromIntegral i)))) indicesI
   in pack bits

coeffFromBytes :: Vec 8 (BitVector 8) -> Index 16 -> Unsigned 12
coeffFromBytes (b0 :> b1 :> b2 :> b3 :> b4 :> b5 :> b6 :> b7 :> Nil) pointer =
  let d1' x y = toU12 x + shiftL (resize (unpack (y .&. 0x0F) :: Unsigned 8)) 8
      d2' x y = resize (unpack (x `shiftR` 4) :: Unsigned 8) + shiftL (toU12 y) 4
   in case pointer of
        0 -> d1' b0 b1
        1 -> d2' b1 b2
        2 -> d1' b3 b4
        3 -> d2' b4 b5
        4 -> d1' b6 b7
        _ -> 0

showHexWord64 :: BitVector 64 -> String
showHexWord64 w =
  let u = unpack w :: Unsigned 64
      hex = showHex u ""
      pad = P.replicate (16 - P.length hex) '0'
   in pad <> hex
