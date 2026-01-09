{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Component.SampleNTT
  ( topEntity,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import Hash.NonPipelined.SHAKE128 qualified as SHAKE128

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
     in bundle (sampleStream, sampleTready)

sampleNTT ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  Signal dom (AXI4Stream 64, Bool) ->
  Signal dom (AXI4Stream 12, Bool)
sampleNTT = mealy step (State 0 0)
  where
    step ::
      State ->
      (AXI4Stream 64, Bool) ->
      (State, (AXI4Stream 12, Bool))
    -- Extract 12-bit chunks from SHAKE128's 64-bit output
    -- Each 64-bit word contains 5 complete 12-bit coefficients (60 bits used, 4 bits unused)
    step (State beat pointer) (input, ready)
      | tvalid input && ready =
          let -- Calculate current coefficient count (0-indexed, so 255 is the 256th coefficient)
              coeffCount :: Index 256
              coeffCount = resize beat * 5 + resize pointer
              isLastCoeff = coeffCount == 255
           in if isLastCoeff
                then
                  -- Output last coefficient and stay in done state
                  let -- Extract 12-bit coefficient at correct position
                      -- pointer 0: [11:0], pointer 1: [23:12], pointer 2: [35:24], pointer 3: [47:36], pointer 4: [59:48]
                      highBit = fromIntegral pointer * 12 + 11
                      lowBit = fromIntegral pointer * 12
                      coeff = slice (SNat @63) (SNat @0) (tdata input) `shiftR` lowBit .&. 0xFFF
                      outStream =
                        AXI4Stream
                          { tdata = resize coeff,
                            tvalid = True,
                            tlast = True
                          }
                   in (State beat pointer, (outStream, pointer /= 4)) -- Don't advance state
                else
                  -- Normal processing
                  let -- Extract 12-bit coefficient at correct position
                      lowBit = fromIntegral pointer * 12
                      coeff = slice (SNat @63) (SNat @0) (tdata input) `shiftR` lowBit .&. 0xFFF
                      -- Move to next pointer/beat
                      (nextBeat, nextPointer) =
                        if pointer == 4
                          then (beat + 1, 0)
                          else (beat, pointer + 1)
                      outStream =
                        AXI4Stream
                          { tdata = resize coeff,
                            tvalid = True,
                            tlast = False
                          }
                   in (State nextBeat nextPointer, (outStream, pointer /= 4))
      | otherwise = (State beat pointer, (idleAXI4Stream, False))

-- mealy step (State 0 0)
-- where
--   step ::
--     State ->
--     (AXI4Stream 64, Bool) ->
--     (State, (AXI4Stream 64, Bool))
--   step (State beat pointer) (input, ready)
--     | tvalid input && ready = undefined
--     | otherwise = (State beat pointer, (idleAXI4Stream, False))

-- -- | Stateful sponge with AXI4-Stream backpressure support.
-- {-# OPAQUE sponge #-}
-- sponge ::
--   forall dom.
--   (HiddenClockResetEnable dom) =>
--   (Index 24 -> BitVector 1600 -> BitVector 1600) ->
--   Signal dom (AXI4Stream 64, Bool, Bool) ->
--   Signal dom (AXI4Stream 64, Bool)
-- sponge permModule = SHAKE128.sponge permModule

-- -- -- | Remaining input after extracting 12 bits for sampling
-- -- data Remainder = Remain0 | Remain4 (BitVector 4) | Remain8 (BitVector 8)
-- --   deriving (Show, Eq, Generic, NFDataX)

data State
  = State
      (Index 256) -- beat
      (Index 16) -- which part of the input is being processed, 0 for [0:11], 1 for [4:15], etc.
  deriving (Show, Eq, Generic, NFDataX)

-- -- sampleNTT :: forall dom. (HiddenClockResetEnable dom) => Signal dom (AXI4Stream 64, Bool) -> Signal dom (AXI4Stream 64, Bool)
-- -- sampleNTT = mealy step (State 0 Remain0 0)
-- --   where
-- --     step ::
-- --       State ->
-- --       (AXI4Stream 64, Bool) ->
-- --       (State, (AXI4Stream 64, Bool))
-- --     step (State beat remainder output) (input, ready)
-- --       | tvalid input && ready = _
-- --       | otherwise = (State beat remainder, (idleAXI4Stream, False))

-- -- -- if tvalid && tready
-- -- --   then
-- -- --     let outStream =
-- -- --           AXI4Stream
-- -- --             { tdata = tdata,
-- -- --               tvalid = True,
-- -- --               tlast = sampleIdx == 255 && beat == 20
-- -- --             }
-- -- --         (nextBeat, nextSampleIdx) =
-- -- --           if beat == 20
-- -- --             then (0, sampleIdx + 1)
-- -- --             else (beat + 1, sampleIdx)
-- -- --      in (SamplingState nextBeat nextSampleIdx, (outStream, True))
-- -- --   else
-- -- --     let outStream =
-- -- --           AXI4Stream
-- -- --             { tdata = 0,
-- -- --               tvalid = False,
-- -- --               tlast = False
-- -- --             }
-- -- --      in (SamplingState beat sampleIdx, (outStream, False))
