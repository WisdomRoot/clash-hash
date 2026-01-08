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
  Signal System (AXI4Stream 64, Bool)
topEntity clk rst en treadySig inputSig =
  withClockResetEnable clk rst en $
    let hashInput = bundle (inputSig, treadySig, pure False)
     in sampleNTT (SHAKE128.hash hashInput)

sampleNTT ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  Signal dom (AXI4Stream 64, Bool) ->
  Signal dom (AXI4Stream 64, Bool)
sampleNTT = id
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
