module Component.G
  ( i274o256,
    i274o256Stream,
  )
where

import AXI4Stream
import Clash.Prelude hiding (tlast)
import Component.G.Common qualified as Common
import Permutation qualified
import Sponge.NonPipelinedN256 (complementAt)

type SqueezeBeats = Common.SqueezeBeats

--------------------------------------------------------------------------------
-- General G (explicit k in input): i274o256
--------------------------------------------------------------------------------

data Phase
  = Absorb
  | Permute (Index 24)
  | Squeeze (Index SqueezeBeats)
  deriving (Show, Eq, Generic, NFDataX)

data State = State Phase (BitVector 1600)
  deriving (Show, Eq, Generic, NFDataX)

absorb33 :: BitVector 274 -> BitVector 1600
absorb33 msg274 =
  let msg264 = slice (SNat @263) (SNat @0) msg274
      placed = (0 :: BitVector 1336) ++# msg264
   in complementAt 575 . complementAt 266 . complementAt 265 $ placed

stepI274 ::
  State ->
  (Bool, AXI4Stream 274) ->
  (State, (Bool, AXI4Stream 256))
stepI274 (State phase state) (outReady, input) =
  case phase of
    Absorb ->
      if tvalid input
        then (State (Permute 0) (absorb33 (tdata input)), (False, idleAXI4Stream))
        else (State Absorb state, (True, idleAXI4Stream))
    Permute roundIdx ->
      let state' = Permutation.keccakF1600 roundIdx state
       in if roundIdx == maxBound
            then
              let out0 = validBeat (Common.squeezeSlice state' 0) False
                  nextPhase = if outReady then Squeeze 1 else Squeeze 0
               in (State nextPhase state', (False, out0))
            else (State (Permute (roundIdx + 1)) state', (False, idleAXI4Stream))
    Squeeze idx ->
      let isLast = idx == maxBound
          outStream =
            AXI4Stream
              { tdata = Common.squeezeSlice state idx,
                tvalid = True,
                tlast = isLast
              }
          nextState
            | not outReady = State (Squeeze idx) state
            | isLast = State Absorb 0
            | otherwise = State (Squeeze (idx + 1)) state
       in (nextState, (False, outStream))

{-# ANN
  i274o256
  ( Synthesize
      { t_name = "dut",
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
{-# NOINLINE i274o256 #-}
i274o256 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 274, Bool) ->
  Signal System (AXI4Stream 256, Bool)
i274o256 = toDUT i274o256Core

i274o256Core ::
  HiddenClockResetEnable dom =>
  Pipe dom 274 256
i274o256Core (outReady, inStream) =
  mealyB stepI274 (State Absorb 0) (outReady, inStream)

{-# NOINLINE i274o256Stream #-}
i274o256Stream ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool ->
  Signal System (AXI4Stream 274, Bool) ->
  Signal System (AXI4Stream 256, Bool)
i274o256Stream clk rst en treadySig inputSig =
  withClockResetEnable clk rst en $
    let (msgSig, _flushSig) = unbundle inputSig
        (inReadySig, outStreamSig) = mealyB stepI274 (State Absorb 0) (treadySig, msgSig)
     in bundle (outStreamSig, inReadySig)
