module Component.G
  ( i274o256Stream,
    i274o256,
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

data GPhase
  = GAbsorb
  | GPermute (Index 24)
  | GSqueeze (Index SqueezeBeats)
  deriving (Show, Eq, Generic, NFDataX)

data GState = GState GPhase (BitVector 1600)
  deriving (Show, Eq, Generic, NFDataX)

absorb33 :: BitVector 274 -> BitVector 1600
absorb33 msg274 =
  let msg264 = slice (SNat @263) (SNat @0) msg274
      placed = (0 :: BitVector 1336) ++# msg264
   in complementAt 575 . complementAt 266 . complementAt 265 $ placed

absorbEmpty :: BitVector 1600
absorbEmpty = complementAt 575 . complementAt 2 . complementAt 1 $ (0 :: BitVector 1600)

stepI274 ::
  GState ->
  (AXI4Stream 274, Bool, Bool) ->
  (GState, (AXI4Stream 256, Bool))
stepI274 (GState phase state) (input, tready, flush) =
  case phase of
    GAbsorb ->
      if tvalid input
        then (GState (GPermute 0) (absorb33 (tdata input)), (idleAXI4Stream, False))
        else
          if flush
            then (GState (GPermute 0) absorbEmpty, (idleAXI4Stream, False))
            else (GState GAbsorb state, (idleAXI4Stream, True))
    GPermute roundIdx ->
      let state' = Permutation.keccakF1600 roundIdx state
       in if roundIdx == maxBound
            then
              let out0 = validBeat (Common.squeezeSlice state' 0) False
                  nextPhase = if tready then GSqueeze 1 else GSqueeze 0
               in (GState nextPhase state', (out0, False))
            else (GState (GPermute (roundIdx + 1)) state', (idleAXI4Stream, False))
    GSqueeze idx ->
      let isLast = idx == maxBound
          outStream =
            AXI4Stream
              { tdata = Common.squeezeSlice state idx,
                tvalid = True,
                tlast = isLast
              }
          nextState
            | not tready = GState (GSqueeze idx) state
            | isLast = GState GAbsorb 0
            | otherwise = GState (GSqueeze (idx + 1)) state
       in (nextState, (outStream, False))

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
    let (msgSig, flushSig) = unbundle inputSig
     in mealy stepI274 (GState GAbsorb 0) (bundle (msgSig, treadySig, flushSig))

{-# ANN
  i274o256
  ( Synthesize
      { t_name = "G_I274_O256",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "TREADY",
            PortName "MSG_TDATA",
            PortName "MSG_TVALID",
            PortName "MSG_TLAST",
            PortName "MSG_FLUSH"
          ],
        t_output =
          PortProduct
            ""
            [ PortName "DIGEST_TDATA",
              PortName "DIGEST_TVALID",
              PortName "DIGEST_TLAST",
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
  Signal System Bool ->
  Signal System (BitVector 274) ->
  Signal System Bool ->
  Signal System Bool ->
  Signal System Bool ->
  (Signal System (BitVector 256), Signal System Bool, Signal System Bool, Signal System Bool)
i274o256 clk rst en treadySig msgTdataSig msgTvalidSig msgTlastSig msgFlushSig =
  let msgSig = AXI4Stream <$> msgTdataSig <*> msgTvalidSig <*> msgTlastSig
      outSig = i274o256Stream clk rst en treadySig (bundle (msgSig, msgFlushSig))
      (outStream, msgTreadySig) = unbundle outSig
   in (tdata <$> outStream, tvalid <$> outStream, tlast <$> outStream, msgTreadySig)
