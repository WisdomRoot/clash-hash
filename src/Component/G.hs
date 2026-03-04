{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Component.G
  ( sponge,
    i274o256Stream,
    i274o256,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import Parameter
import Permutation qualified
import Sponge.NonPipelinedN256
import Sponge.XOR qualified as XOR
import TH (mkRead)

type PadBeats = 3

type SqueezeBeats = 2

-- | Padding function + XOR, flips 3 bits depending on the current beatCounter.
-- | k is encoded by which bit near the (k || 0x1F) byte is flipped in the first beat:
-- | k=2 -> flip bit 257; k=3 -> flip bit 256; k=4 -> flip bit 258.
pad512 :: Index PadBeats -> BitVector 1600 -> BitVector 1600
pad512 0 = complementAt 575 . complementAt 266 . complementAt 265 . complementAt 257
pad512 1 = complementAt 575 . complementAt 514 . complementAt 513
pad512 _ = complementAt 575 . complementAt 2 . complementAt 1 -- special case for a whole 576-bit padding

-- | Padding function + XOR for k = 3.
pad768 :: Index PadBeats -> BitVector 1600 -> BitVector 1600
pad768 0 = complementAt 575 . complementAt 266 . complementAt 265 . complementAt 257 . complementAt 256
pad768 1 = complementAt 575 . complementAt 514 . complementAt 513
pad768 _ = complementAt 575 . complementAt 2 . complementAt 1 -- special case for a whole 576-bit padding

-- | Padding function + XOR for k = 4.
pad1024 :: Index PadBeats -> BitVector 1600 -> BitVector 1600
pad1024 0 = complementAt 575 . complementAt 266 . complementAt 265 . complementAt 258
pad1024 1 = complementAt 575 . complementAt 514 . complementAt 513
pad1024 _ = complementAt 575 . complementAt 2 . complementAt 1 -- special case for a whole 576-bit padding

-- | Squeeze phase bit slicing helper: extracts 256-bit chunks from the Keccak state.
$( mkRead
     "squeezeSlice"
     1600
     [ (0, 0, 256),
       (1, 256, 256)
     ]
 )

{-# INLINE squeezeSlice #-}

-- | Stateful sponge with AXI4-Stream backpressure support.
{-# OPAQUE sponge #-}
sponge ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  MLKEM ->
  (Index 24 -> BitVector 1600 -> BitVector 1600) -> -- Permutation function
  Signal dom (AXI4Stream 256, Bool, Bool) -> -- Input message, output tready, flush signal
  Signal dom (AXI4Stream 256, Bool) -- Output digest (AXI4-Stream), input tready
sponge mlkem permModule = mealy step (State (Absorb 0) 0)
  where
    padFn = case mlkem of
      MLKEM512 -> pad512
      MLKEM768 -> pad768
      MLKEM1024 -> pad1024
    step ::
      State PadBeats (Index SqueezeBeats) ->
      (AXI4Stream 256, Bool, Bool) ->
      (State PadBeats (Index SqueezeBeats), (AXI4Stream 256, Bool))
    step (State (Absorb counter) state) (input, _tready, flush) = absorb padFn XOR.staticXOR512_256 counter state input flush
    step (State (Permute counter seenTLAST) state) (_msg, tready, _flush) = permute permModule padFn (`squeezeSlice` 0) counter seenTLAST state tready
    step (State (Squeeze counter) state) (_msg, tready, _flush) = squeeze squeezeSlice counter state tready

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
              let out0 = validBeat (squeezeSlice state' 0) False
                  nextPhase = if tready then GSqueeze 1 else GSqueeze 0
               in (GState nextPhase state', (out0, False))
            else (GState (GPermute (roundIdx + 1)) state', (idleAXI4Stream, False))
    GSqueeze idx ->
      let isLast = idx == maxBound
          outStream =
            AXI4Stream
              { tdata = squeezeSlice state idx,
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
