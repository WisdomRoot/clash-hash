{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Sponge.NonPipelined.SampleNTT
  ( sponge,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import Sponge.NonPipelined
import Sponge.NonPipelined.SHAKE128 qualified as SHAKE128
import Sponge.XOR qualified as XOR

type RateBeats = 21

type PadBeats = 21

-- | Stateful sponge with AXI4-Stream backpressure support.
{-# OPAQUE sponge #-}
sponge ::
  forall dom n.
  ( HiddenClockResetEnable dom,
    KnownNat n,
    n ~ DivRU (MsgBits + 2) 1344,
    MsgBits + 2 <= n * 1344,
    MsgBits + 4 <= n * 1344
  ) =>
  (Index 24 -> BitVector 1600 -> BitVector 1600) ->
  Signal dom (AXI4Stream MsgBits, Bool, Bool) ->
  Signal dom (AXI4Stream DigestBits, Bool)
sponge permModule = mealy step (State (Absorb 0) 0)
  where
    step ::
      State PadBeats RateBeats ->
      (AXI4Stream MsgBits, Bool, Bool) ->
      (State PadBeats RateBeats, (AXI4Stream DigestBits, Bool))
    step (State (Absorb counter) state) (input, _tready, flush) = absorb SHAKE128.pad XOR.staticXOR128 counter state input flush
    step (State (Permute counter seenTLAST) state) (_msg, tready, _flush) = permute permModule SHAKE128.pad counter seenTLAST state tready
    step (State (Squeeze counter) state) (_msg, tready, _flush)
      | counter == maxBound =
          let outStream = AXI4Stream {tdata = SHAKE128.squeezeSlice counter state, tvalid = True, tlast = False}
              nextState =
                if tready
                  then State (Permute 0 SeenTLASTAndPadded) state
                  else State (Squeeze maxBound) state
           in (nextState, (outStream, False))
      | otherwise =
          let outStream = AXI4Stream {tdata = SHAKE128.squeezeSlice counter state, tvalid = True, tlast = False}
              nextState =
                if tready
                  then State (Squeeze (counter + 1)) state
                  else State (Squeeze counter) state
           in (nextState, (outStream, False))
