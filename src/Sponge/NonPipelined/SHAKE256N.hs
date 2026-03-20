{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Sponge.NonPipelined.SHAKE256N
  ( sponge,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import Sponge.NonPipelinedN
import Sponge.XOR qualified as XOR
import TH (mkRead)

type RateBeats = 17
type PadBeats = 17

-- | Padding function + XOR, normal-order version for SHAKE256.
pad :: Index PadBeats -> BitVector 1600 -> BitVector 1600
pad 0 =
  complementAt 1087
    . complementAt 68
    . complementAt 67
    . complementAt 66
    . complementAt 65
    . complementAt 64
pad 1 =
  complementAt 1087
    . complementAt 132
    . complementAt 131
    . complementAt 130
    . complementAt 129
    . complementAt 128
pad 2 =
  complementAt 1087
    . complementAt 196
    . complementAt 195
    . complementAt 194
    . complementAt 193
    . complementAt 192
pad 3 =
  complementAt 1087
    . complementAt 260
    . complementAt 259
    . complementAt 258
    . complementAt 257
    . complementAt 256
pad 4 =
  complementAt 1087
    . complementAt 324
    . complementAt 323
    . complementAt 322
    . complementAt 321
    . complementAt 320
pad 5 =
  complementAt 1087
    . complementAt 388
    . complementAt 387
    . complementAt 386
    . complementAt 385
    . complementAt 384
pad 6 =
  complementAt 1087
    . complementAt 452
    . complementAt 451
    . complementAt 450
    . complementAt 449
    . complementAt 448
pad 7 =
  complementAt 1087
    . complementAt 516
    . complementAt 515
    . complementAt 514
    . complementAt 513
    . complementAt 512
pad 8 =
  complementAt 1087
    . complementAt 580
    . complementAt 579
    . complementAt 578
    . complementAt 577
    . complementAt 576
pad 9 =
  complementAt 1087
    . complementAt 644
    . complementAt 643
    . complementAt 642
    . complementAt 641
    . complementAt 640
pad 10 =
  complementAt 1087
    . complementAt 708
    . complementAt 707
    . complementAt 706
    . complementAt 705
    . complementAt 704
pad 11 =
  complementAt 1087
    . complementAt 772
    . complementAt 771
    . complementAt 770
    . complementAt 769
    . complementAt 768
pad 12 =
  complementAt 1087
    . complementAt 836
    . complementAt 835
    . complementAt 834
    . complementAt 833
    . complementAt 832
pad 13 =
  complementAt 1087
    . complementAt 900
    . complementAt 899
    . complementAt 898
    . complementAt 897
    . complementAt 896
pad 14 =
  complementAt 1087
    . complementAt 964
    . complementAt 963
    . complementAt 962
    . complementAt 961
    . complementAt 960
pad 15 =
  complementAt 1087
    . complementAt 1028
    . complementAt 1027
    . complementAt 1026
    . complementAt 1025
    . complementAt 1024
pad _ =
  complementAt 1087
    . complementAt 4
    . complementAt 3
    . complementAt 2
    . complementAt 1
    . complementAt 0

$(mkRead "squeezeSlice" 1600 [(i, i * 64, 64) | i <- [0 .. 16]])

{-# OPAQUE sponge #-}
sponge ::
  forall dom n.
  ( HiddenClockResetEnable dom,
    KnownNat n,
    n ~ DivRU (MsgBits + 2) 1088,
    MsgBits + 2 <= n * 1088,
    MsgBits + 4 <= n * 1088
  ) =>
  (Index 24 -> BitVector 1600 -> BitVector 1600) ->
  Signal dom (AXI4Stream MsgBits, Bool, Bool) ->
  Signal dom (AXI4Stream DigestBits, Bool)
sponge permModule = mealy step (State (Absorb 0) 0)
  where
    step ::
      State PadBeats (Index RateBeats) ->
      (AXI4Stream MsgBits, Bool, Bool) ->
      (State PadBeats (Index RateBeats), (AXI4Stream DigestBits, Bool))
    step (State (Absorb counter) state) (input, _tready, flush) = absorb pad XOR.staticXOR256' counter state input flush
    step (State (Permute counter seenTLAST) state) (_msg, tready, _flush) = permute permModule pad counter seenTLAST state tready
    step (State (Squeeze counter) state) (_msg, tready, _flush)
      | counter == maxBound =
          let outStream = AXI4Stream {tdata = squeezeSlice state counter, tvalid = True, tlast = False}
              nextState =
                if tready
                  then State (Permute 0 SeenTLASTAndPadded) state
                  else State (Squeeze maxBound) state
           in (nextState, (outStream, False))
      | otherwise =
          let outStream = AXI4Stream {tdata = squeezeSlice state counter, tvalid = True, tlast = False}
              nextState =
                if tready
                  then State (Squeeze (counter + 1)) state
                  else State (Squeeze counter) state
           in (nextState, (outStream, False))
