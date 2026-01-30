module Component.PRF.Common
  ( hash,
    Eta (..),
  )
where

import AXI4Stream
import Clash.Prelude hiding (tlast)
import Permutation qualified
import Sponge.NonPipelined (complementAt)

-- | Eta parameter for PRF: 2 or 3.
data Eta = Eta2 | Eta3 deriving (Show, Eq, Generic, NFDataX)

data State
  = Absorb
  | Permute (Index 24) (Unsigned 5) (BitVector 1600)
  | Squeeze (Unsigned 5) (BitVector 1600)
  | Done
  deriving (Show, Eq, Generic, NFDataX)

hash ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  Eta ->
  Signal dom (BitVector 264) ->
  Signal dom Bool ->
  Signal dom (AXI4Stream 64, Bool)
hash eta msgSig treadySig = mealy step Absorb (bundle (msgSig, treadySig))
  where
    step ::
      State ->
      (BitVector 264, Bool) ->
      (State, (AXI4Stream 64, Bool))
    step st (msg, tready) =
      case st of
        Absorb ->
          let initState = absorb33 msg
           in (Permute 0 0 initState, (idleAXI4Stream, True))
        Permute roundIdx wordIdx state ->
          let state' = Permutation.keccakF1600Reversed roundIdx state
           in if roundIdx == maxBound
                then (Squeeze wordIdx state', (idleAXI4Stream, False))
                else (Permute (roundIdx + 1) wordIdx state', (idleAXI4Stream, False))
        Squeeze wordIdx state ->
          let lastWord = wordIdx == (if eta == Eta3 then 23 else 15)
              inBlockIdx = fromIntegral (wordIdx `mod` 17) :: Index 17
              outStream =
                AXI4Stream
                  { tdata = squeezeSlice inBlockIdx state,
                    tvalid = True,
                    tlast = lastWord
                  }
              nextState =
                if tready
                  then
                    if lastWord
                      then Done
                      else
                        let nextWord = wordIdx + 1
                         in if inBlockIdx == 16
                              then Permute 0 nextWord state
                              else Squeeze nextWord state
                  else Squeeze wordIdx state
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
