module Component.SampleNTT.Common
  ( absorb34,
    screenCoeff96,
  )
where

import Clash.Prelude
import Sponge.NonPipelined (complementAt)

screenCoeff96 :: BitVector 96 -> Vec 8 (Bool, BitVector 12)
screenCoeff96 chunk =
  let c0 = slice (SNat @11) (SNat @0) chunk
      c1 = slice (SNat @23) (SNat @12) chunk
      c2 = slice (SNat @35) (SNat @24) chunk
      c3 = slice (SNat @47) (SNat @36) chunk
      c4 = slice (SNat @59) (SNat @48) chunk
      c5 = slice (SNat @71) (SNat @60) chunk
      c6 = slice (SNat @83) (SNat @72) chunk
      c7 = slice (SNat @95) (SNat @84) chunk
   in (c0 < 3329, c0)
        :> (c1 < 3329, c1)
        :> (c2 < 3329, c2)
        :> (c3 < 3329, c3)
        :> (c4 < 3329, c4)
        :> (c5 < 3329, c5)
        :> (c6 < 3329, c6)
        :> (c7 < 3329, c7)
        :> Nil
{-# INLINE screenCoeff96 #-}

-- | Absorb 34 bytes: place message and apply SHAKE padding.
absorb34 :: BitVector 272 -> BitVector 1600
absorb34 = pad34Bytes . placeMsg
  where
    placeMsg :: BitVector 272 -> BitVector 1600
    placeMsg msg = (0 :: BitVector 1328) ++# msg

    pad34Bytes :: BitVector 1600 -> BitVector 1600
    pad34Bytes =
      complementAt 1343
        . complementAt 272
        . complementAt 273
        . complementAt 274
        . complementAt 275
        . complementAt 276
