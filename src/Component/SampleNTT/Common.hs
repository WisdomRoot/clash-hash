module Component.SampleNTT.Common
  ( absorb34,
  )
where

import Clash.Prelude
import Sponge.NonPipelined (complementAt)

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
