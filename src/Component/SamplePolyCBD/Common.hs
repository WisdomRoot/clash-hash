module Component.SamplePolyCBD.Common
  ( absorb33,
    cbd2,
    cbd3,
  )
where

import Clash.Prelude
import Sponge.NonPipelined (complementAt)

-- | Absorb 33 bytes: place message into the first 5 beats and apply SHAKE256 padding.
absorb33 :: BitVector 264 -> BitVector 1600
absorb33 = pad33Bytes . placeMsg
  where
    placeMsg :: BitVector 264 -> BitVector 1600
    placeMsg msg = (0 :: BitVector 1336) ++# msg

    pad33Bytes :: BitVector 1600 -> BitVector 1600
    pad33Bytes =
      complementAt 1087
        . complementAt 264
        . complementAt 265
        . complementAt 266
        . complementAt 267
        . complementAt 268

-- | CBD(eta=2): Convert 4 bits to a coefficient in [-2, 2] mod 3329.
cbd2 :: BitVector 4 -> BitVector 12
cbd2 bits =
  let b0 = resize (unpack (slice d0 d0 bits) :: Unsigned 1) :: Unsigned 2
      b1 = resize (unpack (slice d1 d1 bits) :: Unsigned 1) :: Unsigned 2
      b2 = resize (unpack (slice d2 d2 bits) :: Unsigned 1) :: Unsigned 2
      b3 = resize (unpack (slice d3 d3 bits) :: Unsigned 1) :: Unsigned 2
      a = b0 + b1 -- 0, 1, or 2
      b = b2 + b3 -- 0, 1, or 2
   in if a >= b
        then resize (pack (a - b))
        else 3329 - resize (pack (b - a))

-- | CBD(eta=3): Convert 6 bits to a coefficient in [-3, 3] mod 3329.
cbd3 :: BitVector 6 -> BitVector 12
cbd3 bits =
  let b0 = resize (unpack (slice d0 d0 bits) :: Unsigned 1) :: Unsigned 3
      b1 = resize (unpack (slice d1 d1 bits) :: Unsigned 1) :: Unsigned 3
      b2 = resize (unpack (slice d2 d2 bits) :: Unsigned 1) :: Unsigned 3
      b3 = resize (unpack (slice d3 d3 bits) :: Unsigned 1) :: Unsigned 3
      b4 = resize (unpack (slice d4 d4 bits) :: Unsigned 1) :: Unsigned 3
      b5 = resize (unpack (slice d5 d5 bits) :: Unsigned 1) :: Unsigned 3
      a = b0 + b1 + b2 -- 0, 1, 2, or 3
      b = b3 + b4 + b5 -- 0, 1, 2, or 3
   in if a >= b
        then resize (pack (a - b))
        else 3329 - resize (pack (b - a))
