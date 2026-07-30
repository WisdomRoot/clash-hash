{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}

module Component.NTT
  ( topEntity
  , butterfly
  , ntt256
  ) where

import Clash.Prelude

type Coeff = Unsigned 23
type Product = Unsigned 46
type Poly = Vec 256 Coeff
type Zetas = Vec 256 Coeff

q :: Integer
q = 8_380_417

qCoeff :: Coeff
qCoeff = fromInteger q

addModQ :: Coeff -> Coeff -> Coeff
addModQ a b =
  let sumWide :: Unsigned 24
      sumWide = resize a + resize b

      qWide :: Unsigned 24
      qWide = fromInteger q
   in resize
        ( if sumWide >= qWide
            then sumWide - qWide
            else sumWide
        )

subModQ :: Coeff -> Coeff -> Coeff
subModQ a b =
  if a >= b
    then a - b
    else qCoeff - (b - a)

mulModQ :: Coeff -> Coeff -> Coeff
mulModQ a b =
  let productWide :: Product
      productWide = resize a * resize b

      qWide :: Product
      qWide = fromInteger q
   in resize (productWide `mod` qWide)

butterfly :: (Coeff, Coeff, Coeff) -> (Coeff, Coeff)
butterfly (a, b, zeta) =
  let t = mulModQ zeta b
      outA = addModQ a t
      outB = subModQ a t
   in (outA, outB)

ntt256 :: Zetas -> Poly -> Poly
ntt256 zetas input =
  let s128 = nttStage 128 1 zetas input
      s64  = nttStage 64 2 zetas s128
      s32  = nttStage 32 4 zetas s64
      s16  = nttStage 16 8 zetas s32
      s8   = nttStage 8 16 zetas s16
      s4   = nttStage 4 32 zetas s8
      s2   = nttStage 2 64 zetas s4
      s1   = nttStage 1 128 zetas s2
   in s1

nttStage :: Unsigned 9 -> Unsigned 9 -> Zetas -> Poly -> Poly
nttStage len zetaBase zetas input = imap calculateOutput input
  where
    calculateOutput :: Index 256 -> Coeff -> Coeff
    calculateOutput index _ =
      let i :: Unsigned 9
          i = fromIntegral index

          groupSize :: Unsigned 9
          groupSize = 2 * len

          groupIndex :: Unsigned 9
          groupIndex = i `div` groupSize

          positionInGroup :: Unsigned 9
          positionInGroup = i `mod` groupSize

          zetaIndex :: Index 256
          zetaIndex = fromIntegral (zetaBase + groupIndex)

          zeta :: Coeff
          zeta = zetas !! zetaIndex
       in if positionInGroup < len
            then
              let aIndex :: Index 256
                  aIndex = fromIntegral i

                  bIndex :: Index 256
                  bIndex = fromIntegral (i + len)

                  a = input !! aIndex
                  b = input !! bIndex

                  (outA, _) = butterfly (a, b, zeta)
               in outA
            else
              let aIndex :: Index 256
                  aIndex = fromIntegral (i - len)

                  bIndex :: Index 256
                  bIndex = fromIntegral i

                  a = input !! aIndex
                  b = input !! bIndex

                  (_, outB) = butterfly (a, b, zeta)
               in outB

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Zetas, Poly)
  -> Signal System Poly
topEntity _clk _rst _en =
  fmap (\(zetas, input) -> ntt256 zetas input)

{-# ANN topEntity
  (Synthesize
    { t_name = "NTT256"
    , t_inputs =
        [ PortName "clk"
        , PortName "rst"
        , PortName "en"
        , PortProduct
            "input"
            [ PortName "zetas"
            , PortName "poly"
            ]
        ]
    , t_output = PortName "result"
    }) #-}