{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}

module Component.NTT
  ( topEntity
  , butterfly
  ) where

import Clash.Prelude

type Coeff = Unsigned 23

type Product = Unsigned 46

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
   in resize $
        if sumWide >= qWide
          then sumWide - qWide
          else sumWide

subModQ :: Coeff -> Coeff -> Coeff
subModQ a b =
  if a >= b
    then a - b
    else qCoeff - (b - a)

mulModQ :: Coeff -> Coeff -> Coeff
mulModQ a b =
  let productWide :: Product
      productWide = resize a * resize b
   in resize (productWide `mod` fromInteger q)

butterfly :: (Coeff, Coeff, Coeff) -> (Coeff, Coeff)
butterfly (a, b, zeta) =
  let t = mulModQ zeta b in (addModQ a t, subModQ a t)

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Coeff, Coeff, Coeff)
  -> Signal System (Coeff, Coeff)
topEntity _clk _rst _en =
  fmap butterfly

{-# ANN topEntity
  (Synthesize
    { t_name = "NTT"
    , t_inputs =
        [ PortName "clk"
        , PortName "rst"
        , PortName "en"
        , PortProduct
            "input"
            [ PortName "a"
            , PortName "b"
            , PortName "zeta"
            ]
        ]
    , t_output =
        PortProduct
          "output"
          [ PortName "outA"
          , PortName "outB"
          ]
    }) #-}