{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}

module Component.NTT
  ( topEntity
  , butterfly1
  , butterfly2
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

butterfly1 :: (Coeff, Coeff, Coeff) -> (Coeff, Coeff)
butterfly1 (a, b, zeta) =
  let t = mulModQ zeta b in (addModQ a t, subModQ a t)

butterfly2 :: ((Coeff, Coeff, Coeff),(Coeff, Coeff, Coeff)) -> ((Coeff, Coeff),(Coeff,Coeff))
butterfly2 ((a0,b0,z0),(a1,b1,z1)) = (butterfly1 (a0,b0,z0), butterfly1 (a1,b1,z1))

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System ((Coeff, Coeff, Coeff),(Coeff, Coeff, Coeff))
  -> Signal System ((Coeff, Coeff),(Coeff,Coeff))
topEntity _clk _rst _en =
  fmap butterfly2

{-# ANN topEntity
  (Synthesize
    { t_name = "NTT2Butterfly"
    , t_inputs =
        [ PortName "clk"
        , PortName "rst"
        , PortName "en"
        , PortProduct
            "input"
            [ PortProduct
                "butterfly0"
                [ PortName "a0"
                , PortName "b0"
                , PortName "zeta0"
                ]
            , PortProduct
                "butterfly1"
                [ PortName "a1"
                , PortName "b1"
                , PortName "zeta1"
                ]
            ]
        ]
    , t_output =
        PortProduct
          "output"
          [ PortProduct
              "butterfly0"
              [ PortName "outA0"
              , PortName "outB0"
              ]
          , PortProduct
              "butterfly1"
              [ PortName "outA1"
              , PortName "outB1"
              ]
          ]
    }) #-}