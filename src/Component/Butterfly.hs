module Component.Butterfly where

import Clash.Prelude
import Component.NTTCore
  ( Coeff
  , butterfly
  )

{-# ANN topEntity
  (Synthesize
    { t_name = "Butterfly"
    , t_inputs =
        [ PortName "clk"
        , PortName "rst"
        , PortName "en"
        , PortProduct ""
            [ PortName "a"
            , PortName "b"
            , PortName "zeta"
            ]
        ]
    , t_output =
        PortProduct ""
          [ PortName "outA"
          , PortName "outB"
          ]
    }) #-}

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Coeff, Coeff, Coeff)
  -> Signal System (Coeff, Coeff)
topEntity _clk _rst _en =
  fmap butterfly