-- src/Component/NTTCore.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}

module Component.NTTCore
  ( Coeff
  , Product
  , butterfly
  , montgomeryMul
  ) where

import Clash.Prelude

type Coeff = Unsigned 23
type Product = Unsigned 46

type MontWord = Unsigned 24
type MontWide = Unsigned 48

qCoeff :: Coeff
qCoeff = 8_380_417

qMont :: MontWord
qMont = 8_380_417

-- -q^(-1) mod 2^24
qInv :: MontWord
qInv = 8_380_415

montgomeryReduce :: Product -> Coeff
montgomeryReduce x =
  let
    xLow :: MontWord
    xLow = truncateB x

    mIntermediate :: Unsigned 47
    mIntermediate =
      (resize xLow `shiftL` 23)
        - (resize xLow `shiftL` 13)
        - resize xLow

    m :: MontWord
    m = truncateB mIntermediate

    mWide :: MontWide
    mWide = resize m

    mq :: MontWide
    mq =
      (mWide `shiftL` 23)
        - (mWide `shiftL` 13)
        + mWide

    sumWide :: Unsigned 49
    sumWide = resize x + resize mq

    shifted :: Unsigned 25
    shifted = truncateB (shiftR sumWide 24)

    qWide :: Unsigned 25
    qWide = 8_380_417

    reduced :: Unsigned 25
    reduced =
      if shifted >= qWide
        then shifted - qWide
        else shifted
  in
    truncateB reduced

montgomeryMul :: Coeff -> Coeff -> Coeff
montgomeryMul a b =
  -- 23 × 23 -> 46 bits.
  montgomeryReduce (a `mul` b)

addModQ :: Coeff -> Coeff -> Coeff
addModQ a b =
  let
    sumWide :: MontWord
    sumWide = resize a + resize b

    qWide :: MontWord
    qWide = resize qCoeff
  in
    if sumWide >= qWide
      then truncateB (sumWide - qWide)
      else truncateB sumWide

subModQ :: Coeff -> Coeff -> Coeff
subModQ a b =
  if a >= b
    then a - b
    else qCoeff - (b - a)

butterfly :: (Coeff, Coeff, Coeff) -> (Coeff, Coeff)
butterfly (a, b, zeta) =
  let
    t = montgomeryMul zeta b
  in
    (addModQ a t, subModQ a t)