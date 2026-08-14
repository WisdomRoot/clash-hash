{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Component.NTTCore
  ( Coeff
  , Product
  , butterfly
  , butterflyPipeline
  , montgomeryMul
  ) where

import Clash.Prelude
import GHC.Generics (Generic)

type Coeff = Unsigned 23
type Product = Unsigned 46

type MontWord = Unsigned 24
type MontWide = Unsigned 48

qCoeff :: Coeff
qCoeff = 8_380_417

-- -q^(-1) mod 2^24
qInv :: MontWord
qInv = 8_380_415


-- Montgomery reduction
montgomeryReduce :: Product -> Coeff
montgomeryReduce x =
  let
    xLow :: MontWord
    xLow =
      truncateB x

    mIntermediate :: Unsigned 47
    mIntermediate =
      (resize xLow `shiftL` 23)
        - (resize xLow `shiftL` 13)
        - resize xLow

    m :: MontWord
    m =
      truncateB mIntermediate

    mWide :: MontWide
    mWide =
      resize m

    mq :: MontWide
    mq =
      (mWide `shiftL` 23)
        - (mWide `shiftL` 13)
        + mWide

    sumWide :: Unsigned 49
    sumWide =
      resize x + resize mq

    shifted :: Unsigned 25
    shifted =
      truncateB (shiftR sumWide 24)

    qWide :: Unsigned 25
    qWide =
      8_380_417

    reduced :: Unsigned 25
    reduced =
      if shifted >= qWide
        then shifted - qWide
        else shifted
  in
    truncateB reduced


-- Montgomery multiplication
montgomeryMul :: Coeff -> Coeff -> Coeff
montgomeryMul a b =
  montgomeryReduce (a `mul` b)


-- Modular addition/subtraction
addModQ :: Coeff -> Coeff -> Coeff
addModQ a b =
  let
    sumWide :: MontWord
    sumWide =
      resize a + resize b

    qWide :: MontWord
    qWide =
      resize qCoeff
  in
    if sumWide >= qWide
      then truncateB (sumWide - qWide)
      else truncateB sumWide


subModQ :: Coeff -> Coeff -> Coeff
subModQ a b =
  if a >= b
    then a - b
    else qCoeff - (b - a)

-- Original combinational butterfly
butterfly
  :: (Coeff, Coeff, Coeff)
  -> (Coeff, Coeff)
butterfly (a, b, zeta) =
  let
    t =
      montgomeryMul zeta b
  in
    ( addModQ a t
    , subModQ a t
    )


-- Pipelined butterfly
butterflyPipeline
  :: forall dom.
     HiddenClockResetEnable dom
  => Signal dom (Coeff, Coeff, Coeff)
  -> Signal dom (Coeff, Coeff)
butterflyPipeline input =
  outputReg
  where
    -- Pipeline Stage 1: 23 x 23 multiplication
    mulStage :: Signal dom (Coeff, Product)
    mulStage =
      fmap
        (\(a, b, zeta) ->
          let
            productWide :: Product
            productWide = zeta `mul` b
          in
            (a, productWide)
        )
        input

    mulReg :: Signal dom (Coeff, Product)
    mulReg =
      register
        (0, 0)
        mulStage

    -- Pipeline Stage 2: Montgomery reduction
    reduceStage :: Signal dom (Coeff, Coeff)
    reduceStage =
      fmap
        (\(a, productWide) ->
          (a, montgomeryReduce productWide)
        )
        mulReg

    reduceReg :: Signal dom (Coeff, Coeff)
    reduceReg =
      register
        (0, 0)
        reduceStage

    -- Pipeline Stage 3: Modular add/sub
    addSubStage :: Signal dom (Coeff, Coeff)
    addSubStage =
      fmap
        (\(a, t) ->
          ( addModQ a t
          , subModQ a t
          )
        )
        reduceReg

    outputReg :: Signal dom (Coeff, Coeff)
    outputReg =
      register
        (0, 0)
        addSubStage