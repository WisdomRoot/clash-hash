{-# LANGUAGE DataKinds #-}

module Component.NTT
  ( topEntity
  , butterfly
  , ntt256
  , montgomeryMul
  ) where

import Clash.Prelude
import Component.NTTConstants (Coeff, zetasMont)
import Prelude hiding ((!!))

type Product = Unsigned 46
type Poly = Vec 256 Coeff
type MontWord = Unsigned 24
type MontWide = Unsigned 48

q :: Integer
q = 8380417

qCoeff :: Coeff
qCoeff = fromInteger q

qMont :: MontWord
qMont = fromInteger q

-- -q^(-1) mod 2^24
qInv :: MontWord
qInv = 8380415

montgomeryReduce :: Product -> Coeff
montgomeryReduce productValue =
  let
    productExtended :: MontWide
    productExtended =
      resize productValue

    productLow :: MontWord
    productLow =
      resize productValue

    mProduct :: MontWide
    mProduct =
      resize productLow * resize qInv

    m :: MontWord
    m =
      resize mProduct

    mq :: MontWide
    mq =
      resize m * resize qMont

    sumValue :: MontWide
    sumValue =
      productExtended + mq

    shifted :: MontWide
    shifted =
      shiftR sumValue 24

    candidate :: MontWord
    candidate =
      resize shifted

    reduced :: MontWord
    reduced =
      if candidate >= qMont
        then candidate - qMont
        else candidate
  in
    resize reduced

addModQ :: Coeff -> Coeff -> Coeff
addModQ a b =
  let
    sumWide :: Unsigned 24
    sumWide =
      resize a + resize b

    qWide :: Unsigned 24
    qWide =
      fromInteger q
  in
    resize
      (if sumWide >= qWide
         then sumWide - qWide
         else sumWide)

subModQ :: Coeff -> Coeff -> Coeff
subModQ a b =
  if a >= b
    then a - b
    else qCoeff - (b - a)

-- Computes a*b*R^-1 mod q.
montgomeryMul :: Coeff -> Coeff -> Coeff
montgomeryMul a b =
  let
    productWide :: Product
    productWide =
      resize a * resize b
  in
    montgomeryReduce productWide

-- zetaMont must equal zeta*R mod q.
butterfly :: (Coeff, Coeff, Coeff) -> (Coeff, Coeff)
butterfly (a, b, zetaMont) =
  let
    t =
      montgomeryMul zetaMont b

    outA =
      addModQ a t

    outB =
      subModQ a t
  in
    (outA, outB)

ntt256 :: Poly -> Poly
ntt256 input =
  let
    s128 = nttStage 128 1 input
    s64  = nttStage 64 2 s128
    s32  = nttStage 32 4 s64
    s16  = nttStage 16 8 s32
    s8   = nttStage 8 16 s16
    s4   = nttStage 4 32 s8
    s2   = nttStage 2 64 s4
    s1   = nttStage 1 128 s2
  in
    s1

nttStage :: Unsigned 9 -> Unsigned 9 -> Poly -> Poly
nttStage len zetaBase input =
  imap calculateOutput input
  where
    calculateOutput :: Index 256 -> Coeff -> Coeff
    calculateOutput index _ =
      let
        i :: Unsigned 9
        i =
          fromIntegral index

        groupSize :: Unsigned 9
        groupSize =
          2 * len

        groupIndex :: Unsigned 9
        groupIndex =
          i `div` groupSize

        positionInGroup :: Unsigned 9
        positionInGroup =
          i `mod` groupSize

        zetaIndex :: Index 256
        zetaIndex =
          fromIntegral (zetaBase + groupIndex)

        zetaMont :: Coeff
        zetaMont =
          zetasMont !! zetaIndex
      in
        if positionInGroup < len
          then
            let
              aIndex :: Index 256
              aIndex =
                fromIntegral i

              bIndex :: Index 256
              bIndex =
                fromIntegral (i + len)

              a =
                input !! aIndex

              b =
                input !! bIndex

              (outA, _) =
                butterfly (a, b, zetaMont)
            in
              outA
          else
            let
              aIndex :: Index 256
              aIndex =
                fromIntegral (i - len)

              bIndex :: Index 256
              bIndex =
                fromIntegral i

              a =
                input !! aIndex

              b =
                input !! bIndex

              (_, outB) =
                butterfly (a, b, zetaMont)
            in
              outB

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System Poly
  -> Signal System Poly
topEntity _clk _rst _en =
  fmap ntt256

{-# ANN topEntity
  (Synthesize
    { t_name = "NTT256"
    , t_inputs =
        [ PortName "clk"
        , PortName "rst"
        , PortName "en"
        , PortName "poly"
        ]
    , t_output = PortName "result"
    }) #-}