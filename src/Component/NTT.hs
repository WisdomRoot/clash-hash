{-# LANGUAGE DataKinds #-}

module Component.NTT
  ( topEntity
  , butterfly
  , ntt256
  , montgomeryMul
  ) where

import Clash.Prelude
import Component.NTTConstants (zetasMont)
import Prelude hiding ((!!), zipWith, map, (++))
import Component.NTTCore
  ( Coeff
  , butterfly
  , montgomeryMul
  )

type Poly = Vec 256 Coeff

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

nttStage128 :: Poly -> Poly
nttStage128 input =
  let
    left :: Vec 128 Coeff
    right :: Vec 128 Coeff
    (left, right) = splitAtI input

    zetaMont :: Coeff
    zetaMont = zetasMont !! 1

    results :: Vec 128 (Coeff, Coeff)
    results =
      zipWith
        (\a b -> butterfly (a, b, zetaMont))
        left
        right

    outLeft :: Vec 128 Coeff
    outLeft = map fst results

    outRight :: Vec 128 Coeff
    outRight = map snd results
  in
    outLeft ++ outRight

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
  fmap nttStage128

{-# ANN topEntity
  (Synthesize
    { t_name = "NTTStage128"
    , t_inputs =
        [ PortName "clk"
        , PortName "rst"
        , PortName "en"
        , PortName "poly"
        ]
    , t_output = PortName "result"
    }) #-}