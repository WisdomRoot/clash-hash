{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Component.NTT
  ( topEntity
  , butterfly
  , montgomeryMul
  ) where

import Clash.Prelude
import Component.NTTConstants (zetasMont)
import Component.NTTCore
  ( Coeff
  , butterfly
  , montgomeryMul
  )
import GHC.Generics (Generic)
import Prelude hiding ((!!), repeat, not)

type Poly = Vec 256 Coeff

data NTTState = NTTState
  { stateBusy   :: Bool
  , stateDone   :: Bool
  , stateStage  :: Index 8
  , stateOpBase :: Unsigned 9
  , statePoly   :: Poly
  }
  deriving (Generic, NFDataX)


initialState :: NTTState
initialState =
  NTTState
    { stateBusy   = False
    , stateDone   = False
    , stateStage  = 0
    , stateOpBase = 0
    , statePoly   = repeat 0
    }

stageParameters
  :: Index 8
  -> (Unsigned 9, Unsigned 9)
stageParameters stage =
  case stage of
    0 -> (128,   1)
    1 -> ( 64,   2)
    2 -> ( 32,   4)
    3 -> ( 16,   8)
    4 -> (  8,  16)
    5 -> (  4,  32)
    6 -> (  2,  64)
    7 -> (  1, 128)

runButterfly
  :: Unsigned 9
  -> Unsigned 9
  -> Unsigned 9
  -> Poly
  -> (Index 256, Coeff, Index 256, Coeff)

runButterfly len zetaBase opNumber poly =
  let
    groupIndex :: Unsigned 9
    groupIndex =
      opNumber `div` len

    position :: Unsigned 9
    position =
      opNumber `mod` len

    groupSize :: Unsigned 9
    groupSize =
      2 * len

    aRaw :: Unsigned 9
    aRaw =
      groupIndex * groupSize + position

    bRaw :: Unsigned 9
    bRaw =
      aRaw + len

    aIndex :: Index 256
    aIndex =
      fromIntegral aRaw

    bIndex :: Index 256
    bIndex =
      fromIntegral bRaw

    zetaIndex :: Index 256
    zetaIndex =
      fromIntegral (zetaBase + groupIndex)

    a :: Coeff
    a =
      poly !! aIndex

    b :: Coeff
    b =
      poly !! bIndex

    zetaMont :: Coeff
    zetaMont =
      zetasMont !! zetaIndex

    (outA, outB) =
      butterfly (a, b, zetaMont)

  in
    (aIndex, outA, bIndex, outB)

runFourButterflies
  :: Index 8
  -> Unsigned 9
  -> Poly
  -> Poly

runFourButterflies stage opBase poly =
  let
    (len, zetaBase) =
      stageParameters stage

    -- Butterfly lane 0
    (a0, outA0, b0, outB0) =
      runButterfly
        len
        zetaBase
        opBase
        poly

    -- Butterfly lane 1
    (a1, outA1, b1, outB1) =
      runButterfly
        len
        zetaBase
        (opBase + 1)
        poly

    -- Butterfly lane 2
    (a2, outA2, b2, outB2) =
      runButterfly
        len
        zetaBase
        (opBase + 2)
        poly

    -- Butterfly lane 3
    (a3, outA3, b3, outB3) =
      runButterfly
        len
        zetaBase
        (opBase + 3)
        poly


    -- Write the 8 resulting coefficients back.
    p1 = replace a0 outA0 poly
    p2 = replace b0 outB0 p1

    p3 = replace a1 outA1 p2
    p4 = replace b1 outB1 p3

    p5 = replace a2 outA2 p4
    p6 = replace b2 outB2 p5

    p7 = replace a3 outA3 p6
    p8 = replace b3 outB3 p7

  in
    p8


nttStep
  :: NTTState
  -> (Bool, Poly)
  -> (NTTState, (Bool, Poly))

nttStep state (start, inputPoly)
  | not (stateBusy state) =
      if start
        then
          let
            nextState =
              NTTState
                { stateBusy   = True
                , stateDone   = False
                , stateStage  = 0
                , stateOpBase = 0
                , statePoly   = inputPoly
                }
          in
            (nextState, (False, inputPoly))

        else
          let
            nextState =
              state
                { stateDone = False
                }
          in
            (nextState, (False, statePoly state))

  | otherwise =
      let
        currentStage =
          stateStage state

        currentOp =
          stateOpBase state

        updatedPoly =
          runFourButterflies
            currentStage
            currentOp
            (statePoly state)

        lastGroup =
          currentOp == 124

        lastStage =
          currentStage == 7

      in
        if lastGroup
          then
            if lastStage
              then
                let
                  nextState =
                    NTTState
                      { stateBusy   = False
                      , stateDone   = True
                      , stateStage  = 7
                      , stateOpBase = 0
                      , statePoly   = updatedPoly
                      }
                in
                  (nextState, (True, updatedPoly))

              else
                let
                  nextState =
                    NTTState
                      { stateBusy   = True
                      , stateDone   = False
                      , stateStage  = currentStage + 1
                      , stateOpBase = 0
                      , statePoly   = updatedPoly
                      }
                in
                  (nextState, (False, updatedPoly))

          else
            let
              nextState =
                NTTState
                  { stateBusy   = True
                  , stateDone   = False
                  , stateStage  = currentStage
                  , stateOpBase = currentOp + 4
                  , statePoly   = updatedPoly
                  }
            in
              (nextState, (False, updatedPoly))


nttSequential
  :: HiddenClockResetEnable dom
  => Signal dom (Bool, Poly)
  -> Signal dom (Bool, Poly)

nttSequential =
  mealy nttStep initialState


topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System Bool
  -> Signal System Poly
  -> ( Signal System Bool
     , Signal System Poly
     )

topEntity clk rst en start poly =
  unbundle result
  where
    result =
      exposeClockResetEnable
        nttSequential
        clk
        rst
        en
        (bundle (start, poly))


{-# ANN topEntity
  (Synthesize
    { t_name = "NTT256"
    , t_inputs =
        [ PortName "clk"
        , PortName "rst"
        , PortName "en"
        , PortName "start"
        , PortName "poly"
        ]
    , t_output =
        PortProduct ""
          [ PortName "done"
          , PortName "result"
          ]
    }) #-}