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
  , butterflyPipeline
  , montgomeryMul
  )
import GHC.Generics (Generic)
import Prelude hiding ((!!), repeat, not, (&&))

type Poly = Vec 256 Coeff

-- Pipeline request/response
data ButterflyRequest = ButterflyRequest
  { reqAIndex    :: Index 256
  , reqBIndex    :: Index 256
  , reqA         :: Coeff
  , reqB         :: Coeff
  , reqZeta      :: Coeff
  , reqLastGroup :: Bool
  }
  deriving (Generic, NFDataX)


data ButterflyResponse = ButterflyResponse
  { rspAIndex    :: Index 256
  , rspBIndex    :: Index 256
  , rspA         :: Coeff
  , rspB         :: Coeff
  , rspLastGroup :: Bool
  }
  deriving (Generic, NFDataX)

-- Controller phase
data NTTPhase
  = Idle
  | Issue
  | Drain
  deriving (Generic, NFDataX, Eq)

data NTTState = NTTState
  { statePhase  :: NTTPhase
  , stateDone   :: Bool
  , stateStage  :: Index 8
  , stateOpBase :: Unsigned 9
  , statePoly   :: Poly
  }
  deriving (Generic, NFDataX)

initialState :: NTTState
initialState =
  NTTState
    { statePhase  = Idle
    , stateDone   = False
    , stateStage  = 0
    , stateOpBase = 0
    , statePoly   = repeat 0
    }

-- Stage parameters
stageParameters :: Index 8 -> (Unsigned 9, Unsigned 9)
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

-- Generate one butterfly request
makeRequest :: Unsigned 9 -> NTTState -> (Bool, ButterflyRequest)
makeRequest lane state =
  if statePhase state == Issue
    then
      (True, request)
    else
      (False, request)
  where
    poly =
      statePoly state

    stage =
      stateStage state

    opNumber =
      stateOpBase state + lane

    (len, zetaBase) =
      stageParameters stage

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

    request =
      ButterflyRequest
        { reqAIndex    = aIndex
        , reqBIndex    = bIndex
        , reqA         = poly !! aIndex
        , reqB         = poly !! bIndex
        , reqZeta      = zetasMont !! zetaIndex
        , reqLastGroup = stateOpBase state == 124
        }


-- Pipeline one butterfly lane
pipelineLane :: HiddenClockResetEnable dom
             => Signal dom (Bool, ButterflyRequest)
             -> Signal dom (Bool, ButterflyResponse)
pipelineLane requestSignal =
  bundle (validDelayed, responseSignal)
  where

    -- Extract arithmetic input.
    butterflyInput =
      fmap
        (\(_, request) ->
          ( reqA request
          , reqB request
          , reqZeta request
          )
        )
        requestSignal

    -- Actual 3-cycle butterfly datapath.
    butterflyOutput =
      butterflyPipeline butterflyInput

    -- Metadata travelling alongside butterfly
    metadata =
      fmap
        (\(valid, request) ->
          ( valid
          , reqAIndex request
          , reqBIndex request
          , reqLastGroup request
          )
        )
        requestSignal


    metaReg1 =
      register
        (False, 0, 0, False)
        metadata

    metaReg2 =
      register
        (False, 0, 0, False)
        metaReg1

    metaReg3 =
      register
        (False, 0, 0, False)
        metaReg2


    validDelayed =
      fmap
        (\(valid, _, _, _) -> valid)
        metaReg3


    responseSignal =
      liftA2
        (\(_, aIndex, bIndex, lastGroup) (outA, outB) ->
          ButterflyResponse
            { rspAIndex    = aIndex
            , rspBIndex    = bIndex
            , rspA         = outA
            , rspB         = outB
            , rspLastGroup = lastGroup
            }
        )
        metaReg3
        butterflyOutput


-- Write one butterfly result into the polynomial
writeResponse :: (Bool, ButterflyResponse)
  -> Poly
  -> Poly
writeResponse (valid, response) poly =
  if valid
    then
      let
        p1 =
          replace
            (rspAIndex response)
            (rspA response)
            poly

        p2 =
          replace
            (rspBIndex response)
            (rspB response)
            p1
      in
        p2

    else
      poly


-- Write four pipeline outputs
writeFourResponses
  :: ( (Bool, ButterflyResponse)
     , (Bool, ButterflyResponse)
     , (Bool, ButterflyResponse)
     , (Bool, ButterflyResponse)
     )
  -> Poly
  -> Poly
writeFourResponses
  (response0, response1, response2, response3)
  poly =
    let
      p1 = writeResponse response0 poly
      p2 = writeResponse response1 p1
      p3 = writeResponse response2 p2
      p4 = writeResponse response3 p3
    in
      p4

-- Controller
nttNextState
  :: NTTState
  -> (Bool, Poly)
  -> ( (Bool, ButterflyResponse)
     , (Bool, ButterflyResponse)
     , (Bool, ButterflyResponse)
     , (Bool, ButterflyResponse)
     )
  -> NTTState
nttNextState state (start, inputPoly) responses =
  case statePhase state of

    -- IDLE
    Idle ->
      if start
        then
          NTTState
            { statePhase  = Issue
            , stateDone   = False
            , stateStage  = 0
            , stateOpBase = 0
            , statePoly   = inputPoly
            }

        else
          state
            { stateDone = False
            }

    -- ISSUE
    Issue ->
      let

        updatedPoly =
          writeFourResponses
            responses
            (statePoly state)

        currentOp =
          stateOpBase state

        lastIssue =
          currentOp == 124

      in

        if lastIssue
          then
            NTTState
              { statePhase  = Drain
              , stateDone   = False
              , stateStage  = stateStage state
              , stateOpBase = currentOp
              , statePoly   = updatedPoly
              }

          else
            NTTState
              { statePhase  = Issue
              , stateDone   = False
              , stateStage  = stateStage state
              , stateOpBase = currentOp + 4
              , statePoly   = updatedPoly
              }

    -- DRAIN
    Drain ->
      let

        updatedPoly =
          writeFourResponses
            responses
            (statePoly state)

        (valid0, response0) =
          case responses of
            (r0, _, _, _) -> r0

        finalGroupReturned =
          valid0 && rspLastGroup response0

        lastStage =
          stateStage state == 7

      in

        if finalGroupReturned
          then

            if lastStage
              then
                -- Entire NTT is complete.
                NTTState
                  { statePhase  = Idle
                  , stateDone   = True
                  , stateStage  = 7
                  , stateOpBase = 0
                  , statePoly   = updatedPoly
                  }

              else
                NTTState
                  { statePhase  = Issue
                  , stateDone   = False
                  , stateStage  = stateStage state + 1
                  , stateOpBase = 0
                  , statePoly   = updatedPoly
                  }

          else
            state
              { stateDone = False
              , statePoly = updatedPoly
              }


-- Complete pipelined NTT
nttPipelined :: HiddenClockResetEnable dom
  => Signal dom (Bool, Poly)
  -> Signal dom (Bool, Poly)
nttPipelined inputSignal =
  bundle
    ( fmap stateDone stateSignal
    , fmap statePoly stateSignal
    )
  where

    stateSignal =
      register initialState nextStateSignal

    -- Generate four requests every cycle
    request0 =
      fmap (makeRequest 0) stateSignal

    request1 =
      fmap (makeRequest 1) stateSignal

    request2 =
      fmap (makeRequest 2) stateSignal

    request3 =
      fmap (makeRequest 3) stateSignal


    -- Four parallel pipelined butterfly lanes
    response0 =
      pipelineLane request0

    response1 =
      pipelineLane request1

    response2 =
      pipelineLane request2

    response3 =
      pipelineLane request3


    responses =
      bundle
        ( response0
        , response1
        , response2
        , response3
        )


    -- Controller next-state logic
    nextStateSignal =
      liftA3
        nttNextState
        stateSignal
        inputSignal
        responses


-- Top entity
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
        nttPipelined
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