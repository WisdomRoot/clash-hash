module Timing
  ( Output (..),
    OutputTiming,
    expandOutputTiming,
    Input (..),
    InputTiming,
    expandInputTiming,
    Backpressure (..),
    BackpressureTiming,
    expandBackpressureTiming,
  )
where

import Prelude

type Label = String

--------------------------------------------------------------------------------

data Output
  = Silent Int Label -- cycles of silence with a string label
  | Output Int -- cycles with expected output handshake (tvalid && tready)
  deriving (Eq)

type OutputTiming = [Output]

--------------------------------------------------------------------------------

data Input
  = Hold Int -- cycles with no input, i.e. when input tvalid is low
  | Input Int -- cycles with input, i.e. when input tvalid is high

type InputTiming = [Input]

expandInputTiming :: InputTiming -> [Bool]
expandInputTiming = concatMap expand
  where
    expand :: Input -> [Bool]
    expand (Hold n) = replicate n False
    expand (Input n) = replicate n True

data Backpressure
  = Ready Int -- cycles when output tready is high (ready to accept output)
  | Backpress Int -- cycles when output tready is low (backpressure applied)

type BackpressureTiming = [Backpressure]

expandBackpressureTiming :: BackpressureTiming -> [Bool]
expandBackpressureTiming = concatMap expand
  where
    expand :: Backpressure -> [Bool]
    expand (Ready n) = replicate n True
    expand (Backpress n) = replicate n False

expandOutputTiming :: OutputTiming -> [Bool]
expandOutputTiming = concatMap expand
  where
    expand :: Output -> [Bool]
    expand (Silent n _) = replicate n False
    expand (Output n) = replicate n True
