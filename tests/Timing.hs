{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Timing
  ( Output (..),
    OutputTiming (..),
    view,
    Input (..),
    InputTiming,
    Backpressure (..),
    BackpressureTiming,
  )
where

import Prelude

type Label = String

--------------------------------------------------------------------------------

data Output
  = Silent Int Label -- cycles of silence with a string label
  | Output Int -- cycles with output, i.e. when output tvalid is high
  deriving (Eq)

newtype OutputTiming = OutputTiming [Output]
  deriving (Eq)

instance Show OutputTiming where
  show (OutputTiming outputs) = concatMap render outputs
    where
      render :: Output -> String
      render (Silent n _) = replicate n '.'
      render (Output n) = replicate n '#'

view :: (Show a) => Int -> Int -> a -> String
view start end a =
  let s = show a
      len = length s
      lo = max 0 start
      hi = min (len - 1) end
   in if len == 0 || lo > hi
        then ""
        else take (hi - lo + 1) (drop lo s) <> "\n" <> renderMarkers lo hi <> "\n"
  where
    renderMarkers :: Int -> Int -> String
    renderMarkers lo hi =
      let total = hi - lo + 1
          base = replicate total '-'
          first =
            if lo `mod` 10 == 0
              then lo
              else lo + (10 - (lo `mod` 10))
          marks = [first, first + 10 .. hi]
       in foldl place base marks
      where
        place :: String -> Int -> String
        place acc m =
          let str = show m
              idx = m - lo
           in replaceSlice acc idx str

    replaceSlice :: String -> Int -> String -> String
    replaceSlice acc idx str =
      let pre = take idx acc
          post = drop (idx + length str) acc
       in pre ++ str ++ post

exampleOutputTiming :: OutputTiming
exampleOutputTiming =
  OutputTiming
    [ Silent 2 "reset",
      Silent 24 "permute",
      Output 128,
      Silent 24 "permute",
      Output 128
    ]

--------------------------------------------------------------------------------

data Input
  = Hold Int -- cycles with no input, i.e. when input tvalid is low
  | Input Int -- cycles with input, i.e. when input tvalid is high

type InputTiming = [Input]

data Backpressure
  = Ready Int -- cycles when output tready is high (ready to accept output)
  | Backpress Int -- cycles when output tready is low (backpressure applied)

type BackpressureTiming = [Backpressure]
