{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.SamplePolyCBD512 (spec, simulate) where

import Component.PRF.Common (Eta (Eta3))
import Clash.Prelude ((++#))
import Component.SamplePolyCBD512 qualified as SamplePolyCBD512
import Data.List qualified as L
import Data.Maybe (isJust)
import Stream
import Test.Hspec (Spec, describe, it)
import Test.Reference.SamplePolyCBD qualified as Reference
import Prelude (Maybe (..), ($))
import Prelude qualified as P

simulate :: InputTiming 264 -> OutputTiming 12
simulate inputTiming =
  let (inputPattern, inputValues) = expandInputTiming inputTiming
      inputBV =
        case inputValues of
          (v : _) -> v
          [] -> P.error "SamplePolyCBD512.simulate: no input provided"
      startSilence =
        case L.findIndex isJust inputPattern of
          Just i -> i
          Nothing -> P.error "SamplePolyCBD512.simulate: no input provided"
      coeffs = Reference.run Eta3 inputBV
      (c0, c1) = P.splitAt 181 coeffs
      base = [Silent 25, Output c0, Silent 25, Output c1]
   in if startSilence P.== 0 then base else Silent startSilence : base

simulate24 :: InputTiming 264 -> OutputTiming 24
simulate24 inputTiming =
  let (inputPattern, inputValues) = expandInputTiming inputTiming
      inputBV =
        case inputValues of
          (v : _) -> v
          [] -> P.error "SamplePolyCBD512.simulate24: no input provided"
      startSilence =
        case L.findIndex isJust inputPattern of
          Just i -> i
          Nothing -> P.error "SamplePolyCBD512.simulate24: no input provided"
      coeffs = Reference.run Eta3 inputBV
      pairs = toPairs coeffs
      (p0, p1) = P.splitAt 90 pairs
      base = [Silent 25, Output p0, Silent 25, Output p1]
   in if startSilence P.== 0 then base else Silent startSilence : base
  where
    toPairs (a : b : rest) = (b ++# a) : toPairs rest
    toPairs _ = []

spec :: Spec
spec = describe "SamplePolyCBD512" $ do
  timingSpec ""
    SamplePolyCBD512.i264o12
    simulate
    [ ("no backpressure", [Input [toBV @264 "0123456789abcdef0123456789abcdef!"], Hold 305], [Ready 306]),
      ("periodic backpressure", [Input [toBV @264 "0123456789abcdef0123456789abcdef!"], Hold 315], [Ready 40, Backpress 10, Ready 266]),
      ("initial backpressure", [Input [toBV @264 "0123456789abcdef0123456789abcdef!"], Hold 305], [Backpress 12, Ready 294])
    ]
  timingSpec " (i264o24)"
    SamplePolyCBD512.i264o24
    simulate24
    [ ("no backpressure", [Input [toBV @264 "0123456789abcdef0123456789abcdef!"], Hold 305], [Ready 306]),
      ("periodic backpressure", [Input [toBV @264 "0123456789abcdef0123456789abcdef!"], Hold 315], [Ready 40, Backpress 10, Ready 266]),
      ("initial backpressure", [Input [toBV @264 "0123456789abcdef0123456789abcdef!"], Hold 305], [Backpress 12, Ready 294])
    ]
  where
    timingSpec label topEntity simulateFn = P.mapM_
        ( \(name, inputTiming, backpressure) ->
            it
              ("matches expected handshake timing" P.++ label P.++ " (" P.++ name P.++ ")")
              (run topEntity simulateFn inputTiming backpressure)
        )
