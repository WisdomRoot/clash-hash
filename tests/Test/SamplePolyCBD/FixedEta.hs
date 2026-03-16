{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.SamplePolyCBD.FixedEta
  ( fixedEtaSpecO12,
    fixedEtaSpecO24,
  )
where

import AXI4Stream (AXI4Stream (..), Pipe)
import Clash.Prelude (BitVector, Clock, Enable, Reset, Signal, System, (++#), bundle, clockGen, enableGen, resetGen, unbundle)
import Component.PRF.Common (Eta (Eta2, Eta3))
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.Maybe (isJust)
import Stream
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, chooseInt, forAll)
import Test.Reference.SamplePolyCBD qualified as Reference
import Prelude (Maybe (..), ($))
import Prelude qualified as P

type CBDTopEntity n =
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 264, P.Bool) ->
  Signal System (AXI4Stream n, P.Bool)

asPipe :: CBDTopEntity n -> Pipe System 264 n
asPipe topEntity (outReady, inStream) =
  let (outStream, inReady) =
        unbundle (topEntity clockGen resetGen enableGen (bundle (inStream, outReady)))
   in (inReady, outStream)

simulate12 :: P.String -> Eta -> InputTiming 264 -> OutputTiming 12
simulate12 label eta inputTiming =
  let (inputPattern, inputValues) = expandInputTiming inputTiming
      inputBV =
        case inputValues of
          (v : _) -> v
          [] -> P.error ("Test." P.++ label P.++ ".simulate: no input provided")
      startSilence =
        case L.findIndex isJust inputPattern of
          Just i -> i
          Nothing -> P.error ("Test." P.++ label P.++ ".simulate: no input provided")
      coeffs = Reference.run eta inputBV
      base = case eta of
        Eta2 -> [Silent 25, Output coeffs]
        Eta3 ->
          let (c0, c1) = P.splitAt 181 coeffs
           in [Silent 25, Output c0, Silent 25, Output c1]
   in if startSilence P.== 0 then base else Silent startSilence : base

simulate24 :: P.String -> Eta -> InputTiming 264 -> OutputTiming 24
simulate24 label eta inputTiming =
  let (inputPattern, inputValues) = expandInputTiming inputTiming
      inputBV =
        case inputValues of
          (v : _) -> v
          [] -> P.error ("Test." P.++ label P.++ ".simulate24: no input provided")
      startSilence =
        case L.findIndex isJust inputPattern of
          Just i -> i
          Nothing -> P.error ("Test." P.++ label P.++ ".simulate24: no input provided")
      coeffs = Reference.run eta inputBV
      pairs = pairCoeffs coeffs
      base = case eta of
        Eta2 -> [Silent 25, Output pairs]
        Eta3 ->
          let (p0, p1) = P.splitAt 90 pairs
           in [Silent 25, Output p0, Silent 25, Output p1]
   in if startSilence P.== 0 then base else Silent startSilence : base
  where
    pairCoeffs (c0 : c1 : rest) = (c1 ++# c0) : pairCoeffs rest
    pairCoeffs [c0] = [(0 :: BitVector 12) ++# c0]
    pairCoeffs [] = []

genCase :: Gen (InputTiming 264, BackpressureTiming)
genCase = do
  inputBV <- Stream.genInputBV @264 33
  backpressure <- genBackpressure
  holdLen <- chooseInt (0, 5)
  let inputTiming =
        if holdLen P.== 0
          then [Input [inputBV]]
          else [Hold holdLen, Input [inputBV]]
  P.pure (inputTiming, backpressure)

fixedEtaSpecO12 :: P.String -> Eta -> CBDTopEntity 12 -> Spec
fixedEtaSpecO12 label eta topEntity =
  let pipeEntity = asPipe topEntity
      simulate = simulate12 label eta
   in describe label $ do
        it "i264o12 matches expected output (no backpressure)" $ do
          let input = toBV @264 ("0123456789abcdef0123456789abcdef!" :: BS.ByteString)
          runPipeInput pipeEntity simulate [Input [input]] [Ready 1]
        it "i264o12 matches expected output (periodic backpressure)" $ do
          let input = toBV @264 ("0123456789abcdef0123456789abcdef!" :: BS.ByteString)
          runPipeInput pipeEntity simulate [Input [input]] [Ready 2, Backpress 1]
        describe "QuickCheck property tests" $
          it "matches reference for random inputs and backpressure" $
            forAll genCase $ \(inputTiming, backpressure) ->
              runPipeInput pipeEntity simulate inputTiming backpressure

fixedEtaSpecO24 :: P.String -> Eta -> CBDTopEntity 24 -> Spec
fixedEtaSpecO24 label eta topEntity =
  let pipeEntity = asPipe topEntity
      simulate = simulate24 label eta
   in describe label $ do
        it "i264o24 matches expected output (no backpressure)" $ do
          let input = toBV @264 ("0123456789abcdef0123456789abcdef!" :: BS.ByteString)
          runPipeInput pipeEntity simulate [Input [input]] [Ready 1]
        it "i264o24 matches expected output (periodic backpressure)" $ do
          let input = toBV @264 ("0123456789abcdef0123456789abcdef!" :: BS.ByteString)
          runPipeInput pipeEntity simulate [Input [input]] [Ready 2, Backpress 1]
        describe "QuickCheck property tests" $
          it "matches reference for random inputs and backpressure" $
            forAll genCase $ \(inputTiming, backpressure) ->
              runPipeInput pipeEntity simulate inputTiming backpressure
