{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.G.Specialized
  ( specializedSpec,
  )
where

import AXI4Stream (AXI4Stream (..), Pipe)
import Clash.Prelude (Clock, Enable, Reset, Signal, System, (++#), bundle, clockGen, enableGen, resetGen, unbundle)
import Data.List qualified as L
import Data.Maybe (isJust)
import Data.Word (Word8)
import Stream
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, chooseInt, forAll)
import Test.TestHarness.G.Common qualified as GReference
import Prelude (Maybe (..), ($))
import Prelude qualified as P

type GTopEntity =
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 256, P.Bool) ->
  Signal System (AXI4Stream 512, P.Bool)

asPipe :: GTopEntity -> Pipe System 256 512
asPipe topEntity (outReady, inStream) =
  let (outStream, inReady) =
        unbundle (topEntity clockGen resetGen enableGen (bundle (inStream, outReady)))
   in (inReady, outStream)

simulateFor :: P.String -> Word8 -> InputTiming 256 -> OutputTiming 512
simulateFor label k inputTiming =
  let (inputPattern, inputValues) = expandInputTiming inputTiming
      inputBV =
        case inputValues of
          (v : _) -> v
          [] -> P.error ("Test." P.++ label P.++ ".simulate: no input provided")
      startSilence =
        case L.findIndex isJust inputPattern of
          Just i -> i
          Nothing -> P.error ("Test." P.++ label P.++ ".simulate: no input provided")
      inputBS = bvToBS 32 inputBV
      (rho, sigma) = GReference.gReferenceK k inputBS
      out0 = toBV @256 rho
      out1 = toBV @256 sigma
      base = [Silent 24, Output [out1 ++# out0]]
   in if startSilence P.== 0 then base else Silent startSilence : base

genCase :: Gen (InputTiming 256, BackpressureTiming)
genCase = do
  inputBV <- Stream.genInputBV @256 32
  backpressure <- genBackpressure
  holdLen <- chooseInt (0, 5)
  let inputTiming =
        if holdLen P.== 0
          then [Input [inputBV]]
          else [Hold holdLen, Input [inputBV]]
  P.pure (inputTiming, backpressure)

specializedSpec :: P.String -> Word8 -> GTopEntity -> Spec
specializedSpec label k topEntity =
  let pipeEntity = asPipe topEntity
      simulate = simulateFor label k
   in describe label $ do
        it "matches expected output (no backpressure)" $ do
          let input = toBV @256 "0123456789abcdef0123456789abcdef"
          runPipeInput pipeEntity simulate [Input [input]] [Ready 1]
        it "matches expected output (upstream stall)" $ do
          let input = toBV @256 "0123456789abcdef0123456789abcdef"
          runPipeInput pipeEntity simulate [Hold 5, Input [input]] [Ready 1]
        it "matches expected output (periodic backpressure)" $ do
          let input = toBV @256 "0123456789abcdef0123456789abcdef"
          runPipeInput pipeEntity simulate [Input [input]] [Ready 2, Backpress 1]
        describe "QuickCheck property tests" $
          it "matches reference for random inputs and backpressure" $
            forAll genCase $ \(inputTiming, backpressure) ->
              runPipeInput pipeEntity simulate inputTiming backpressure
