{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.SamplePolyCBD3
  ( specO12,
    specO24,
  )
where

import AXI4Stream (Pipe)
import Clash.Prelude (BitVector, System, (++#), bundle, clockGen, enableGen, resetGen, unbundle)
import Component.PRF.Common (Eta (Eta3))
import Component.SamplePolyCBD3 qualified as SamplePolyCBD3
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.Maybe (isJust)
import Data.Word (Word8)
import Stream
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, arbitrary, chooseInt, forAll, shuffle, vectorOf)
import Test.Reference.SamplePolyCBD qualified as Reference
import Prelude (Maybe (..), ($))
import Prelude qualified as P

i264o12AsPipe :: Pipe System 264 12
i264o12AsPipe (outReady, inStream) =
  let (outStream, inReady) =
        unbundle (SamplePolyCBD3.i264o12 clockGen resetGen enableGen (bundle (inStream, outReady)))
   in (inReady, outStream)

i264o24AsPipe :: Pipe System 264 24
i264o24AsPipe (outReady, inStream) =
  let (outStream, inReady) =
        unbundle (SamplePolyCBD3.i264o24 clockGen resetGen enableGen (bundle (inStream, outReady)))
   in (inReady, outStream)

simulate :: InputTiming 264 -> OutputTiming 12
simulate inputTiming =
  let (inputPattern, inputValues) = expandInputTiming inputTiming
      inputBV =
        case inputValues of
          (v : _) -> v
          [] -> P.error "Test.SamplePolyCBD3.simulate: no input provided"
      startSilence =
        case L.findIndex isJust inputPattern of
          Just i -> i
          Nothing -> P.error "Test.SamplePolyCBD3.simulate: no input provided"
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
          [] -> P.error "Test.SamplePolyCBD3.simulate24: no input provided"
      startSilence =
        case L.findIndex isJust inputPattern of
          Just i -> i
          Nothing -> P.error "Test.SamplePolyCBD3.simulate24: no input provided"
      coeffs = Reference.run Eta3 inputBV
      pairs = pairCoeffs coeffs
      (p0, p1) = P.splitAt 90 pairs
      base = [Silent 25, Output p0, Silent 25, Output p1]
   in if startSilence P.== 0 then base else Silent startSilence : base
  where
    pairCoeffs (c0 : c1 : rest) = (c1 ++# c0) : pairCoeffs rest
    pairCoeffs [c0] = [(0 :: BitVector 12) ++# c0]
    pairCoeffs [] = []

compressBackpressure :: [P.Bool] -> BackpressureTiming
compressBackpressure [] = []
compressBackpressure (b : bs) =
  let (same, rest) = P.span (P.== b) bs
      len = 1 P.+ P.length same
      tag = if b then Ready len else Backpress len
   in tag : compressBackpressure rest

genBackpressure :: Gen BackpressureTiming
genBackpressure = do
  bools <-
    shuffle
      ( P.replicate 8 P.True
          P.++ P.replicate 2 P.False
      )
  P.pure (compressBackpressure bools)

genInputBV :: Gen (BitVector 264)
genInputBV = do
  bytes <- vectorOf 33 (arbitrary :: Gen Word8)
  P.pure (toBV @264 (BS.pack bytes))

genCase :: Gen (InputTiming 264, BackpressureTiming)
genCase = do
  inputBV <- genInputBV
  backpressure <- genBackpressure
  holdLen <- chooseInt (0, 5)
  let inputTiming =
        if holdLen P.== 0
          then [Input [inputBV]]
          else [Hold holdLen, Input [inputBV]]
  P.pure (inputTiming, backpressure)

specO12 :: Spec
specO12 = describe "CBD3-O12" $ do
  it "i264o12 matches expected output (no backpressure)" $ do
    let input = toBV @264 ("0123456789abcdef0123456789abcdef!" :: BS.ByteString)
    runPipeInput i264o12AsPipe simulate [Input [input]] [Ready 1]
  it "i264o12 matches expected output (periodic backpressure)" $ do
    let input = toBV @264 ("0123456789abcdef0123456789abcdef!" :: BS.ByteString)
    runPipeInput i264o12AsPipe simulate [Input [input]] [Ready 2, Backpress 1]
  describe "QuickCheck property tests" $
    it "matches reference for random inputs and backpressure" $
      forAll genCase $ \(inputTiming, backpressure) ->
        runPipeInput i264o12AsPipe simulate inputTiming backpressure

specO24 :: Spec
specO24 = describe "CBD3-O24" $ do
  it "i264o24 matches expected output (no backpressure)" $ do
    let input = toBV @264 ("0123456789abcdef0123456789abcdef!" :: BS.ByteString)
    runPipeInput i264o24AsPipe simulate24 [Input [input]] [Ready 1]
  it "i264o24 matches expected output (periodic backpressure)" $ do
    let input = toBV @264 ("0123456789abcdef0123456789abcdef!" :: BS.ByteString)
    runPipeInput i264o24AsPipe simulate24 [Input [input]] [Ready 2, Backpress 1]
  describe "QuickCheck property tests" $
    it "matches reference for random inputs and backpressure" $
      forAll genCase $ \(inputTiming, backpressure) ->
        runPipeInput i264o24AsPipe simulate24 inputTiming backpressure
