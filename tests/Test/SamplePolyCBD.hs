{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.SamplePolyCBD
  ( specO24,
  )
where

import AXI4Stream (Pipe)
import Clash.Prelude (BitVector, SNat (..), System, (++#), bundle, clockGen, enableGen, resetGen, slice, unbundle)
import Component.PRF.Common (Eta (Eta2, Eta3))
import Component.SamplePolyCBD qualified as SamplePolyCBD
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.Maybe (isJust)
import Data.Word (Word8)
import Stream
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, arbitrary, chooseInt, elements, forAll, vectorOf)
import Test.Reference.SamplePolyCBD qualified as Reference
import Prelude (Maybe (..), ($))
import Prelude qualified as P

i272o24AsPipe :: Pipe System 272 24
i272o24AsPipe (outReady, inStream) =
  let (outStream, inReady) =
        unbundle (SamplePolyCBD.i272o24 clockGen resetGen enableGen (bundle (inStream, outReady)))
   in (inReady, outStream)

etaFromInput :: BitVector 272 -> Eta
etaFromInput msg =
  let etaByte = slice (SNat @271) (SNat @264) msg :: BitVector 8
   in if etaByte P.== 3 then Eta3 else Eta2

msg264FromInput :: BitVector 272 -> BitVector 264
msg264FromInput msg = slice (SNat @263) (SNat @0) msg

simulate24 :: InputTiming 272 -> OutputTiming 24
simulate24 inputTiming =
  let (inputPattern, inputValues) = expandInputTiming inputTiming
      inputBV =
        case inputValues of
          (v : _) -> v
          [] -> P.error "Test.SamplePolyCBD.simulate24: no input provided"
      startSilence =
        case L.findIndex isJust inputPattern of
          Just i -> i
          Nothing -> P.error "Test.SamplePolyCBD.simulate24: no input provided"
      eta = etaFromInput inputBV
      msg264 = msg264FromInput inputBV
      coeffs = Reference.run eta msg264
      pairs = pairCoeffs coeffs
      base =
        case eta of
          Eta2 -> [Silent 25, Output pairs]
          Eta3 ->
            let (p0, p1) = P.splitAt 90 pairs
             in [Silent 25, Output p0, Silent 25, Output p1]
   in if startSilence P.== 0 then base else Silent startSilence : base
  where
    pairCoeffs (c0 : c1 : rest) = (c1 ++# c0) : pairCoeffs rest
    pairCoeffs [c0] = [(0 :: BitVector 12) ++# c0]
    pairCoeffs [] = []

genInputBVGeneral :: Gen (BitVector 272)
genInputBVGeneral = do
  msgBytes <- vectorOf 33 (arbitrary :: Gen Word8)
  etaByte <- elements [2, 3]
  P.pure (toBV @272 (BS.pack (msgBytes P.++ [etaByte])))

genCase :: Gen (InputTiming 272, BackpressureTiming)
genCase = do
  inputBV <- genInputBVGeneral
  backpressure <- genBackpressure
  holdLen <- chooseInt (0, 5)
  let inputTiming =
        if holdLen P.== 0
          then [Input [inputBV]]
          else [Hold holdLen, Input [inputBV]]
  P.pure (inputTiming, backpressure)

specO24 :: Spec
specO24 = describe "CBD-O24" $ do
  it "matches expected output (eta=2, no backpressure)" $ do
    let input = toBV @272 ("0123456789abcdef0123456789abcdef!" P.<> BS.pack [2])
    runPipeInput i272o24AsPipe simulate24 [Input [input]] [Ready 1]
  it "matches expected output (eta=3, no backpressure)" $ do
    let input = toBV @272 ("0123456789abcdef0123456789abcdef!" P.<> BS.pack [3])
    runPipeInput i272o24AsPipe simulate24 [Input [input]] [Ready 1]
  describe "QuickCheck property tests (i272o24)" $
    it "matches reference for random inputs and backpressure" $
      forAll genCase $ \(inputTiming, backpressure) ->
        runPipeInput i272o24AsPipe simulate24 inputTiming backpressure
