{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.SamplePolyCBD (spec) where

import AXI4Stream (Pipe)
import Clash.Prelude (BitVector, SNat (..), System, bundle, clockGen, enableGen, resetGen, slice, unbundle)
import Component.PRF.Common (Eta (Eta2, Eta3))
import Component.SamplePolyCBD qualified as SamplePolyCBD
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.Maybe (isJust)
import Data.Word (Word8)
import Stream
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, arbitrary, chooseInt, elements, forAll, shuffle, vectorOf)
import Test.Reference.SamplePolyCBD qualified as Reference
import Prelude (Maybe (..), ($))
import Prelude qualified as P

i272o12AsPipe :: Pipe System 272 12
i272o12AsPipe (outReady, inStream) =
  let (outStream, inReady) =
        unbundle (SamplePolyCBD.i272o12 clockGen resetGen enableGen (bundle (inStream, outReady)))
   in (inReady, outStream)

etaFromInput :: BitVector 272 -> Eta
etaFromInput msg =
  let etaByte = slice (SNat @271) (SNat @264) msg :: BitVector 8
   in if etaByte P.== 3 then Eta3 else Eta2

msg264FromInput :: BitVector 272 -> BitVector 264
msg264FromInput msg = slice (SNat @263) (SNat @0) msg

simulate :: InputTiming 272 -> OutputTiming 12
simulate inputTiming =
  let (inputPattern, inputValues) = expandInputTiming inputTiming
      inputBV =
        case inputValues of
          (v : _) -> v
          [] -> P.error "Test.SamplePolyCBD.simulate: no input provided"
      startSilence =
        case L.findIndex isJust inputPattern of
          Just i -> i
          Nothing -> P.error "Test.SamplePolyCBD.simulate: no input provided"
      eta = etaFromInput inputBV
      msg264 = msg264FromInput inputBV
      coeffs = Reference.run eta msg264
      base =
        case eta of
          Eta2 -> [Silent 25, Output coeffs]
          Eta3 ->
            let (c0, c1) = P.splitAt 181 coeffs
             in [Silent 25, Output c0, Silent 25, Output c1]
   in if startSilence P.== 0 then base else Silent startSilence : base

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

genInputBV :: Gen (BitVector 272)
genInputBV = do
  msgBytes <- vectorOf 33 (arbitrary :: Gen Word8)
  etaByte <- elements [2, 3]
  P.pure (toBV @272 (BS.pack (msgBytes P.++ [etaByte])))

genCase :: Gen (InputTiming 272, BackpressureTiming)
genCase = do
  inputBV <- genInputBV
  backpressure <- genBackpressure
  holdLen <- chooseInt (0, 5)
  let inputTiming =
        if holdLen P.== 0
          then [Input [inputBV]]
          else [Hold holdLen, Input [inputBV]]
  P.pure (inputTiming, backpressure)

spec :: Spec
spec = describe "SamplePolyCBD (Stream)" $ do
  it "matches expected output (eta=2, no backpressure)" $ do
    let input = toBV @272 ("0123456789abcdef0123456789abcdef!" P.<> BS.pack [2])
    runPipeInput i272o12AsPipe simulate [Input [input]] [Ready 1]
  it "matches expected output (eta=3, no backpressure)" $ do
    let input = toBV @272 ("0123456789abcdef0123456789abcdef!" P.<> BS.pack [3])
    runPipeInput i272o12AsPipe simulate [Input [input]] [Ready 1]
  it "matches expected output (eta=3, periodic backpressure)" $ do
    let input = toBV @272 ("0123456789abcdef0123456789abcdef!" P.<> BS.pack [3])
    runPipeInput i272o12AsPipe simulate [Input [input]] [Ready 2, Backpress 1]
  describe "QuickCheck property tests" $
    it "matches reference for random inputs and backpressure" $
      forAll genCase $ \(inputTiming, backpressure) ->
        runPipeInput i272o12AsPipe simulate inputTiming backpressure
