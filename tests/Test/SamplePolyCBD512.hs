{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.SamplePolyCBD512 (spec, simulate) where

import Component.PRF.Common (Eta (Eta3))
import Clash.Prelude (BitVector)
import Component.SamplePolyCBD512 qualified as SamplePolyCBD512
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.Maybe (isJust)
import Data.Word (Word8)
import Stream
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, arbitrary, forAll, shuffle, vectorOf)
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

genInputBV :: Gen (BitVector 264)
genInputBV = do
  bytes <- vectorOf 33 (arbitrary :: Gen Word8)
  P.pure (toBV @264 (BS.pack bytes))

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
      ( P.replicate 256 P.True
          P.++ P.replicate 64 P.False
      )
  P.pure (compressBackpressure bools)

genCase :: Gen (BitVector 264, BackpressureTiming)
genCase = do
  inputBV <- genInputBV
  backpressure <- genBackpressure
  P.pure (inputBV, backpressure)

spec :: Spec
spec = describe "SamplePolyCBD512" $ do
  it "matches expected handshake timing (no backpressure)" $ do
    let input = [toBV @264 "0123456789abcdef0123456789abcdef!"]
    run
      SamplePolyCBD512.i264o12
      simulate
      [Input input, Hold 305]
      [Ready 306]
  it "matches expected handshake timing (periodic backpressure)" $ do
    let input = toBV @264 "0123456789abcdef0123456789abcdef!"
    run
      SamplePolyCBD512.i264o12
      simulate
      [Input [input], Hold 315]
      [Ready 40, Backpress 10, Ready 266]
  it "matches expected handshake timing (initial backpressure)" $ do
    let input = toBV @264 "0123456789abcdef0123456789abcdef!"
    run
      SamplePolyCBD512.i264o12
      simulate
      [Input [input], Hold 305]
      [Backpress 12, Ready 294]
  describe "QuickCheck property tests" $
    it "matches reference for random inputs and backpressure" $
      forAll genCase $ \(inputBV, backpressure) -> do
        run SamplePolyCBD512.i264o12 simulate [Input [inputBV]] backpressure
