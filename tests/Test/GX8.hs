{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.GX8 (spec) where

import AXI4Stream (Pipe)
import Clash.Prelude (System, (++#), bundle, clockGen, enableGen, resetGen, unbundle)
import Component.GX8 qualified as GX8
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.Maybe (isJust)
import Stream
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, chooseInt, forAll)
import Test.TestHarness.ExternalReference (callPythonReference)
import Prelude (Maybe (..), ($))
import Prelude qualified as P

i272o512AsPipe :: Pipe System 272 512
i272o512AsPipe (outReady, inStream) =
  let (outStream, inReady) =
        unbundle (GX8.i272o512 clockGen resetGen enableGen (bundle (inStream, outReady)))
   in (inReady, outStream)

gReference :: ByteString -> (ByteString, ByteString)
gReference input =
  let output = callPythonReference ("reference" </> "kyber" </> "g.py") input
   in (BS.take 32 output, BS.drop 32 output)

simulate :: InputTiming 272 -> OutputTiming 512
simulate inputTiming =
  let (inputPattern, inputValues) = expandInputTiming inputTiming
      inputBV =
        case inputValues of
          (v : _) -> v
          [] -> P.error "Test.GX8.simulate: no input provided"
      startSilence =
        case L.findIndex isJust inputPattern of
          Just i -> i
          Nothing -> P.error "Test.GX8.simulate: no input provided"
      inputBS = bvToBS 33 inputBV
      (rho, sigma) = gReference inputBS
      out0 = toBV @256 rho
      out1 = toBV @256 sigma
      base = [Silent 3, Output [out1 ++# out0]]
   in if startSilence P.== 0 then base else Silent startSilence : base

genCase :: Gen (InputTiming 272, BackpressureTiming)
genCase = do
  inputBV <- Stream.genInputBV @272 33
  backpressure <- genBackpressure
  holdLen <- chooseInt (0, 5)
  let inputTiming =
        if holdLen P.== 0
          then [Input [inputBV]]
          else [Hold holdLen, Input [inputBV]]
  P.pure (inputTiming, backpressure)

spec :: Spec
spec = describe "G-X8" $ do
  it "matches expected output (no backpressure)" $ do
    let input = toBV @272 ("0123456789abcdef0123456789abcdef" P.<> BS.pack [0x02])
    runPipeInput i272o512AsPipe simulate [Input [input]] [Ready 1]
  it "matches expected output (upstream stall)" $ do
    let input = toBV @272 ("0123456789abcdef0123456789abcdef" P.<> BS.pack [0x02])
    runPipeInput i272o512AsPipe simulate [Hold 5, Input [input]] [Ready 1]
  it "matches expected output (periodic backpressure)" $ do
    let input = toBV @272 ("0123456789abcdef0123456789abcdef" P.<> BS.pack [0x02])
    runPipeInput i272o512AsPipe simulate [Input [input]] [Ready 2, Backpress 1]
  describe "QuickCheck property tests" $
    it "matches reference for random inputs and backpressure" $
      forAll genCase $ \(inputTiming, backpressure) ->
        runPipeInput i272o512AsPipe simulate inputTiming backpressure
