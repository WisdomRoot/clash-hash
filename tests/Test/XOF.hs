{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.XOF (spec) where

import AXI4Stream (Pipe)
import Clash.Prelude (BitVector, System, bundle, clockGen, enableGen, resetGen, unbundle)
import Component.XOF qualified as XOF
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.Maybe (isJust)
import Reference.Crypton qualified as Crypton
import Stream
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, chooseInt, forAll)
import Prelude (Maybe (..), ($))
import Prelude qualified as P

i272o48AsPipe :: Pipe System 272 48
i272o48AsPipe (outReady, inStream) =
  let (outStream, inReady) =
        unbundle (XOF.i272o48 clockGen resetGen enableGen (bundle (inStream, outReady)))
   in (inReady, outStream)

toChunks48 :: ByteString -> [BitVector 48]
toChunks48 bs
  | BS.null bs = []
  | P.otherwise =
      let (chunk, rest) = BS.splitAt 6 bs
       in toBV @48 chunk : toChunks48 rest

simulate :: InputTiming 272 -> OutputTiming 48
simulate inputTiming =
  let (inputPattern, inputValues) = expandInputTiming inputTiming
      inputBV =
        case inputValues of
          (v : _) -> v
          [] -> P.error "Test.XOF.simulate: no input provided"
      startSilence =
        case L.findIndex isJust inputPattern of
          Just i -> i
          Nothing -> P.error "Test.XOF.simulate: no input provided"
      inputBS = bvToBS 34 inputBV
      outputBS = Crypton.shake128 (56 P.* 6) inputBS
      chunks = toChunks48 outputBS
      (firstBlock, secondBlock) = P.splitAt 28 chunks
      base = [Silent 25, Output firstBlock, Silent 24, Output secondBlock]
   in if startSilence P.== 0 then base else Silent startSilence : base

genCase :: Gen (InputTiming 272, BackpressureTiming)
genCase = do
  inputBV <- Stream.genInputBV @272 34
  backpressure <- genBackpressure
  holdLen <- chooseInt (0, 5)
  let inputTiming =
        if holdLen P.== 0
          then [Input [inputBV]]
          else [Hold holdLen, Input [inputBV]]
  P.pure (inputTiming, backpressure)

spec :: Spec
spec = describe "XOF" $ do
  it "matches expected output (no backpressure)" $ do
    let input = toBV @272 ("0123456789abcdef0123456789abcdef!!" :: ByteString)
    runPipeInput i272o48AsPipe simulate [Input [input]] [Ready 1]
  it "matches expected output (upstream stall)" $ do
    let input = toBV @272 ("0123456789abcdef0123456789abcdef!!" :: ByteString)
    runPipeInput i272o48AsPipe simulate [Hold 5, Input [input]] [Ready 1]
  it "matches expected output (periodic backpressure)" $ do
    let input = toBV @272 ("0123456789abcdef0123456789abcdef!!" :: ByteString)
    runPipeInput i272o48AsPipe simulate [Input [input]] [Ready 2, Backpress 1]
  describe "QuickCheck property tests" $
    it "matches reference for random inputs and backpressure" $
      forAll genCase $ \(inputTiming, backpressure) ->
        runPipeInput i272o48AsPipe simulate inputTiming backpressure
