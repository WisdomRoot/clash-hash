{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.XOF6 (spec) where

import AXI4Stream (Pipe2)
import Clash.Prelude (BitVector, System)
import Component.XOF6 qualified as XOF6
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

i272o72AsPipe :: Pipe2 System 272 72
i272o72AsPipe = XOF6.i272o72Core

toChunks72 :: ByteString -> [BitVector 72]
toChunks72 bs
  | BS.null bs = []
  | P.otherwise =
      let (chunk, rest) = BS.splitAt 9 bs
       in toBV @72 chunk : toChunks72 rest

simulate :: InputTiming 272 -> OutputTiming 72
simulate inputTiming =
  let (inputPattern, inputValues) = expandInputTiming inputTiming
      inputBV =
        case inputValues of
          (v : _) -> v
          [] -> P.error "Test.XOF6.simulate: no input provided"
      startSilence =
        case L.findIndex isJust inputPattern of
          Just i -> i
          Nothing -> P.error "Test.XOF6.simulate: no input provided"
      inputBS = bvToBS 34 inputBV
      outputBS = Crypton.shake128 (56 P.* 9) inputBS
      chunks = toChunks72 outputBS
      (block0, rest0) = P.splitAt 18 chunks
      (block1, block2) = P.splitAt 19 rest0
      base = [Silent 25, Output block0, Silent 24, Output block1, Silent 24, Output block2]
   in if startSilence P.== 0 then base else Silent startSilence : base

genCase :: Gen (InputTiming 272, BackpressureTiming)
genCase = do
  inputBV <- genInputBV @272 34
  backpressure <- genBackpressure
  holdLen <- chooseInt (0, 5)
  let inputTiming =
        if holdLen P.== 0
          then [Input [inputBV]]
          else [Hold holdLen, Input [inputBV]]
  P.pure (inputTiming, backpressure)

spec :: Spec
spec = describe "XOF6" $ do
  it "matches expected output (no backpressure)" $ do
    let input = toBV @272 ("0123456789abcdef0123456789abcdef!!" :: ByteString)
    runPipe2Input i272o72AsPipe simulate [Input [input]] [Ready 1]
  it "matches expected output (upstream stall)" $ do
    let input = toBV @272 ("0123456789abcdef0123456789abcdef!!" :: ByteString)
    runPipe2Input i272o72AsPipe simulate [Hold 5, Input [input]] [Ready 1]
  it "matches expected output (periodic backpressure)" $ do
    let input = toBV @272 ("0123456789abcdef0123456789abcdef!!" :: ByteString)
    runPipe2Input i272o72AsPipe simulate [Input [input]] [Ready 2, Backpress 1]
  describe "QuickCheck property tests" $
    it "matches reference for random inputs and backpressure" $
      forAll genCase $ \(inputTiming, backpressure) ->
        runPipe2Input i272o72AsPipe simulate inputTiming backpressure
