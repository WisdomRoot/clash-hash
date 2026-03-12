{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.G768 (spec) where

import AXI4Stream (Pipe)
import Clash.Prelude (BitVector, System, bundle, clockGen, enableGen, resetGen, unbundle)
import Component.G768 qualified as G768
import Data.Bits (setBit, testBit)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.Maybe (isJust)
import Data.Word (Word8)
import Stream
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, arbitrary, chooseInt, forAll, shuffle, vectorOf)
import Test.TestHarness.G.Common qualified as GReference
import Prelude (Maybe (..), ($))
import Prelude qualified as P

i256o256AsPipe :: Pipe System 256 256
i256o256AsPipe (outReady, inStream) =
  let (outStream, inReady) =
        unbundle (G768.i256o256 clockGen resetGen enableGen (bundle (inStream, outReady)))
   in (inReady, outStream)

bv256ToBS :: BitVector 256 -> ByteString
bv256ToBS bv = BS.pack [byteAt i | i <- [0 .. 31]]
  where
    byteAt :: P.Int -> Word8
    byteAt byteIdx =
      let base = byteIdx P.* 8
       in P.foldl
            (\acc bitIdx -> if testBit bv (base P.+ bitIdx) then setBit acc bitIdx else acc)
            (0 :: Word8)
            [0 .. 7]

simulate :: InputTiming 256 -> OutputTiming 256
simulate inputTiming =
  let (inputPattern, inputValues) = expandInputTiming inputTiming
      inputBV =
        case inputValues of
          (v : _) -> v
          [] -> P.error "Test.G768.simulate: no input provided"
      startSilence =
        case L.findIndex isJust inputPattern of
          Just i -> i
          Nothing -> P.error "Test.G768.simulate: no input provided"
      inputBS = bv256ToBS inputBV
      (rho, sigma) = GReference.gReferenceK 3 inputBS
      out0 = toBV @256 rho
      out1 = toBV @256 sigma
      base = [Silent 24, Output [out0, out1]]
   in if startSilence P.== 0 then base else Silent startSilence : base

genInputBV :: Gen (BitVector 256)
genInputBV = do
  bytes <- vectorOf 32 (arbitrary :: Gen Word8)
  P.pure (toBV @256 (BS.pack bytes))

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

genCase :: Gen (InputTiming 256, BackpressureTiming)
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
spec = describe "G768" $ do
  it "matches expected output (no backpressure)" $ do
    let input = toBV @256 "0123456789abcdef0123456789abcdef"
    runPipeInput i256o256AsPipe simulate [Input [input]] [Ready 1]
  it "matches expected output (upstream stall)" $ do
    let input = toBV @256 "0123456789abcdef0123456789abcdef"
    runPipeInput i256o256AsPipe simulate [Hold 5, Input [input]] [Ready 1]
  it "matches expected output (periodic backpressure)" $ do
    let input = toBV @256 "0123456789abcdef0123456789abcdef"
    runPipeInput i256o256AsPipe simulate [Input [input]] [Ready 2, Backpress 1]
  describe "QuickCheck property tests" $
    it "matches reference for random inputs and backpressure" $
      forAll genCase $ \(inputTiming, backpressure) ->
        runPipeInput i256o256AsPipe simulate inputTiming backpressure
