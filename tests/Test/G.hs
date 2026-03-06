{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.G (spec) where

import AXI4Stream (Pipe)
import Clash.Prelude (BitVector, System, bundle, clockGen, enableGen, resetGen, unbundle)
import Component.G qualified as G
import Data.Bits (setBit, testBit)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.Maybe (isJust)
import Data.Word (Word8)
import Stream
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, arbitrary, chooseInt, forAll, shuffle, vectorOf)
import Test.TestHarness.ExternalReference (callPythonReference)
import Prelude (Maybe (..), ($))
import Prelude qualified as P

i274o256AsPipe :: Pipe System 274 256
i274o256AsPipe (outReady, inStream) =
  let (outStream, inReady) =
        unbundle (G.i274o256 clockGen resetGen enableGen (bundle (inStream, outReady)))
   in (inReady, outStream)

bv274ToBS :: BitVector 274 -> ByteString
bv274ToBS bv = BS.pack [byteAt i | i <- [0 .. 32]]
  where
    byteAt :: P.Int -> Word8
    byteAt byteIdx =
      let base = byteIdx P.* 8
       in P.foldl
            (\acc bitIdx -> if testBit bv (base P.+ bitIdx) then setBit acc bitIdx else acc)
            (0 :: Word8)
            [0 .. 7]

gReference :: ByteString -> (ByteString, ByteString)
gReference input =
  let output = callPythonReference ("reference" </> "kyber" </> "g.py") input
   in (BS.take 32 output, BS.drop 32 output)

simulate :: InputTiming 274 -> OutputTiming 256
simulate inputTiming =
  let (inputPattern, inputValues) = expandInputTiming inputTiming
      inputBV =
        case inputValues of
          (v : _) -> v
          [] -> P.error "Test.G.simulate: no input provided"
      startSilence =
        case L.findIndex isJust inputPattern of
          Just i -> i
          Nothing -> P.error "Test.G.simulate: no input provided"
      inputBS = bv274ToBS inputBV
      (rho, sigma) = gReference inputBS
      out0 = toBV @256 rho
      out1 = toBV @256 sigma
      base = [Silent 24, Output [out0, out1]]
   in if startSilence P.== 0 then base else Silent startSilence : base

genInputBV :: Gen (BitVector 274)
genInputBV = do
  bytes <- vectorOf 33 (arbitrary :: Gen Word8)
  P.pure (toBV @274 (BS.pack bytes))

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

genCase :: Gen (InputTiming 274, BackpressureTiming)
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
spec = describe "G (Stream)" $ do
  it "matches expected output (no backpressure)" $ do
    let input = toBV @274 ("0123456789abcdef0123456789abcdef" P.<> BS.pack [0x02])
    runPipeInput i274o256AsPipe simulate [Input [input]] [Ready 1]
  it "matches expected output (upstream stall)" $ do
    let input = toBV @274 ("0123456789abcdef0123456789abcdef" P.<> BS.pack [0x02])
    runPipeInput i274o256AsPipe simulate [Hold 5, Input [input]] [Ready 1]
  it "matches expected output (periodic backpressure)" $ do
    let input = toBV @274 ("0123456789abcdef0123456789abcdef" P.<> BS.pack [0x02])
    runPipeInput i274o256AsPipe simulate [Input [input]] [Ready 2, Backpress 1]
  describe "QuickCheck property tests" $
    it "matches reference for random inputs and backpressure" $
      forAll genCase $ \(inputTiming, backpressure) ->
        runPipeInput i274o256AsPipe simulate inputTiming backpressure
