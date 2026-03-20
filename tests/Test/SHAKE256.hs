{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.SHAKE256 (spec) where

import AXI4Stream (PipeCtrl2)
import Clash.Prelude (BitVector, System)
import Component.SHAKE256 qualified as SHAKE256
import Data.Bits (setBit, testBit)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.Word (Word8)
import Reference.Crypton qualified as Crypton
import Stream
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, arbitrary, chooseInt, forAll, vectorOf)
import Prelude (Bool (..), Int, Maybe (..), ($))
import Prelude qualified as P

i64o64AsPipeCtrl :: PipeCtrl2 System Bool 64 64
i64o64AsPipeCtrl = SHAKE256.i64o64Core

bv64ToBytes :: BitVector 64 -> [Word8]
bv64ToBytes bv = [byteAt i | i <- [0 .. 7]]
  where
    byteAt :: Int -> Word8
    byteAt byteIdx =
      let base = byteIdx P.* 8
       in P.foldl
            (\acc bitIdx -> if testBit bv (base P.+ bitIdx) then setBit acc bitIdx else acc)
            (0 :: Word8)
            [0 .. 7]

toWords64 :: ByteString -> [BitVector 64]
toWords64 bs
  | BS.null bs = []
  | P.otherwise =
      let (chunk, rest) = BS.splitAt 8 bs
       in toBV @64 chunk : toWords64 rest

simulate :: [Bool] -> InputTiming 64 -> OutputTiming 64
simulate flushPattern inputTiming =
  let outputBytes = 64
      (inputPattern, inputValues) = expandInputTiming inputTiming
      outputWords = toWords64 (Crypton.shake256 outputBytes (BS.pack (P.concatMap bv64ToBytes inputValues)))
      firstOutputIdx =
        if P.null inputValues
          then case L.findIndex P.id flushPattern of
            Just i -> i P.+ 24
            Nothing -> P.error "Test.SHAKE256.simulate: empty input requires flush"
          else
            let lastBeatIdx =
                  case [i | (i, Just _) <- P.zip [0 ..] inputPattern] of
                    [] -> P.error "Test.SHAKE256.simulate: no input provided"
                    xs -> P.last xs
                beatCount = P.length inputValues
                permuteDelay = if beatCount P.== 17 then 48 else 24
             in lastBeatIdx P.+ permuteDelay
   in [Silent firstOutputIdx, Output outputWords]

genMessageBeats :: Gen [BitVector 64]
genMessageBeats = do
  beatCount <- chooseInt (1, 17)
  vectorOf beatCount (arbitrary :: Gen (BitVector 64))

genCase :: Gen (InputTiming 64, BackpressureTiming)
genCase = do
  beats <- genMessageBeats
  backpressure <- genBackpressure
  holdLen <- chooseInt (0, 5)
  let inputTiming =
        if holdLen P.== 0
          then [Input beats]
          else [Hold holdLen, Input beats]
  P.pure (inputTiming, backpressure)

spec :: Spec
spec = describe "SHAKE3-256" $ do
  it "matches expected output (flush-only, empty input)" $
    runPipeCtrl2Input i64o64AsPipeCtrl False [True] simulate [] [Ready 1]
  it "matches expected output (flush-only with periodic backpressure)" $
    runPipeCtrl2Input i64o64AsPipeCtrl False [True] simulate [] [Ready 2, Backpress 1]
  it "matches expected output (single-beat input, no backpressure)" $ do
    let input = toBV @64 ("01234567" :: ByteString)
    runPipeCtrl2Input i64o64AsPipeCtrl False [False] simulate [Input [input]] [Ready 1]
  it "matches expected output (17-beat input boundary case)" $ do
    let block = P.replicate 17 (toBV @64 ("ABCDEFGH" :: ByteString))
    runPipeCtrl2Input i64o64AsPipeCtrl False [False] simulate [Input block] [Ready 1]
  describe "QuickCheck property tests" $
    it "matches reference for random inputs and backpressure" $
      forAll genCase $ \(inputTiming, backpressure) ->
        runPipeCtrl2Input i64o64AsPipeCtrl False [False] simulate inputTiming backpressure
