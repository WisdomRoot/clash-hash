{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.TestHarness.SampleNTTCLI
  ( runTest,
    runHardware,
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Clash.Sized.Vector qualified as V
import Component.SampleNTT qualified as SampleNTT
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Word (Word16, Word8)
import System.Process (readProcess)
import Test.Hspec (Expectation, shouldBe)
import Prelude qualified as P

runTest :: ByteString -> Expectation
runTest input = do
  expected <- callSampleNTTCLI input
  let actual = runHardware input
  actual `shouldBe` expected

runHardware :: ByteString -> [Word16]
runHardware input =
  let msgBV = bsToBV272 input
      msgSig = pure msgBV
      treadySignal = pure True
      output =
        SampleNTT.topEntity
          clockGen
          resetGen
          enableGen
          msgSig
          treadySignal
      samples = sampleN @System 400 (bundle (output, treadySignal))
      validOutputs =
        [ tdata stream
          | ((stream, _), ready) <- samples,
            tvalid stream P.&& ready
        ]
      coeffs = P.map (P.fromIntegral . (unpack :: BitVector 12 -> Unsigned 12)) (P.take 10 validOutputs)
   in coeffs

callSampleNTTCLI :: ByteString -> IO [Word16]
callSampleNTTCLI input = do
  let hexArg = bsToHex input
  out <- readProcess "python3" ["reference/kyber/sample_ntt_cli.py", hexArg] ""
  pure (parseFirst10 out)

parseFirst10 :: String -> [Word16]
parseFirst10 output =
  case P.dropWhile (/= "SHAKE128 raw (first 10 12-bit chunks):") (lines output) of
    _hdr : line : _ ->
      let nums = P.map read (words (trim line)) :: [Int]
       in P.map P.fromIntegral (P.take 10 nums)
    _ -> []

trim :: String -> String
trim = dropWhileEnd isSpace . P.dropWhile isSpace

bsToBV272 :: ByteString -> BitVector 272
bsToBV272 bs =
  let bytes = BS.unpack bs
      padded = P.take 34 (bytes P.++ P.repeat 0)
      vec :: Vec 34 (BitVector 8)
      vec = V.unsafeFromList (P.map (fromIntegral :: Word8 -> BitVector 8) padded)
   in pack vec

bsToHex :: ByteString -> String
bsToHex bs = P.concatMap toHex (BS.unpack bs)
  where
    toHex :: Word8 -> String
    toHex w =
      let hi = fromIntegral (w `div` 16) :: Int
          lo = fromIntegral (w `mod` 16) :: Int
       in [hexDigit hi, hexDigit lo]

    hexDigit :: Int -> Char
    hexDigit n
      | n < 10 = P.toEnum (n + P.fromEnum '0')
      | otherwise = P.toEnum (n - 10 + P.fromEnum 'a')
