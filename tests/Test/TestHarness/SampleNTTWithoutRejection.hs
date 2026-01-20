{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.TestHarness.SampleNTTWithoutRejection
  ( runTest,
    runHardware,
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
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
  let expected' = P.map reverseBits12Word expected
      actual = runHardware input
  actual `shouldBe` expected'

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
  out <- readProcess "python3" ["reference/kyber/sample_ntt_without_rejection.py", hexArg] ""
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
  let padded = BS.take 34 (bs P.<> BS.replicate 34 0)
      bytes = BS.unpack padded
      step acc w = (acc `shiftL` 8) .|. resize (pack (fromIntegral w :: BitVector 8))
   in P.foldl step (0 :: BitVector 272) bytes

reverseBits12Word :: Word16 -> Word16
reverseBits12Word w =
  let bv = pack (fromIntegral w :: Unsigned 12)
      rev = pack (reverse (unpack bv :: Vec 12 Bit))
   in P.fromIntegral (unpack rev :: Unsigned 12)

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
