{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.TestHarness.SamplePolyCBD
  ( SamplePolyCBDTopEntity,
    UpstreamStall (..),
    DownstreamBackpressure (..),
    samplePolyCBDReference,
    unpackPython512Bytes,
    runSamplePolyCBDHardware,
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Component.PRF.Common (Eta (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word16, Word8)
import System.FilePath ((</>))
import Test.TestHarness.ExternalReference (callPythonReference)
import Test.TestHarness.StreamCommon (DownstreamBackpressure (..), UpstreamStall (..), makeBackpressureSignal)
import Prelude qualified as P

type SamplePolyCBDTopEntity =
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector 264) ->
  Signal System Bool ->
  Signal System (AXI4Stream 12, Bool)

samplePolyCBDReference :: Eta -> ByteString -> Word8 -> [Word16]
samplePolyCBDReference eta s b =
  let etaByte = case eta of
        Eta2 -> 2
        Eta3 -> 3
      input = BS.concat [BS.singleton etaByte, s, BS.singleton b]
      output = callPythonReference ("reference" </> "kyber" </> "prf_cbd.py") input
      coeffs = unpackPython512Bytes output
   in P.map (.&. 0x0FFF) coeffs

unpackPython512Bytes :: ByteString -> [Word16]
unpackPython512Bytes bs = go (BS.unpack bs)
  where
    go (lo : hi : rest) =
      let val = P.fromIntegral lo + 256 P.* P.fromIntegral hi
       in val : go rest
    go [] = []
    go _ = P.error "unpackPython512Bytes: Expected 512 bytes (little-endian uint16)"

runSamplePolyCBDHardware ::
  SamplePolyCBDTopEntity ->
  ByteString ->
  Word8 ->
  DownstreamBackpressure ->
  [Word16]
runSamplePolyCBDHardware topEntity s b backpressure =
  let msgBV = bsToBV264 (s <> BS.singleton b)
      msgSig = pure msgBV
      treadySignal = makeBackpressureSignal backpressure
      output =
        topEntity
          clockGen
          resetGen
          enableGen
          msgSig
          treadySignal
      sampleCount = 24 P.+ 24 P.+ 256 P.+ 200
      samples = sampleN @System sampleCount (bundle (output, treadySignal))
      validOutputs =
        [ tdata stream
          | ((stream, _), ready) <- samples,
            tvalid stream P.&& ready
        ]
      coeffs =
        P.map (P.fromIntegral . (unpack :: BitVector 12 -> Unsigned 12)) (P.take 256 validOutputs)
   in coeffs

bsToBV264 :: ByteString -> BitVector 264
bsToBV264 bs =
  let padded = BS.take 33 (bs P.<> BS.replicate 33 0)
      bytes = BS.unpack padded
      step acc w =
        (acc `shiftL` 8) .|. resize (reverseBits8 (pack (fromIntegral w :: BitVector 8)))
   in P.foldl step (0 :: BitVector 264) bytes

reverseBits8 :: BitVector 8 -> BitVector 8
reverseBits8 bv = pack (reverse (unpack bv :: Vec 8 Bit))
