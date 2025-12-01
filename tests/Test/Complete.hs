{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Complete (spec) where

import Clash.Prelude
import qualified Prelude as P
import qualified Driver
import qualified Hash.KeccakF1600
import qualified Reference.SHA3 as Ref
import qualified Reference.SHA3internal as RefInt
import Test.Hspec

spec :: Spec
spec = describe "AXI sponge end-to-end" $ do
  it "Hash.KeccakF1600.topEntity matches Reference.SHA3.sha3_256 for 'abc'" $ do
    -- Message bits (LSB-first per byte) for "abc"
    let msgBits24 = RefInt.toBitString $(listToVecTH "abc")
        msgBits64 = msgBits24 ++ repeat @40 0
        msgBV64   = pack msgBits64 :: BitVector 64

        expectedBits = Ref.sha3_256 msgBits24
        expectedBV   = pack expectedBits :: BitVector 256

        (digestSig, doneSig) = Driver.run Hash.KeccakF1600.topEntity msgBV64
        digests = sampleN @System 50 digestSig
        dones   = sampleN @System 50 doneSig

        finalDigest = P.head [d | (d, True) <- P.zip digests dones]

    finalDigest `shouldBe` expectedBV
