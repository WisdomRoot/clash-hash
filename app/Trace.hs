{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Clash.Prelude
import qualified Driver
import qualified Hash.KeccakF1600
import System.Directory (createDirectoryIfMissing)
import qualified Prelude as P

-- Adjust the message or testbench as needed
msg :: BitVector 64
msg = 0x0000000000000000

main :: IO ()
main = do
  let (_digest, done, sReady, mValid, mData, mLast, beat, sValid, sData, sLast, mReady) =
        Driver.debug Hash.KeccakF1600.topEntity msg
      signals = bundle (done, sReady, mValid, mData, mLast, beat, sValid, sData, sLast, mReady)
      samples = sampleN @System 200 signals
      vcdText = renderVCD samples
  let outDir = "trace"
  createDirectoryIfMissing True outDir
  let path = outDir P.++ "/trace.vcd"
  P.writeFile path vcdText
  P.putStrLn ("Wrote " P.++ path P.++ " (0–200 cycles)")

renderVCD ::
  [ ( Bool,
      Bool,
      Bool,
      BitVector 64,
      Bool,
      Unsigned 3,
      Bool,
      BitVector 64,
      Bool,
      Bool
    )
  ] ->
  String
renderVCD samples =
  P.unlines
    $ header
    P.++ ["$dumpvars"]
    P.++ dumpVars (0 :: Int) (P.head samples)
    P.++ ["$end"]
    P.++ P.concatMap step (P.zip [(0 :: Int) ..] samples)
  where
    header =
      [ "$timescale 1ns $end",
        "$scope module trace $end",
        "$var wire 1 a done $end",
        "$var wire 1 b outReady $end",
        "$var wire 1 c outValid $end",
        "$var wire 64 d outData $end",
        "$var wire 1 e outLast $end",
        "$var wire 3 f beat $end",
        "$var wire 1 g inTValid $end",
        "$var wire 64 h inTData $end",
        "$var wire 1 i inTLast $end",
        "$var wire 1 j inTReady $end",
        "$upscope $end",
        "$enddefinitions $end"
      ]

    step (t, sig) =
      ["#" P.++ show t] P.++ dumpVars t sig

    dumpVars _ (done', sReady', mValid', mData', mLast', beat', sValid', sData', sLast', mReady') =
      [ one done' "a",
        one sReady' "b",
        one mValid' "c",
        bin mData' "d",
        one mLast' "e",
        bin (bitCoerce beat' :: BitVector 3) "f",
        one sValid' "g",
        bin sData' "h",
        one sLast' "i",
        one mReady' "j"
      ]

    one b name = (if b then "1" else "0") P.++ name

    bin :: forall n. (KnownNat n) => BitVector n -> String -> String
    bin bv name = 'b' : bits P.++ ' ' : name
      where
        bits = P.map (\b -> if b then '1' else '0') (toList (bitCoerce bv :: Vec n Bool))
