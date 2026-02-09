{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.SamplePolyCBD512 (spec) where

import Component.PRF.Common (Eta (Eta3))
import Component.SamplePolyCBD512 qualified as SamplePolyCBD512
import Stream
import Test.Hspec (Spec, describe, it)
import Test.Reference.SamplePolyCBD qualified as Reference
import Prelude (($))
import Prelude qualified as P

spec :: Spec
spec = describe "SamplePolyCBD512" $ do
  it "matches expected handshake timing (no backpressure)" $ do
    let input = [toBV @264 "0123456789abcdef0123456789abcdef!"]
        (output0, output1) =
          P.splitAt 181 (Reference.run Eta3 "0123456789abcdef0123456789abcdef!")
    run
      SamplePolyCBD512.i264o12
      [Silent 25, Output output0, Silent 25, Output output1]
      [Input input, Hold 305]
      [Ready 306]
  it "matches expected handshake timing (periodic backpressure)" $ do
    let input = toBV @264 "0123456789abcdef0123456789abcdef!"
        coeffs = Reference.run Eta3 "0123456789abcdef0123456789abcdef!"
        (c0, rest0) = P.splitAt 15 coeffs
        (c1, c2) = P.splitAt 166 rest0
    run
      SamplePolyCBD512.i264o12
      [Silent 25, Output c0, Silent 10, Output c1, Silent 25, Output c2]
      [Input [input], Hold 315]
      [Ready 40, Backpress 10, Ready 266]
  it "matches expected handshake timing (initial backpressure)" $ do
    let input = toBV @264 "0123456789abcdef0123456789abcdef!"
        coeffs = Reference.run Eta3 "0123456789abcdef0123456789abcdef!"
        (c0, c1) = P.splitAt 181 coeffs
    run
      SamplePolyCBD512.i264o12
      [Silent 25, Output c0, Silent 25, Output c1]
      [Input [input], Hold 305]
      [Backpress 12, Ready 294]
