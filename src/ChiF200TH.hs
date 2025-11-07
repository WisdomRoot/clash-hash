{-# LANGUAGE TemplateHaskell #-}

module ChiF200TH
  ( chiF200
  , topEntity
  ) where

import Clash.Prelude
import Language.Haskell.TH
import SHA3internal (State, sha3_constants, chi_constants)

-- Chi transformation for b=200: l=3, w=8
-- Uses lightweight TH to generate explicit code with literal indices

type State200 = State 200

-- | Chi transformation - generated via TH with literal indices
chiF200 :: State200 -> State200
chiF200 = $(do
  let triples = toList (chi_constants (sha3_constants @3 @8 @200))
  s <- newName "s"

  -- Type-annotated index literal to avoid defaulting warnings
  let idx i = sigE (litE (integerL (toInteger i)))
                   (conT ''Index `appT` litT (numTyLit 200))

  -- Generate one bit expression: s !! i0 `xor` (complement (s !! i1) .&. s !! i2)
  let bitExpr (i0, i1, i2) =
        infixE (Just (infixE (Just (varE s)) (varE '(!!)) (Just (idx i0))))
               (varE 'xor)
               (Just (infixE (Just (appE (varE 'complement)
                                         (infixE (Just (varE s)) (varE '(!!)) (Just (idx i1)))))
                             (varE '(.&.))
                             (Just (infixE (Just (varE s)) (varE '(!!)) (Just (idx i2))))))

  -- Build Vec from expressions
  let vec [] = [| Nil |]
      vec (e:es) = [| $(return e) :> $(vec es) |]

  bitExprs <- mapM bitExpr triples
  lamE [varP s] (vec bitExprs)
  )

-- Hardware top entity - Chi transformation on 200-bit state
{-# ANN topEntity
  (Synthesize
    { t_name = "ChiF200_OneRound"
    , t_inputs = [ PortName "CLK"
                 , PortName "RST"
                 , PortName "EN"
                 , PortName "DIN"
                 ]
    , t_output = PortName "DOUT"
    }) #-}
{-# OPAQUE topEntity #-}

topEntity :: Clock System
          -> Reset System
          -> Enable System
          -> Signal System State200
          -> Signal System State200
topEntity = exposeClockResetEnable $ fmap chiF200
