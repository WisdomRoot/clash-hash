{-# LANGUAGE TemplateHaskell #-}

module ChiF200
  ( chiF200
  , topEntity
  ) where

import Clash.Prelude hiding (toList, map)
import Language.Haskell.TH
import Data.Foldable (toList)
import SHA3internal (State, sha3_constants, chi_constants)

-- Chi transformation for b=200: l=3, w=8
-- Uses Template Haskell to generate explicit code with literal indices

type State200 = State 200

-- | Chi transformation - generated via TH with literal indices
chiF200 :: State200 -> State200
chiF200 = $(do
  -- Compute constants directly in TH splice to avoid stage restriction
  let constants = sha3_constants @3 @8 @200
      triples = toList (chi_constants constants)
  sName <- newName "s"

  let mkBitExpr :: Vec 3 (Index 200) -> Q Language.Haskell.TH.Exp
      mkBitExpr (i0 :> i1 :> i2 :> Nil) = do
        let idxE i = litE (integerL (toInteger i))
            indexE i = infixE (Just (varE sName)) (varE '(!!)) (Just (idxE i))
            compIndex i = appE (varE 'complement) (indexE i)
        infixE (Just (indexE i0))
               (varE 'xor)
               (Just $ infixE (Just (compIndex i1))
                              (varE '(.&.))
                              (Just (indexE i2)))
      mkBitExpr _ = fail "chi_constants entry must have length 3"

  bitExprs <- mapM mkBitExpr triples

  let buildVec :: [Language.Haskell.TH.Exp] -> Q Language.Haskell.TH.Exp
      buildVec [] = [| Nil |]
      buildVec (e:es) = [| $(return e) :> $(buildVec es) |]

  vecExp <- buildVec bitExprs

  lamE [varP sName] (return vecExp)
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
