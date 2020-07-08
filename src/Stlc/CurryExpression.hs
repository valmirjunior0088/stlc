module Stlc.CurryExpression
  (Expression (..)
  ,exInferType
  )
  where

import Stlc.Type
  (Type (..)
  )

import Stlc.Context
  (Variable
  ,Context (..)
  ,cnInsert
  ,cnLookup
  )

import Control.Monad
  (unless
  )

data Expression =
  ExTrue |
  ExFalse |
  ExCaseBoolean Expression Expression Expression |
  ExZero |
  ExSuccessor Expression |
  ExCaseNatural Expression Expression Expression |
  ExVariable Variable |
  ExAbstraction Variable Expression |
  ExApplication Expression Expression
  deriving (Show)

exInferType :: Context -> Expression -> Either String Type
exInferType context expression =
  undefined