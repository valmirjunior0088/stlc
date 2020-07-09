module Stlc.ChurchExpression
  (Type (..)
  ,Expression (..)
  ,exInferType
  )
  where

import Stlc.Variable
  (Variable (..)
  )

import Stlc.Context
  (Context (..)
  ,cnInsert
  ,cnLookup
  )

import Control.Monad
  (unless
  )

data Type =
  TpBoolean |
  TpNatural |
  TpAbstraction Type Type
  deriving (Eq, Show)

data Expression =
  ExTrue |
  ExFalse |
  ExCaseBoolean Expression Expression Expression |
  ExZero |
  ExSuccessor Expression |
  ExCaseNatural Expression Expression Expression |
  ExVariable Variable |
  ExAbstraction Variable Type Expression |
  ExApplication Expression Expression
  deriving (Show)

exInferType :: Context Type -> Expression -> Either String Type
exInferType context expression =
  case expression of
    ExTrue ->
      Right TpBoolean
    
    ExFalse ->
      Right TpBoolean
    
    ExCaseBoolean input true false ->
      do
        inputType <- exInferType context input

        unless
          (inputType == TpBoolean)
          (Left "ChurchExpression.ExCaseBoolean: input is not a boolean")
        
        trueType <- exInferType context true
        falseType <- exInferType context false

        unless
          (trueType == falseType)
          (Left "ChurchExpression.ExCaseBoolean: type mismatch between branches")

        Right trueType

    ExZero ->
      Right TpNatural
    
    ExSuccessor input ->
      do
        inputType <- exInferType context input

        unless
          (inputType == TpNatural)
          (Left "ChurchExpression.ExSuccessor: input is not a natural")

        Right TpNatural
    
    ExCaseNatural input zero successor ->
      do
        inputType <- exInferType context input

        unless
          (inputType == TpNatural)
          (Left "ChurchExpression.ExCaseNatural: input is not a natural")

        zeroType <- exInferType context zero
        successorType <- exInferType context successor

        unless
          (TpAbstraction TpNatural zeroType == successorType)
          (Left "ChurchExpression.ExCaseNatural: type mismatch between branches")

        Right zeroType
    
    ExVariable variable ->
      cnLookup variable context
    
    ExAbstraction variable variableType body ->
      do
        context' <- cnInsert variable variableType context
        bodyType <- exInferType context' body

        Right (TpAbstraction variableType bodyType)
    
    ExApplication function argument ->
      do
        functionType <- exInferType context function
        
        case functionType of
          TpAbstraction inputType outputType ->
            do
              argumentType <- exInferType context argument

              unless
                (inputType == argumentType)
                (Left "ChurchExpression.ExApplication: type mismatch")

              Right outputType

          _ ->
            Left "ChurchExpression.ExApplication: left-hand side is not a function"
