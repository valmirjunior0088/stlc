module Stlc.ChurchExpression
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
  ExAbstraction Variable Type Expression |
  ExApplication Expression Expression
  deriving (Show)

exInferType :: Context -> Expression -> Either String Type
exInferType context expression =
  case expression of
    ExTrue ->
      Right TpBoolean
    
    ExFalse ->
      Right TpBoolean
    
    ExCaseBoolean input branchTrue branchFalse ->
      do
        inputType <- exInferType context input

        unless
          (inputType == TpBoolean)
          (Left "Inference (ExCaseBoolean): input is not a boolean")
        
        branchTrueType <- exInferType context branchTrue
        branchFalseType <- exInferType context branchFalse

        unless
          (branchTrueType == branchFalseType)
          (Left "Inference (ExCaseBoolean): type mismatch between branches")

        Right branchTrueType

    ExZero ->
      Right TpNatural
    
    ExSuccessor input ->
      do
        inputType <- exInferType context input

        unless
          (inputType == TpNatural)
          (Left "Inference (ExSuccessor): input is not a natural")

        Right TpNatural
    
    ExCaseNatural input branchZero branchSuccessor ->
      do
        inputType <- exInferType context input

        unless
          (inputType == TpNatural)
          (Left "Inference (ExCaseNatural): input is not a natural")

        branchZeroType <- exInferType context branchZero
        branchSuccessorType <- exInferType context branchSuccessor

        unless
          (TpAbstraction TpNatural branchZeroType == branchSuccessorType)
          (Left "Inference (ExCaseNatural): type mismatch between branches")

        Right branchZeroType
    
    ExVariable variable ->
      cnLookup variable context
    
    ExAbstraction variableName variableType abstractionBody ->
      do
        context' <- cnInsert variableName variableType context
        abstractionBodyType <- exInferType context' abstractionBody

        Right (TpAbstraction variableType abstractionBodyType)
    
    ExApplication function argument ->
      do
        functionType <- exInferType context function
        
        case functionType of
          TpAbstraction inputType outputType ->
            do
              argumentType <- exInferType context argument

              unless
                (inputType == argumentType)
                (Left "Inference (ExApplication): type mismatch")

              Right outputType

          _ ->
            Left "Inference (ExApplication): left-hand side is not a function"
