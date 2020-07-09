module Stlc.CurryExpression
  (Type (..)
  ,Expression (..)
  ,exInferType
  )
  where

import Stlc.Variable
  (Variable (..)
  ,vrAppend
  )

import Stlc.Context
  (Context (..)
  ,cnInsert
  ,cnLookup
  ,cnAll
  )

data Type =
  TpVariable Variable |
  TpBoolean |
  TpNatural |
  TpAbstraction Type Type
  deriving (Show)

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

type TypeEquation =
  (Type, Type)

tpIsFresh :: Variable -> Type -> Bool
tpIsFresh variable subject =
  case subject of
    TpVariable variable' ->
      if variable == variable' then False else True
    
    TpBoolean ->
      True
    
    TpNatural ->
      True

    TpAbstraction inputType outputType ->
      tpIsFresh variable inputType || tpIsFresh variable outputType

cnFreshen :: Variable -> Context Type -> Variable
cnFreshen variable context =
  if cnAll (tpIsFresh variable) context
    then variable
    else cnFreshen (vrAppend "'" variable) context

cnFreshTypeVariable :: String -> Context Type -> Type
cnFreshTypeVariable seed context =
  TpVariable (cnFreshen (Variable seed) context)

exCollectTypeEquations :: Context Type -> Expression -> Either String (Type, [TypeEquation])
exCollectTypeEquations context expression =
  case expression of
    ExTrue ->
      Right (TpBoolean, [])
    
    ExFalse ->
      Right (TpBoolean, [])
    
    ExCaseBoolean input branchTrue branchFalse ->
      do
        (inputType, inputEquations) <- exCollectTypeEquations context input
        (branchTrueType, branchTrueEquations) <- exCollectTypeEquations context branchTrue
        (branchFalseType, branchFalseEquations) <- exCollectTypeEquations context branchFalse

        let collectedEquations = inputEquations ++ branchTrueEquations ++ branchFalseEquations
        let localEquations = [(inputType, TpBoolean), (branchTrueType, branchFalseType)]

        Right (branchTrueType, collectedEquations ++ localEquations)
    
    ExZero ->
      Right (TpNatural, [])
    
    ExSuccessor input ->
      do
        (inputType, inputEquations) <- exCollectTypeEquations context input

        Right (TpNatural, inputEquations ++ [(inputType, TpNatural)])
    
    ExCaseNatural input branchZero branchSuccessor ->
      do
        (inputType, inputEquations) <- exCollectTypeEquations context input
        (branchZeroType, branchZeroEquations) <- exCollectTypeEquations context branchZero
        (branchSuccessorType, branchSuccessorEquations) <- exCollectTypeEquations context branchSuccessor

        let collectedEquations = inputEquations ++ branchZeroEquations ++ branchSuccessorEquations
        let localEquations = [(inputType, TpNatural), (TpAbstraction TpNatural branchZeroType, branchSuccessorType)]

        Right (branchZeroType, collectedEquations ++ localEquations)
    
    ExVariable variable ->
      do
        variableType <- cnLookup variable context

        Right (variableType, [])
    
    ExAbstraction variableName abstractionBody ->
      do
        let variableType = cnFreshTypeVariable "x" context
        context' <- cnInsert variableName variableType context
        (abstractionBodyType, abstractionBodyEquations) <- exCollectTypeEquations context' abstractionBody

        Right (TpAbstraction variableType abstractionBodyType, abstractionBodyEquations)
    
    ExApplication function argument ->
      do
        let outputType = cnFreshTypeVariable "x" context
        (functionType, functionEquations) <- exCollectTypeEquations context function
        (argumentType, argumentEquations) <- exCollectTypeEquations context argument

        let collectedEquations = functionEquations ++ argumentEquations
        let localEquations = [(functionType, TpAbstraction argumentType outputType)]

        Right (outputType, collectedEquations ++ localEquations)

type Substitution =
  (Variable, Type)

tpApplySubstitution :: Substitution -> Type -> Type
tpApplySubstitution substitution@(key, value) subject =
  case subject of
    TpVariable variable ->
      if key == variable then value else TpVariable variable

    TpBoolean ->
      TpBoolean

    TpNatural ->
      TpNatural

    TpAbstraction function argument ->
      TpAbstraction (tpApplySubstitution substitution function) (tpApplySubstitution substitution argument)

teApplySubstitution :: Substitution -> TypeEquation -> TypeEquation
teApplySubstitution substitution (left, right) =
  (tpApplySubstitution substitution left, tpApplySubstitution substitution right)

teUnify :: [Substitution] -> [TypeEquation] -> Either String [Substitution]
teUnify substitutions typeEquations =
  case typeEquations of
    [] ->
      Right substitutions
    
    (TpVariable variable, TpVariable variable') : typeEquations' | variable == variable' ->
      teUnify substitutions typeEquations'
    
    (TpBoolean, TpBoolean) : typeEquations' ->
      teUnify substitutions typeEquations'
    
    (TpNatural, TpNatural) : typeEquations' ->
      teUnify substitutions typeEquations'
    
    (TpAbstraction inputType1 outputType1, TpAbstraction inputType2 outputType2) : typeEquations' ->
      teUnify substitutions ([(inputType1, inputType2), (outputType1, outputType2)] ++ typeEquations')
    
    (TpVariable variable, source) : typeEquations' ->
      teSolve substitutions variable source typeEquations'
    
    (source, TpVariable variable) : typeEquations' ->
      teSolve substitutions variable source typeEquations'

    typeEquations' ->
      Left ("teUnify: Could not unify supplied type equations [" ++ show typeEquations' ++ "]")

teSolve :: [Substitution] -> Variable -> Type -> [TypeEquation] -> Either String [Substitution]
teSolve substitutions variable source typeEquations =
  if tpIsFresh variable source
    then
      let
        substitution = (variable, source)
      in
        teUnify
          (substitutions ++ [substitution])
          (map (teApplySubstitution substitution) typeEquations)
    else
      Left ("teSolve: variable " ++ show variable ++ "is not fresh in " ++ show source)

tpApplySubstitutions :: [Substitution] -> Type -> Type
tpApplySubstitutions substitutions subject =
  foldl (flip tpApplySubstitution) subject substitutions

exInferType :: Context Type -> Expression -> Either String Type
exInferType context expression =
  do
    (expressionType, expressionEquations) <- exCollectTypeEquations context expression
    substitutions <- teUnify [] expressionEquations

    Right (tpApplySubstitutions substitutions expressionType)