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

exCollectEquations :: Context Type -> Expression -> Either String (Type, [TypeEquation])
exCollectEquations context expression =
  case expression of
    ExTrue ->
      Right (TpBoolean, [])
    
    ExFalse ->
      Right (TpBoolean, [])
    
    ExCaseBoolean input true false ->
      do
        (inputType, inputEquations) <- exCollectEquations context input
        (trueType, trueEquations) <- exCollectEquations context true
        (falseType, falseEquations) <- exCollectEquations context false

        let collectedEquations = inputEquations ++ trueEquations ++ falseEquations
        let localEquations = [(inputType, TpBoolean), (trueType, falseType)]

        Right (trueType, collectedEquations ++ localEquations)
    
    ExZero ->
      Right (TpNatural, [])
    
    ExSuccessor input ->
      do
        (inputType, inputEquations) <- exCollectEquations context input

        Right (TpNatural, inputEquations ++ [(inputType, TpNatural)])
    
    ExCaseNatural input zero successor ->
      do
        (inputType, inputEquations) <- exCollectEquations context input
        (zeroType, zeroEquations) <- exCollectEquations context zero
        (successorType, successorEquations) <- exCollectEquations context successor

        let collectedEquations = inputEquations ++ zeroEquations ++ successorEquations

        let inputTypeEquation = (inputType, TpNatural)
        let outputTypeEquation = (TpAbstraction TpNatural zeroType, successorType)
        let localEquations = [inputTypeEquation, outputTypeEquation]

        Right (zeroType, collectedEquations ++ localEquations)
    
    ExVariable variable ->
      do
        variableType <- cnLookup variable context

        Right (variableType, [])
    
    ExAbstraction variable body ->
      do
        let variableType = cnFreshTypeVariable "x" context
        context' <- cnInsert variable variableType context
        (bodyType, bodyEquations) <- exCollectEquations context' body

        Right (TpAbstraction variableType bodyType, bodyEquations)
    
    ExApplication function argument ->
      do
        let outputType = cnFreshTypeVariable "x" context
        (functionType, functionEquations) <- exCollectEquations context function
        (argumentType, argumentEquations) <- exCollectEquations context argument

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
      TpAbstraction
        (tpApplySubstitution substitution function)
        (tpApplySubstitution substitution argument)

teApplySubstitution :: Substitution -> TypeEquation -> TypeEquation
teApplySubstitution substitution (left, right) =
  (tpApplySubstitution substitution left, tpApplySubstitution substitution right)

teUnify :: [Substitution] -> [TypeEquation] -> Either String [Substitution]
teUnify substitutions equations =
  case equations of
    [] ->
      Right substitutions
    
    (TpVariable variable, TpVariable variable') : equations' | variable == variable' ->
      teUnify substitutions equations'
    
    (TpBoolean, TpBoolean) : equations' ->
      teUnify substitutions equations'
    
    (TpNatural, TpNatural) : equations' ->
      teUnify substitutions equations'
    
    (TpAbstraction input1 output1, TpAbstraction input2 output2) : equations' ->
      teUnify substitutions ([(input1, input2), (output1, output2)] ++ equations')
    
    (TpVariable variable, source) : equations' ->
      teSolve substitutions variable source equations'
    
    (source, TpVariable variable) : equations' ->
      teSolve substitutions variable source equations'

    equations' ->
      Left ("teUnify: Could not unify supplied type equations [" ++ show equations' ++ "]")

teSolve :: [Substitution] -> Variable -> Type -> [TypeEquation] -> Either String [Substitution]
teSolve substitutions variable source equations =
  if tpIsFresh variable source
    then
      let
        substitution = (variable, source)
      in
        teUnify
          (substitutions ++ [substitution])
          (map (teApplySubstitution substitution) equations)
    else
      Left ("teSolve: variable " ++ show variable ++ " is not fresh in " ++ show source)

tpApplySubstitutions :: [Substitution] -> Type -> Type
tpApplySubstitutions substitutions subject =
  foldl (flip tpApplySubstitution) subject substitutions

exInferType :: Context Type -> Expression -> Either String Type
exInferType context expression =
  do
    (expressionType, expressionEquations) <- exCollectEquations context expression
    substitutions <- teUnify [] expressionEquations

    Right (tpApplySubstitutions substitutions expressionType)
