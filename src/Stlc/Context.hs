module Stlc.Context
  (Variable
  ,Context (..)
  ,cnEmpty
  ,cnInsert
  ,cnLookup
  )
  where

import Prelude hiding
  (lookup
  )

import Stlc.Type
  (Type (..)
  )

import Stlc.Variable
  (Variable (..)
  )

import Data.Map
  (Map
  ,empty
  ,insert
  ,lookup
  )

newtype Context =
  Context (Map Variable Type)

cnEmpty :: Context
cnEmpty =
  Context empty

cnInsert :: Variable -> Type -> Context -> Either String Context
cnInsert key value (Context context) =
  case lookup key context of
    Nothing -> Right (Context (insert key value context))
    Just _ -> Left ("Context: variable '" ++ show key ++ "' already exists")

cnLookup :: Variable -> Context -> Either String Type
cnLookup key (Context context) =
  case lookup key context of
    Nothing -> Left ("Context: variable '" ++ show key ++ "' does not exist")
    Just value -> Right value
