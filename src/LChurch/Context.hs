module LChurch.Context
  (Context (..)
  ,cnInsert
  ,cnLookup
  ,cnEmpty
  )
  where

import Prelude hiding
  (lookup
  )

import LChurch.Syntax
  (Variable
  ,Type (..)
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
    Just _ -> Left ("Context: variable '" ++ key ++ "' already exists")

cnLookup :: Variable -> Context -> Either String Type
cnLookup key (Context context) =
  case lookup key context of
    Nothing -> Left ("Context: variable '" ++ key ++ "' does not exist")
    Just value -> Right value
