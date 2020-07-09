module Stlc.Context
  (Context (..)
  ,cnEmpty
  ,cnInsert
  ,cnLookup
  ,cnAll
  )
  where

import Prelude hiding
  (lookup
  )

import Data.Map.Strict
  (Map
  ,empty
  ,member
  ,insert
  ,lookup
  ,elems
  )

import Stlc.Variable
  (Variable (..)
  )

newtype Context a =
  Context (Map Variable a)

cnEmpty :: Context a
cnEmpty =
  Context empty

cnInsert :: Variable -> a -> Context a -> Either String (Context a)
cnInsert key value (Context context) =
  if member key context
    then Left ("Context: variable '" ++ show key ++ "' already exists")
    else Right (Context (insert key value context))

cnLookup :: Variable -> Context a -> Either String a
cnLookup key (Context context) =
  case lookup key context of
    Nothing -> Left ("Context: variable '" ++ show key ++ "' does not exist")
    Just value -> Right value

cnAll :: (a -> Bool) -> Context a -> Bool
cnAll predicate (Context context) =
  all predicate (elems context)