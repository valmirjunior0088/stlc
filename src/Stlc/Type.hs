module Stlc.Type
  (Type (..)
  )
  where

data Type =
  TpBoolean |
  TpNatural |
  TpAbstraction Type Type
  deriving (Eq, Show)