module Stlc.Variable
  (Variable (..)
  )
  where

newtype Variable =
  Variable String
  deriving (Eq, Ord)

instance Show Variable where
  show (Variable string) =
    string