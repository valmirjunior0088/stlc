module Stlc.Variable
  (Variable (..)
  ,vrAppend
  )
  where

newtype Variable =
  Variable String
  deriving (Eq, Ord)

instance Show Variable where
  show (Variable string) =
    string

vrAppend :: String -> Variable -> Variable
vrAppend string (Variable variable) =
  Variable (variable ++ string)