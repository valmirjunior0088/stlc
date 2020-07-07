module LChurch.Syntax
  (Variable
  ,Type (..)
  ,Expression (..)
  )
  where

type Variable =
  String

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
