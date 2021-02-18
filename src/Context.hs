module Context where

import Type
import Kind

data Info
  = HasKind Kind
  | HasType Type
  deriving (Show)

type Context = [(String, Info)]