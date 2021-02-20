module Simply.Context where

import Simply.Type
import Simply.Kind

data Info
  = HasKind Kind
  | HasType Type
  deriving (Show)

type Context = [(String, Info)]