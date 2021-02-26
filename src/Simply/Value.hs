module Simply.Value where

import Simply.AST


type Env = [Value]


data Value
  = Lam String Term'Check Env
  | Free String
  | App Value Value
  deriving (Show)
