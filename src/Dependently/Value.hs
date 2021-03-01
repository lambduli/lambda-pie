module Dependently.Value where

import Dependently.AST


type Env = [Value]


data Value
  = Star
  | Pi String Value Term'Check Env
  | Lam String Term'Check Env
  | Free String
  | App Value Value
  deriving (Show)
