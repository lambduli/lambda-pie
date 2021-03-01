module Dependently.AST where

import Dependently.Name


-- Inferable Term
data Term'Infer
  = Term'Check ::: Term'Check
  | Star
  | Pi String Term'Check Term'Check
  | Bound Int String
  | Free Name
  | Term'Infer :@: Term'Check
  deriving (Show, Eq)


-- Checkable Term
data Term'Check
  = Inf Term'Infer
  | Lam String Term'Check
  deriving (Show, Eq)
