module AST where

import Type


-- Inferable Term
data Term'Infer
  = Term'Check ::: Type
  | Var String
  | Term'Infer :@: Term'Check
  deriving (Show, Eq)


-- Checkable Term
data Term'Check
  = Inf Term'Infer
  | Lam Term'Check
  deriving (Show, Eq)
