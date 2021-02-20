module Simply.AST where

import Simply.Type


-- Inferable Term
data Term'Infer
  = Term'Check ::: Type
  | Var String
  | Term'Infer :@: Term'Check
  deriving (Show, Eq)


-- Checkable Term
data Term'Check
  = Inf Term'Infer
  | Lam String Term'Check
  deriving (Show, Eq)
