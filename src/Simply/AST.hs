module Simply.AST where

import Simply.Type ( Type )
import Simply.Name ( Name )


-- Inferable Term
data Term'Infer
  = Term'Check ::: Type
  | Bound Int String
  | Free Name
  | Term'Infer :@: Term'Check
  deriving (Show, Eq)


-- Checkable Term
data Term'Check
  = Inf Term'Infer
  | Lam String Term'Check
  deriving (Show, Eq)
