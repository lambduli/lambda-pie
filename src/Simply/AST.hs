module Simply.AST where

import Simply.Type ( Type )
import Simply.Name ( Name )


-- Inferable Term
data Term'Infer
  = Term'Check ::: Type
  | Bound Int String
  | Free Name
  | Term'Infer :@: Term'Check
  deriving (Eq)


instance Show Term'Infer where
  show (term ::: type')
    = show term ++ " :: " ++ show type'
  show (Bound ind name)
    = name ++ show ind
  show (Free name)
    = show name
  show (left :@: right)
    = "(" ++ show left ++ " " ++ show right ++ ")"


-- Checkable Term
data Term'Check
  = Inf Term'Infer
  | Lam String Term'Check
  deriving (Eq)


instance Show Term'Check where
  show (Inf term)
    = show term
  show (Lam par body)
    = "(λ " ++ par ++ " -> " ++ show body ++ ")"