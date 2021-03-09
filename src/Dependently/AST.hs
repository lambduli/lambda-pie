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
  | LamAnn String Term'Check Term'Infer
  deriving (Eq)


instance Show Term'Infer where
  show (term ::: type')
    = show term ++ " :: " ++ show type'
  show Star
    = "*"
  show (Pi par in'type out'type)
    = "Pi " ++ par ++ " :: " ++ show in'type ++ " . " ++ show out'type
  show (Bound ind name)
    = name ++ show ind
  show (Free name)
    = show name
  show (left :@: r@(Inf (r'l :@: r'r)))
    = show left ++ " (" ++ show r ++ ")"
  show (left :@: right)
    = show left ++ " " ++ show right
  show (LamAnn par in'type body)
    = "(λ " ++ par ++ " :: " ++ show in'type ++ " -> " ++ show body ++ ")"


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