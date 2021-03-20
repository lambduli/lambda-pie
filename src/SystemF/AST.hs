module SystemF.AST where

import SystemF.Type ( Type )
import SystemF.Name ( Name )


-- Inferable Term
data Term'Infer
  = Term'Check ::: Type
  | Type Type -- NEW
  | Bound Int String
  | Free Name
  | Term'Infer :@: Term'Check
  | Term'Infer :$: Term'Infer -- NEW  Term'Infer - Type only
  | TyLam String Term'Infer -- NEW
  | LamAnn String Type Term'Infer
  deriving (Eq)


instance Show Term'Infer where
  show (term ::: type')
    = show term ++ " :: " ++ show type'
  show (Type type') -- NEW
    = "[" ++ show type' ++ "]" -- NEW
  show (Bound ind name)
    = name ++ show ind
  show (Free name)
    = show name
  show (left :@: r@(Inf (r'l :@: r'r)))
    = show left ++ " (" ++ show r ++ ")"
  show (left :@: right)
    = show left ++ " " ++ show right
  show (left :$: t'right) -- NEW
    = "(" ++ show left ++ " " ++ show t'right ++ ")" -- NEW
  show (TyLam t'par body) -- NEW
    = "(Λ " ++ t'par ++ " . " ++ show body ++ ")" -- NEW
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