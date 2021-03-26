module SystemF.Value where

import SystemF.AST hiding (Lam, Free, TyLam)
import SystemF.Type


type Env = [Value]


data Value
  = Lam String Term'Check Env
  | TyLam String Term'Infer Env -- NEW
  | Free String
  | App Value Value
  | TyApp Value Type -- NEW -- second Value is only ever be TypeArg


instance Show Value where
  show (Lam _ _ _)
    = "<lambda>"
  show (TyLam _ _ _) -- NEW
    = "<type lambda>" -- NEW
  show (Free name)
    = name
  show (App left right)
    = "(" ++ show left ++ " " ++ show right ++ ")"
  show (TyApp left right't) -- NEW
    = "(" ++ show left ++ " [" ++ show right't ++ "])" -- NEW
