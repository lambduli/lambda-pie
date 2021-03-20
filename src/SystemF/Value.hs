module SystemF.Value where

import SystemF.AST hiding (Lam, Free, TyLam)
import SystemF.Type


type Env = [Value]


data Value
  = Lam String Term'Check Env
  | TyLam String Term'Check Env -- NEW
  | TypeArg Type -- NEW
  | Free String
  | App Value Value
  | TyApp Value Value -- NEW -- second Value is only ever be TypeArg


instance Show Value where
  show (Lam _ _ _)
    = "<lambda>"
  show (TyLam _ _ _) -- NEW
    = "<type lambda>" -- NEW
  show (TypeArg t) -- NEW
    = "[" ++ show t ++ "]" -- NEW
  show (Free name)
    = name
  show (App left right)
    = "(" ++ show left ++ " " ++ show right ++ ")"
  show (TyApp left right't)
    = "(" ++ show left ++ " " ++ show right't ++ ")" -- NEW
