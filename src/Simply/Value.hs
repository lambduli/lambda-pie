module Simply.Value where

import Simply.AST hiding (Lam, Free)


type Env = [Value]


data Value
  = Lam String Term'Check Env
  | Free String
  | App Value Value


instance Show Value where
  show (Lam _ _ _)
    = "<lambda>"
  show (Free name)
    = name
  show (App left right)
    = "(" ++ show left ++ " " ++ show right ++ ")"
