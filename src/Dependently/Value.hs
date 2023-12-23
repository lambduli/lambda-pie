module Dependently.Value where

import Dependently.AST (Term'Check)


type Env = [Value]


data Value
  = Star
  | Pi String Value Term'Check Env
  | Lam String Term'Check Env
  | Free String
  | App Value Value

-- TODO: WIP
instance Show Value where
  show Star
    = "*"
  show (Pi par in'type out'type [])
    = "(Π " ++ par ++ " :: " ++ show in'type ++ " . " ++ show out'type ++ ")"
  show (Pi par in'type out'type env)
    = "(Π " ++ par ++ " :: " ++ show in'type ++ " . " ++ show out'type ++ ")[" ++ show env ++ "]"
  show (Lam par body [])
    = "(λ " ++ par ++ " -> " ++ show body ++ ")"
  show (Lam par body env)
    = "(λ " ++ par ++ " -> " ++ show body ++ ")[" ++ show env ++ "]"
  show (Free name)
    = name
  show (App left right)
    = "(" ++ show left ++ " @ " ++ show right ++ ")"
