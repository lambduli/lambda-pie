module SystemF.Type where

import SystemF.Name ( Name(..) )


data Type
  = TFree Name
  | Type :-> Type
  | Forall String Type -- NEW
  deriving (Eq)


instance Show Type where
  show (TFree name)
    = show name
  show (from :-> to)
    = "(" ++ show from ++ " -> " ++ show to ++ ")"
  show (Forall t'par type') -- NEW
    = "(forall " ++ t'par ++ " . " ++ show type' ++ ")" -- NEW


specify'type :: Type -> String -> Type -> Type
specify'type (TFree (Global id)) name rep
  | id == name = rep
  | otherwise = TFree (Global id)
specify'type (from :-> to) name rep
  = specify'type from name rep :-> specify'type to name rep
specify'type (Forall par type') name rep
  | par == name = Forall par type'
  | otherwise = Forall par (specify'type type' name rep)