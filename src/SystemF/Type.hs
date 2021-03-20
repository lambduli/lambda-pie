module SystemF.Type where

import SystemF.Name ( Name )


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