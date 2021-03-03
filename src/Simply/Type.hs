module Simply.Type where

import Simply.Name ( Name )


data Type
  = TFree Name
  | Type :-> Type
  deriving (Eq)


instance Show Type where
  show (TFree name)
    = show name
  show (from :-> to)
    = "(" ++ show from ++ " -> " ++ show to ++ ")"