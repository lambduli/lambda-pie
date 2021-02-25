module Simply.Type where

import Simply.Name ( Name )


data Type
  = TFree Name
  | Type :-> Type
  deriving (Show, Eq)
