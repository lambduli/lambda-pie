module Dependently.Name where


data Name
  = Global String
  | Local Int String
  deriving (Show, Eq)
