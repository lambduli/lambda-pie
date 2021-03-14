module Dependently.Name where


data Name
  = Global String
  | Local Int String
  deriving (Eq)


instance Show Name where
  show (Global name)
    = name
  show (Local ind name)
    = name -- ++ show ind
