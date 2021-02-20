module Simply.Value where


data Value
  = Lam String Value
  | Var String
  | App Value Value
  deriving (Show)
