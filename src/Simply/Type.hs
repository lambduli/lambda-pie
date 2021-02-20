module Simply.Type where


data Type
  = TVar String
  | Type :-> Type
  deriving (Show, Eq)
