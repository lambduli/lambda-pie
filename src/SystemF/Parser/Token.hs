module SystemF.Parser.Token where


data Token
  = Identifier String
  | Lambda
  | TyLam -- NEW
  | DoubleColon
  | LeftParen
  | RightParen
  | LeftBracket -- NEW
  | RightBracket -- NEW
  | Star
  | Arrow
  | Forall -- NEW
  | Dot -- NEW

  | Assume

  | EOF
  deriving (Show, Eq)