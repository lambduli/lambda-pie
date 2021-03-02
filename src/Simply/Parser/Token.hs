module Simply.Parser.Token where


data Token
  = Identifier String
  | Lambda
  | DoubleColon
  | LeftParen
  | RightParen
  | Star
  | Arrow

  | Assume

  | EOF
  deriving (Show, Eq)