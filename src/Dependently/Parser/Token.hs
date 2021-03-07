module Dependently.Parser.Token where


data Token
  = Identifier String
  | Lambda
  | DoubleColon
  | Dot
  | Forall
  | LeftParen
  | RightParen
  | Star
  | Arrow

  | Assume

  | EOF
  deriving (Show, Eq)