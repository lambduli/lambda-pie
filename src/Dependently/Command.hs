module Dependently.Command where

import Dependently.Name
import Dependently.AST


newtype Command = Assume [(Name, Term'Check)]


instance Show Command where
  show (Assume lst)
    = "assume " ++ shw lst
      where
        shw [] = ""
        shw ((name, info) : rest) = "(" ++ show name ++ " :: " ++ show info ++ ")"
