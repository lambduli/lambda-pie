module Simply.Command where

import Simply.Name
import Simply.Context


newtype Command = Assume [(Name, Info)]


instance Show Command where
  show (Assume lst)
    = "assume " ++ shw lst
      where
        shw [] = ""
        shw ((name, info) : rest) = "(" ++ show name ++ " :: " ++ show info ++ ")"