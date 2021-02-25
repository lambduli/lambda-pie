module Simply.Context where

import Simply.Name ( Name )
import Simply.Type ( Type )
import Simply.Kind ( Kind )


data Info
  = HasKind Kind
  | HasType Type
  deriving (Show)

type Context = [(Name, Info)]
