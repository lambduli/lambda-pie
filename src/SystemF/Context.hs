module SystemF.Context where

import SystemF.Name ( Name )
import SystemF.Type ( Type )
import SystemF.Kind ( Kind )


data Info
  = HasKind Kind
  | HasType Type
  deriving (Show)

type Context = [(Name, Info)]
