module Dependently.Context where

import Dependently.Value
import Dependently.Name

type Type = Value

type Context = [(Name, Type)]