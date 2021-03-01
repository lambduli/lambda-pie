module Main where

import Dependently.AST
import Dependently.Context
import Dependently.Eval
import Dependently.TypeChecker
import Dependently.Value

main :: IO ()
main = do
  putStrLn "Hello λΠ"