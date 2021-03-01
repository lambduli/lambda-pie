module Main where

import Simply.AST
import Simply.Context
import Simply.Eval
import Simply.Kind
import Simply.Type
import Simply.TypeChecker
import Simply.Value

main :: IO ()
main = do
  putStrLn "Hello λ->"
