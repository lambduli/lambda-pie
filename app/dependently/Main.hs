module Main where

import System.IO
import Data.Bifunctor (second)

import Dependently.Parser.Parser (parse'expr)
import Dependently.AST
import Dependently.Context
import Dependently.Eval
import Dependently.TypeChecker
import Dependently.Value
import Dependently.Command

main :: IO ()
-- main = do
--   putStrLn "Hello λΠ"

--   main :: IO ()
main = do
  putStrLn "REPL for λΠ"
  putStrLn ""

  -- print $ parse'expr "(lambda c -> c)"
  -- print $ parse'expr "(lambda (c :: Bar) -> c)"
  -- print $ parse'expr "a (lambda c -> c)"
  -- print $ parse'expr "a b c (d e (f g h)) (i j) k"
  print $ parse'expr "a b c d e"
  print $ parse'expr "a b (c d e) f g h"
  print $ parse'expr "(\\ x -> x)"
  print $ parse'expr "((\\ c -> c) :: (B -> B))"
  -- print $ parse'expr "(((\\ c -> c) :: (B -> B)) d e f) g h i j"
  -- print $ parse'expr "(a :: Foo) (\\ x -> x) (((\\ c -> c) :: (B -> B)) d e f) g h i j"
  -- putStrLn ""
  -- putStrLn $ show $ parse'expr "(lambda (x :: Foo) (y :: Bar) -> x)"

  -- repl []


readExpression :: IO String
readExpression = do
  putStr "λ2 >> "
  hFlush stdout
  getLine


repl :: Context -> IO ()
repl context = do
  -- read
  line <- readExpression
  if line == ":exit"
    then return ()
    else do
      -- evaluate
      let command'or'expr = parse'expr line
      case command'or'expr of
        Left (Assume assumptions) -> do
          repl $ map (second eval) assumptions ++ context
        Right expr -> do
          -- try to find and print the type
          case type'of expr context of
            Left err -> do putStrLn $ "       Type Error: " ++ err
            Right type' -> do
              let val = eval expr
              putStrLn $ "       " ++ show val ++ " :: " ++ show type'
          repl context