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
main = do
  putStrLn "REPL for λΠ"
  putStrLn ""

  print $ parse "assume Foo :: *"
  print $ parse "assume Foo :: Bar"
  print $ parse "assume (Foo :: *) (Bar :: Foo) (Baz :: Foo)"

  print "@@@@@@@@@@@@@@@"

  print $ parse "(lambda c -> c)"
  print $ parse "(lambda (c :: Bar) -> c)"
  print $ parse "a (lambda c -> c)"
  print $ parse "a b c (d e (f g h)) (i j) k"
  print $ parse "a b c d e"
  print $ parse "a b (c d e) f g h"
  print $ parse "(\\ x -> x)"
  print $ parse "(x :: Foo)"
  print $ parse "x :: Foo"
  print $ parse "(x y z) :: Foo"
  print $ parse "a b c :: Alp"
  print $ parse "(\\ x -> x :: Foo)"
  print $ parse "(forall Foo :: * . Foo)"
  print $ parse "(\\ x :: Foo -> x :: Foo) :: (forall x :: Foo . Foo)"
  print $ parse "(lambda c -> c) :: (forall c :: B . B)"

  print "---------------"
  print $ parse "((lambda c -> c) :: (forall c :: B . B)) (lambda e -> e)"


  print $ parse "(\\ c :: B -> c) :: (forall c :: B . B)"
  print $ parse "((\\ i :: A -> i) (\\ c :: X -> c)) :: (forall c :: B . B)"

  print "..............."
  print $ parse "(((\\ c -> c) :: (forall c :: B . B)) d e f) g h i j"
  print $ parse "(a :: Foo) (\\ x -> x) (((\\ c -> c) :: (forall c :: B . B)) d e f) g h i j"
  print $ parse "(lambda (x :: Foo) (y :: Bar) -> x)"
  print "???????????????"

  -- repl []


parse str = parse'expr str

readExpression :: IO String
readExpression = do
  putStr "λ2 >> "
  hFlush stdout
  getLine


-- repl :: Context -> IO ()
-- repl context = do
--   -- read
--   line <- readExpression
--   if line == ":exit"
--     then return ()
--     else do
--       -- evaluate
--       let command'or'expr = parse'expr line
--       case command'or'expr of
--         Left (Assume assumptions) -> do
--           repl $ map (second eval) assumptions ++ context
--         Right expr -> do
--           -- try to find and print the type
--           case type'of expr context of
--             Left err -> do putStrLn $ "       Type Error: " ++ err
--             Right type' -> do
--               let val = eval expr
--               putStrLn $ "       " ++ show val ++ " :: " ++ show type'
--           repl context