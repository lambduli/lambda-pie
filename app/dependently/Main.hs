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

  -- print $ parse "assume Foo :: *"
  -- print $ parse "assume Foo :: Bar"
  -- print $ parse "assume (Foo :: *) (Bar :: Foo) (Baz :: Foo)"

  print "@@@@@@@@@@@@@@@"

  -- print $ parse "(lambda (c :: Bar) -> c)"
  -- print $ parse "(a b (c d e) f g h)"
  -- print $ parse "(\\ x :: Foo -> x :: Foo) :: (forall x :: Foo . Foo)"

  -- print $ parse "(\\ c :: B -> c) :: (forall c :: B . B)"
  -- print $ parse "((\\ i :: A -> i) (\\ c :: X -> c)) :: (forall c :: B . B)"

  -- print $ parse "(lambda (x :: Foo) (y :: Bar) -> x)"
  print "..............."
  print "???????????????"


  print $ parse "(lambda t x -> x :: *) :: (forall (t :: *) . (forall (x :: t) . t))" -- doesn't

  print $ parse "(lambda x -> x) :: (forall (t :: Bool) . t)" -- doesn't

  print $ parse "(lambda t x -> x :: Bool) :: (forall (t :: Bool) . (forall (x :: t) . t))" -- doesn't
  print $ parse "(lambda t x -> t :: Bool) :: (forall (t :: Bool) . (forall (x :: t) . t))" -- doesn't
  print $ parse "(lambda t x -> t :: Bool) :: (forall (t :: Bool) . (forall (x :: t) . Bool))" -- doesn't
  print $ parse "(lambda t x -> t) :: (forall (t :: Bool) . (forall (x :: t) . Bool))" -- doesn't
  
  print $ parse "(lambda t x -> x :: True) :: (forall (t :: Bool) . (forall (x :: t) . t))" -- doesn't, why?
  print $ parse "(lambda t x -> x) :: (forall (t :: Bool) . (forall (x :: t) . t))" -- doesn't but why?

  -- (lambda t x -> x :: True) (True :: Bool) (Tr :: True) --> True
  print $ parse "(lambda t x -> Tr :: True) :: (forall (t :: Bool) . (forall (x :: t) . t))" -- doesn't, why?

  print $ parse "(lambda t x -> x :: t) :: (forall t :: T . (forall x :: T . T))" -- t is unknown identifier
  print $ parse "(lambda t x -> x) :: (forall t :: T . (forall x :: t . t))" -- WHY NOT?

  -- print $ parse "(lambda t x -> x :: T) :: (forall t :: * . (forall x :: t . t))" -- can't check -- because wrong

  -- print $ parse "(lambda t x -> x) :: (forall t :: * . (forall x :: t . *))" -- can't -- This doesn't type check

  -- "(lambda x -> x) :: (forall x :: T . T)"

  repl []


parse str = parse'expr str

readExpression :: IO String
readExpression = do
  putStr "λΠ >> "
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
          print expr
          -- try to find and print the type
          case type'of expr context of
            Left err -> do putStrLn $ "       Type Error: " ++ err
            Right type' -> do
              let val = eval expr
              putStrLn $ "       " ++ show val ++ " :: " ++ show type'
          repl context