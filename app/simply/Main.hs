module Main where

import System.IO
import Simply.Parser.Parser (parse'expr)
import Simply.Eval
import Simply.TypeChecker
import Simply.Value
import Simply.Context
import Simply.Command

main :: IO ()
main = do
  putStrLn "REPL for λ->"
  putStrLn ""

  print $ parse'expr "(lambda c -> c)"
  print $ parse'expr "(lambda (c :: Bar) -> c)"
  print $ parse'expr "a (lambda c -> c)"
  print $ parse'expr "a b c (d e (f g h)) (i j) k"
  print $ parse'expr "a :: Foo (\\ x -> x) ((\\ c -> c) :: (B -> B) d e f) g h i j"
  -- putStrLn ""
  -- putStrLn $ show $ parse'expr "(lambda (x :: Foo) (y :: Bar) -> x)"

  -- repl []


readExpression :: IO String
readExpression = do
  putStr "λ-> >> "
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
          repl $ assumptions ++ context
        Right expr -> do
          -- try to find and print the type
          case type'of expr context of
            Left err -> do putStrLn $ "       Type Error: " ++ err
            Right type' -> do
              let val = eval expr
              putStrLn $ "       " ++ show val ++ " :: " ++ show type'
          repl context

-- assume (Foo :: *) (foo :: Foo)
-- ( ((lambda x -> x) :: Foo -> Foo) foo )

-- assume (Bool :: *) (True :: Bool) (False :: Bool)

-- (lambda (x :: Foo) (y :: Bar) -> x)