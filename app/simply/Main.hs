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
  -- putStrLn $ show $ parse'expr "(((a b) c) d) :: Foo"
  -- putStrLn ""

  repl []


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
            Left err -> do putStrLn $ "    Type Error: " ++ err
            Right type' -> do putStrLn $ "    :: " ++ show type'
          repl context

-- assume (Foo :: *) (foo :: Foo)
-- ( ((lambda x -> x) :: Foo -> Foo) foo )

-- assume (Bool :: *) (True :: Bool) (False :: Bool)
