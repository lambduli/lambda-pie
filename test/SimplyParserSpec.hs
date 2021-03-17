module SimplyParserSpec where

import Test.Hspec
import System.Exit

import Simply.Parser.Parser (parse'expr)
import Simply.AST
import Simply.Name


spec :: Spec
spec = describe "Just parses:" $ do
  it "a" $ do
    just'parses "a"
  it "a b c" $ do
    just'parses "a b c"
  it "(a b c)" $ do
    just'parses "(a b c)"
  it "(a b c)" $ do
    just'parses "(a b c)"
  it "(a :: T)" $ do
    just'parses "(a :: T)"
  it "(\\ a -> a)" $ do
    just'parses "(\\ a -> a)"

  it "((\\ a -> a) :: T -> T)" $ do
    just'parses "((\\ a -> a) :: T -> T)"
  it "((\\ a -> a) :: (T -> T))" $ do
    just'parses "((\\ a -> a) :: (T -> T))"
  it "((\\ a -> a) :: T -> T) b" $ do
    just'parses "((\\ a -> a) :: T -> T) b"
  it "(\\ a -> a) :: T -> T b" $ do
    just'parses "(\\ a -> a) :: T -> T b"
  it "((\\ a -> a) :: T -> T) b c d" $ do
    just'parses "((\\ a -> a) :: T -> T) b c d"
  it "((\\ a -> a) :: T -> T) (b :: T) c d" $ do
    just'parses "((\\ a -> a) :: T -> T) (b :: T) c d"




just'parses :: String -> IO ()
just'parses expr = do
  case parse'expr expr of
    Left _ -> return ()
    Right _ -> return ()
