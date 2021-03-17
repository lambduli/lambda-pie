module DepTypeCheckSpec where


import Test.Hspec
import System.Exit
import Data.Bifunctor (bimap)

import Dependently.Parser.Parser (parse'expr)
import Dependently.AST
import Dependently.TypeChecker
import qualified Dependently.Value as Val
import Dependently.Context
import Dependently.Name
import Dependently.Eval


spec :: Spec
spec = describe "Test typechecking" $ do
  it "(lambda x -> x) :: (forall (x :: *) . *)" $ do
    type'checks [] "(lambda x -> x) :: (forall (x :: *) . *)"

  it "(lambda t x -> x) :: (forall (t :: *) . (forall (x :: t) . t))" $ do
    type'checks [] "(lambda t x -> x) :: (forall (t :: *) . (forall (x :: t) . t))"

  it "(lambda t x -> x :: t) :: (forall (t :: *) . (forall (x :: t) . t))" $ do
    type'checks [] "(lambda t x -> x) :: (forall (t :: *) . (forall (x :: t) . t))"

  it "(lambda t x -> t) :: (forall t :: * . (forall x :: t . *))" $ do
    type'checks [] "(lambda t x -> t) :: (forall t :: * . (forall x :: t . *))"

  it "(lambda x -> x) :: (forall (t :: Bool) . Bool)" $ do
    type'checks (to'context [("Bool", "*")]) "(lambda x -> x) :: (forall (t :: Bool) . Bool)"

  it "(lambda x -> x) :: (forall (x :: *) . *)" $ do
    type'checks [] "(lambda x -> x) :: (forall (x :: *) . *)"

  it "(lambda t x -> t :: Bool) :: (forall (t :: Bool) . (forall (x :: *) . Bool))" $ do
    type'checks (to'context [("Bool", "*")]) "(lambda t x -> t :: Bool) :: (forall (t :: Bool) . (forall (x :: *) . Bool))"

  it "(lambda t x -> t) :: (forall (t :: Bool) . (forall (x :: *) . Bool))" $ do
    type'checks (to'context [("Bool", "*")]) "(lambda t x -> t) :: (forall (t :: Bool) . (forall (x :: *) . Bool))"

  it "(lambda t x -> x) :: (forall t :: * . (forall x :: t . t))" $ do
    type'checks [] "(lambda t x -> x) :: (forall t :: * . (forall x :: t . t))"

  it "(lambda x -> x :: T) :: (forall x :: T . T)" $ do
    type'checks (to'context [("T", "*")]) "(lambda x -> x :: T) :: (forall x :: T . T)"

  it "(lambda t x -> x :: T) :: (forall t :: T . (forall x :: T . T))" $ do
    type'checks (to'context [("T", "*")]) "(lambda t x -> x :: T) :: (forall t :: T . (forall x :: T . T))"




to'context :: [(String, String)] -> Context
to'context = map $ bimap to'name to'type


to'name :: String -> Name
to'name str
  = let Right (Inf (Free name)) = parse'expr str
    in name


to'type :: String -> Type
to'type str
  = case parse'expr str of
      Right ast ->
        eval ast


type'checks :: Context -> String -> IO ()
type'checks context expr = do
  case parse'expr expr of
    Right ast ->
      case type'of ast context of
            Left err -> exitFailure
            Right type' -> return ()
