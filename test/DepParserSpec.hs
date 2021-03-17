module DepParserSpec where

import Test.Hspec
import System.Exit

import Dependently.Parser.Parser (parse'expr)
import Dependently.AST
import Dependently.Name


spec :: Spec
spec = describe "Test the parser" $ do
  it "Parses a lambda function" $ do
    "(lambda c -> c)" <=>
      Lam "c" (Inf (Bound 0 "c"))

  it "Parses an APP sequence" $ do
    "(a b c d)" <=>
      let
        a = free'var "a"
        b = Inf $ free'var "b"
        c = Inf $ free'var "c"
        d = Inf $ free'var "d"
      in
        Inf $ a :@: b :@: c :@: d

  it "Parses an application to lambda" $ do
    "(a (lambda c -> c))" <=>
      Inf (free'var "a" :@: Lam "c" (Inf (Bound 0 "c")))

  it "Parses a complicated app sequence" $ do
    "(a b c (d e (f g h)))" <=>
      let
        a = free'var "a"
        b = Inf $ free'var "b"
        c = Inf $ free'var "c"
        d = free'var "d"
        e = Inf $ free'var "e"
        f = free'var "f"
        g = Inf $ free'var "g"
        h = Inf $ free'var "h"
      in
        Inf $ a :@: b :@: c :@: Inf (d :@: e :@: Inf (f :@: g :@: h))

  it "Parses an annotated term" $ do
    "x :: Foo" <=>
      Inf (Inf (free'var "x") ::: Inf (free'var "Foo"))

  it "Parses an annotated application" $ do
    "(x y z) :: Foo" <=>
      let
        x = free'var "x"
        y = Inf $ free'var "y"
        z = Inf $ free'var "z"
        foo = Inf $ free'var "Foo"
      in Inf (Inf (x :@: y :@: z) ::: foo)

  it "Parses a lambda with annotation" $ do
    "(\\ x -> x :: Foo)" <=>
      let
        x = Bound 0 "x"
        foo = Inf $ free'var "Foo"
        ann = Inf $ Inf x ::: foo
        lam = Lam "x" ann
      in lam

  it "Parses a Pi type" $ do
    "(forall Foo :: * . Foo)" <=>
      Inf (Pi "Foo" (Inf Star) (Inf (Bound 0 "Foo" )))

  it "Parses an annotated lambda" $ do
    "(lambda c -> c) :: (forall c :: B . B)" <=>
      let
        lam = Lam "c" (Inf (Bound 0 "c"))
        pi = Inf (Pi "c" (Inf (free'var "B")) (Inf (free'var "B")))
      in
        Inf (lam ::: pi)

  it "Parses and application of annotated lambda to lambda" $ do
    "(((lambda c -> c) :: (forall c :: B . B)) (lambda e -> e))" <=>
      let
        lam = Lam "c" (Inf (Bound 0 "c"))
        pi = Inf (Pi "c" (Inf (free'var "B")) (Inf (free'var "B")))
        left = lam ::: pi
        right = Lam "e" (Inf (Bound 0 "e"))
      in
        Inf $ left :@: right

  describe "Just parses:" $ do
    it "((((\\ c -> c) :: (forall c :: B . B)) d e f) g h i j)" $ do
      just'parses "((((\\ c -> c) :: (forall c :: B . B)) d e f) g h i j)"
    it "(a :: Foo (\\ x -> x) ((\\ c -> c) :: (forall c :: B . B) d e f) g h i j)" $ do
      just'parses "(a :: Foo (\\ x -> x) ((\\ c -> c) :: (forall c :: B . B) d e f) g h i j)"
    it "(a :: Foo (b :: Boo) (c :: Coo)) :: FBC" $ do
      just'parses "(a :: Foo (b :: Boo) (c :: Coo)) :: FBC"
    it "(lambda x -> x) :: (forall (x :: *) . *)" $ do
      just'parses "(lambda x -> x) :: (forall (x :: *) . *)"
    it "(lambda t x -> x) :: (forall (t :: *) . (forall (x :: t) . t))" $ do
      just'parses "(lambda t x -> x) :: (forall (t :: *) . (forall (x :: t) . t))"
    it "(lambda t x -> x :: *) :: (forall (t :: *) . (forall (x :: t) . t))" $ do
      just'parses "(lambda t x -> x :: *) :: (forall (t :: *) . (forall (x :: t) . t))"
    it "(lambda x -> x) :: (forall (t :: Bool) . t)" $ do
      just'parses "(lambda x -> x) :: (forall (t :: Bool) . t)"
    it "(lambda x -> x) :: (forall (t :: Bool) . Bool)" $ do
      just'parses "(lambda x -> x) :: (forall (t :: Bool) . Bool)"
    it "(lambda t x -> x :: Bool) :: (forall (t :: Bool) . (forall (x :: t) . t))" $ do
      just'parses "(lambda t x -> x :: Bool) :: (forall (t :: Bool) . (forall (x :: t) . t))"
    it "(lambda t x -> t :: Bool) :: (forall (t :: Bool) . (forall (x :: t) . t))" $ do
      just'parses "(lambda t x -> t :: Bool) :: (forall (t :: Bool) . (forall (x :: t) . t))"
    it "(lambda t x -> t :: Bool) :: (forall (t :: Bool) . (forall (x :: t) . Bool))" $ do
      just'parses "(lambda t x -> t :: Bool) :: (forall (t :: Bool) . (forall (x :: t) . Bool))"
    it "(lambda t x -> t) :: (forall (t :: Bool) . (forall (x :: t) . Bool))" $ do
      just'parses "(lambda t x -> t) :: (forall (t :: Bool) . (forall (x :: t) . Bool))"
    it "(lambda t x -> x :: True) :: (forall (t :: Bool) . (forall (x :: t) . t))" $ do
      just'parses "(lambda t x -> x :: True) :: (forall (t :: Bool) . (forall (x :: t) . t))"
    it "(lambda t x -> x) :: (forall (t :: Bool) . (forall (x :: t) . t))" $ do
      just'parses "(lambda t x -> x) :: (forall (t :: Bool) . (forall (x :: t) . t))"
    it "(lambda t x -> Tr :: True) :: (forall (t :: Bool) . (forall (x :: t) . t))" $ do
      just'parses "(lambda t x -> Tr :: True) :: (forall (t :: Bool) . (forall (x :: t) . t))"
    it "(lambda t x -> t :: Bool) :: (forall (t :: Bool) . (forall (x :: *) . Bool))" $ do
      just'parses "(lambda t x -> t :: Bool) :: (forall (t :: Bool) . (forall (x :: *) . Bool))"
    it "(lambda t x -> t) :: (forall (t :: Bool) . (forall (x :: *) . Bool))" $ do
      just'parses "(lambda t x -> t) :: (forall (t :: Bool) . (forall (x :: *) . Bool))"
    it "(lambda t x -> x :: T) :: (forall t :: T . (forall x :: t . t))" $ do
      just'parses "(lambda t x -> x :: T) :: (forall t :: T . (forall x :: t . t))"
    it "(lambda t x -> x) :: (forall t :: T . (forall x :: t . t))" $ do
      just'parses "(lambda t x -> x) :: (forall t :: T . (forall x :: t . t))"
    it "(lambda t x -> x :: T) :: (forall t :: * . (forall x :: t . t))" $ do
      just'parses "(lambda t x -> x :: T) :: (forall t :: * . (forall x :: t . t))"
    it "(lambda t x -> x) :: (forall t :: * . (forall x :: t . t))" $ do
      just'parses "(lambda t x -> x) :: (forall t :: * . (forall x :: t . t))"
    it "(lambda t x -> x) :: (forall t :: * . (forall x :: t . *))" $ do
      just'parses "(lambda t x -> x) :: (forall t :: * . (forall x :: t . *))"




free'var :: String -> Term'Infer
free'var name = Free $ Global name

just'parses :: String -> IO ()
just'parses expr = do
  case parse'expr expr of
    Left _ -> return ()
    Right _ -> return ()



infix 4 <=>

(<=>) :: String -> Term'Check -> IO ()
(<=>) expr ast = do
  case parse'expr expr of
    Left cmd -> exitFailure
    Right ast' -> ast' `shouldBe` ast
