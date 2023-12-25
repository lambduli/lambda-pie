module Dependently.TypeChecker where

import Control.Monad

import Dependently.Context
import Dependently.AST
import qualified Dependently.Value as Val
import Dependently.Name
import Dependently.Eval
import Dependently.Substitute

-- import Debug.Trace ( trace, traceM )


type Result a = Either String a

throwError :: String -> Result a
throwError = Left


type'infer'0 :: Context -> Term'Infer -> Result Type
type'infer'0 = type'infer 0


type'infer :: Int -> Context -> Term'Infer -> Result Type
type'infer level context (e ::: type') = do
  -- traceM ("annotated  e= " ++ show e)
  -- traceM ("           type= " ++ show type')
  -- traceM ("context= " ++ show context)
  -- traceM "___________________________________"

  type'check level context type' Val.Star
  -- traceM ("past the type'check line  ")
  let type'' = eval'check type' []
  -- traceM ("past eval'check line  type''= " ++ show type'')
  type'check level context e type''
  -- traceM "_______________________________________________"
  return type''

type'infer _ _ Star =
  return Val.Star

type'infer level context (Pi par in'type out'type) = do
  type'check level context in'type Val.Star
  let in'type' = eval'check in'type []
  type'check (level + 1) ((Local level par, in'type') : context)
              (subst'check 0 (Free (Local level par)) out'type) Val.Star
  return Val.Star

type'infer level context (Free name) = do
  case lookup name context of
    Just type' -> return type'
    Nothing -> -- TODO: WIP
      throwError ("Unknown identifier " ++ show name ++ ".")

type'infer level context (left :@: right) = do
  left't <- type'infer level context left
  case left't of
    pi@(Val.Pi par in'type out'type env) -> do
      type'check level context right in'type
      return $ val'app pi (eval'check right [])
-- TODO: carefully check this part ^^^
-- applying the Pi type function might be incorrect, check the paper for reference
    _ -> throwError "Type error: illegal application! Type of *left* must be a Pi."

-- type'infer level context (LamAnn par in'type body) = do
--   out'type <- type'infer (level + 1) ((Local level par, eval in'type) : context)
--                 (subst'infer 0 (Free (Local level par)) body)
--   return $ Val.Pi par (eval in'type) (Inf body) []
-- TODO: also check this part ^^^ I have written it rather hastily
-- I don't think this is correct, instead of (Inf body) and empty env
-- I think it needs to be out'type or something around it
-- (lambda x :: * -> x)
-- Lam "x" x0 [] :: Pi "x" Star x0 []
-- doesn't seem right

type'infer level ctx term
  = error $! "non-hexhaustive pattern matching " ++ show term


type'check :: Int -> Context -> Term'Check -> Type -> Result ()
type'check level context (Inf e) type' = do
  e't <- type'infer level context e
  unless (type' == e't) (throwError $ "Type mismatch. type' = " ++ show type' ++ "  /= " ++ show e't ++ "\ncontext= " ++ show context)

type'check level context (Lam par body) pi@(Val.Pi param in'type out'type env) = do
    type'check  (level + 1)
                ((Local level par, in'type) : context)
                -- (subst'check 0 (Free (Local level par)) body) (trace (show pi ++ " --- " ++ show (val'app pi (Val.Free "_"))) (val'app pi (Val.Free "_")))
                (subst'check 0 (Free (Local level par)) body)
                (val'app pi (Val.Free param))
-- WIP!!!
                -- (subst'check 0 (Free (Local level par)) body) (trace (show pi ++ " --- " ++ show (val'app pi in'type)) (val'app pi (Val.Free param)))
                -- (subst'check 0 (Free (Local level par)) body) (trace (show pi ++ " --- " ++ show (val'app pi in'type)) (val'app pi in'type))
                --                                                                                                                 ^^^
                --                                                   v paperu tam neposlou in'type ale jenom ten parametr znovu

-- TODO: I think this line is wrong ^^^ I partially fixed it (left part)
-- TODO: carefully check this part ^^^
-- applying the Pi type function might be incorrect, check the paper for reference
type'check _ _ _ _ =
  throwError "Type mismatch. Incorrect shape."


-- tahle funkce musi porovnat dva typy
-- kdyz dorazim k necemu, co ma env musim ho cely projit
-- pro kazdej binding substituovat odpovidajici Termy
-- po tom, je treba evaluovat
-- mozna bych to mohl udelat jinak
-- nejdriv udelat klasickou substituci parametru za Local index
-- potom pouzit evaluaci 
-- jenze
-- ja potrebuju jenom nahradit vsechny vyskyty z Termu za hodnotu z Envu
-- co ale musim zjistit je, jak vypada ten Term
-- chci rict, jak tam vypadaji ty promenne, ktere bych mel nahrazovat
-- no logicky jsou to ty Bound ind, protoze ty se v `eval` funkci nahrazuji
-- jiste ze jsou, protoze tohle Val.Lam / Val.Pi vznikaji tak, ze doslova vezmu neevaluovane telo
-- nikdy neprovedu zadnou zmenu puvodniho Termu
-- tudiz bych mozna mohl proste jenom zavolat `eval` s odpovidajicim environmentem na telo
-- tim bych mel nahradit vsechny pozadovane vyskyty, ktere jsou v environmentu
-- jenze tim, ze zavolam eval, zpusobim to, ze se z toho stane Value
-- takze pak budu muset umet porovnat Value
-- porovnat Value nebude tezky, otazka je, jesti to je v poradku

-- pokud by to tak bylo, proste bych evalnul PI a LAM
-- a dostal Value
-- to bych pak proste v klidu porovnal
-- 
-- infix 4 ===
-- (===) :: Type -> Type -> Bool
-- (===) Val.Star Val.Star
--   = True
-- (===) (Val.Pi l'par l'in'type l'body l'env) (Val.Pi r'par r'in'type r'body r'env)
--   | l'par == r'par && l'in'type == r'in'type
--     = let
--         l'val = eval'check l'body l'env
--         r'val = eval'check r'body r'env
--       in
--         l'val == r'val
--   | otherwise = False

-- (===) (Val.Lam l'par l'body l'env) (Val.Lam r'par r'body r'env)
--   | l'par == r'par
--     = let
--         l'val = eval'check l'body l'env
--         r'val = eval'check r'body r'env
--       in
--         l'val == r'val
--   | otherwise = False

-- (===) (Val.Free l'id) (Val.Free r'id)
--   = l'id == r'id
-- (===) (Val.App l'left l'right) (Val.App r'left r'right)
--   = l'left === r'left && l'right === r'right
-- (===) _ _ = False


-- TODO: implement comparison working for Lambda and Pi functions
-- it must "apply" the environment and only compare the two types after all the substitutions
-- ignoring unused rests of the environment(s)
instance Eq Val.Value where
  (==) Val.Star Val.Star = True
  (==) (Val.Pi l'par l'in'type l'body l'env) (Val.Pi r'par r'in'type r'body r'env)
    | l'par == r'par && l'in'type == r'in'type
      = let
          l'val = eval'check l'body l'env
          r'val = eval'check r'body r'env
        in
          l'val == r'val
    | otherwise = False
  (==) (Val.Lam l'par l'body l'env) (Val.Lam r'par r'body r'env)
    | l'par == r'par
      = let
          l'val = eval'check l'body l'env
          r'val = eval'check r'body r'env
        in
          l'val == r'val
    | otherwise = False

  (==) (Val.Free l'id) (Val.Free r'id)
    = l'id == r'id
  (==) (Val.App l'left l'right) (Val.App r'left r'right)
    = l'left == r'left && l'right == r'right
  (==) _ _ = False


class Typeable a where
  type'of :: a -> Context -> Result Type


instance Typeable Term'Infer where
  type'of ann@(expr ::: type') context
    = type'infer'0 context ann
  type'of (Free _) context
    = Right $ Val.Free "*" -- kinda ugly and horrible, but get the message delivered
  type'of app@(left :@: right) context
    = type'infer'0 context app
  -- type'of lam@(LamAnn name in'type body) context
  --   = type'infer'0 context lam


instance Typeable Term'Check where
  type'of (Inf expr) context
    = type'of expr context
  type'of (Lam par body) context
    = Left "Can't infer a type of an unannotated λ."
