module Dependently.TypeChecker where

import Control.Monad

import Dependently.Context
import Dependently.AST
import qualified Dependently.Value as Val
import Dependently.Name
import Dependently.Eval
import Dependently.Substitute


type Result a = Either String a

throwError :: String -> Result a
throwError = Left


type'infer :: Int -> Context -> Term'Infer -> Result Type
type'infer level context (e ::: type') = do
  type'check level context type' Val.Star
  let type'' = eval'check type' []
  type'check level context e type''
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
    Nothing -> throwError $ "Unknown identifier " ++ show name ++ "."
type'infer level context (left :@: right) = do
  left't <- type'infer level context left
  case left't of
    pi@(Val.Pi par in'type out'type env) -> do
      type'check level context right in'type
      return $ val'app pi (eval'check right [])
-- TODO: carefully check this part ^^^
-- applying the Pi type function might be incorrect, check the paper for reference
    _ -> throwError "Type error: illegal application! Type of *left* must be a Pi."


type'check :: Int -> Context -> Term'Check -> Type -> Result ()
type'check level context (Inf e) type' = do
  e't <- type'infer level context e
  unless (type' === e't) (throwError "Type mismatch.")
type'check level context (Lam par body) pi@(Val.Pi param in'type out'type env) = do
    type'check (level + 1) ((Local level par, in'type) : context )
                (subst'check 0 (Free (Local level "_")) body) (val'app pi (Val.Free "_"))
-- TODO: carefully check this part ^^^
-- applying the Pi type function might be incorrect, check the paper for reference
type'check _ _ _ _ =
  throwError "Type mismatch."


infix 4 ===
(===) :: Type -> Type -> Bool
left === right = undefined
-- TODO: implement comparison working for Lambda and Pi functions
-- it must "apply" the environment and only compare the two types after all the substitutions
-- ignoring unused rests of the environment(s)
