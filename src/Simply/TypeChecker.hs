module Simply.TypeChecker where

import Control.Monad

import Simply.Context
import Simply.Kind
import Simply.Type
import Simply.AST
import Simply.Name
import Simply.Substitute


type Result a = Either String a


throwError :: String -> Result a
throwError = Left


kind'check :: Context -> Type -> Kind -> Result ()
kind'check context (TFree name) Star =
  case lookup name context of
    Just (HasKind Star) -> return ()
    Just _ -> throwError $ "A type var " ++ show name ++ " has not kind of a *."
    Nothing -> throwError $ "Unknown type var identifier " ++ show name ++ "."
kind'check context (left't :-> right't) Star = do
  kind'check context left't Star
  kind'check context right't Star


type'infer'0 :: Context -> Term'Infer -> Result Type
type'infer'0 = type'infer 0


type'infer :: Int -> Context -> Term'Infer -> Result Type
type'infer level context (e ::: type') = do
  kind'check context type' Star
  type'check level context e type'
  return type'
type'infer level context (Free name) = do
  case lookup name context of
    Just (HasType type') -> return type'
    Just _ -> throwError $ "Type error for " ++ show name ++ "."
    Nothing -> throwError "Unknown identifier."
type'infer level context (left :@: right) = do
  left't <- type'infer level context left
  case left't of
    in't :-> out't -> do
      type'check level context right in't
      return out't
    _ -> throwError "Type error: illegal application."


type'check :: Int -> Context -> Term'Check -> Type -> Result ()
type'check level context (Inf e) type' = do
  e't <- type'infer level context e
  unless (type' == e't) (throwError "Type mismatch.")
type'check level context (Lam par body) (in't :-> out't) = do
  type'check (level + 1) ((Local level par, HasType in't) : context)
            (subst'check 0 (Free (Local level par)) body) out't
type'check _ _ _ _ =
  throwError "Type mismatch."
