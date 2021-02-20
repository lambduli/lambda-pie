module Simply.TypeChecker where

import Control.Monad

import Simply.Context
import Simply.Kind
import Simply.Type
import Simply.AST

type Result a = Either String a

throwError :: String -> Result a
throwError err =
  Left err


kind'check :: Context -> Type -> Kind -> Result ()
kind'check context (TVar name) Star =
  case lookup name context of
    Just (HasKind Star) -> return ()
    Just _ -> throwError $ "A type var " ++ name ++ " has not kind of a *."
    Nothing -> throwError $ "Unknown type var identifier " ++ name ++ "."
kind'check context (left't :-> right't) Star = do
  kind'check context left't Star
  kind'check context right't Star


type'infer :: Context -> Term'Infer -> Result Type
type'infer context (e ::: type') = do
  kind'check context type' Star
  type'check context e type'
  return type'
type'infer context (Var name) = do
  case lookup name context of
    Just (HasType type') -> return type'
    Just _ -> throwError $ "Type error for " ++ name ++ "."
    Nothing -> throwError "Unknown identifier."
type'infer context (left :@: right) = do
  left't <- type'infer context left
  case left't of
    in't :-> out't -> do
      type'check context right in't
      return out't
    _ -> throwError "Type error: illegal application."


type'check :: Context -> Term'Check -> Type -> Result ()
type'check context (Inf e) type' = do
  e't <- type'infer context e
  unless (type' == e't) (throwError "Type mismatch.")
type'check context (Lam par body) (in't :-> out't) = do
  type'check ((par, HasType in't) : context ) body out't
type'check _ _ _ =
  throwError "Type mismatch."