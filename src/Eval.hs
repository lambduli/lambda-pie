module Eval where

import AST (Term'Infer(..), Term'Check(..))
import qualified Value as Val


eval'infer :: Term'Infer -> Val.Value
eval'infer (e ::: _) =
  eval'check e
eval'infer (Var name) =
  Val.Var name
eval'infer (left :@: right) =
  val'app (eval'infer left) (eval'check right)


val'app :: Val.Value -> Val.Value -> Val.Value
val'app l@(Val.Lam _ _) arg =
  let (Val.Lam par body) = alpha l arg
  in beta body (par, arg)
val'app left right =
  Val.App left right


beta :: Val.Value -> (String, Val.Value) -> Val.Value
beta = undefined


alpha :: Val.Value -> Val.Value -> Val.Value
alpha = undefined


eval'check :: Term'Check -> Val.Value
eval'check (Inf e) =
  eval'infer e
eval'check (Lam par body) =
  Val.Lam par $ eval'check body
