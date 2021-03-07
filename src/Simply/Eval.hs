module Simply.Eval where

import Simply.AST (Term'Infer(..), Term'Check(..))
import qualified Simply.Value as Val
import Simply.Value ( Env )
import Simply.Name


eval'infer :: Term'Infer -> Env -> Val.Value
eval'infer (e ::: _) env
  = eval'check e env
eval'infer (Bound ind name) env
  = env !! ind
eval'infer (Free name) env
  | Global id <- name = Val.Free id
  | Local _ id <- name = Val.Free id
eval'infer (left :@: right) env
  = val'app (eval'infer left env) (eval'check right env)
eval'infer (LamAnn par in'type body) env
  = Val.Lam par (Inf body) env


val'app :: Val.Value -> Val.Value -> Val.Value
val'app (Val.Lam _ body env) arg
  = eval'check body (arg : env)
val'app left right
  = Val.App left right


eval'check :: Term'Check -> Env -> Val.Value
eval'check (Inf e) env
  = eval'infer e env
eval'check (Lam par body) env
  = Val.Lam par body env


class Evaluate a where
  eval :: a -> Val.Value


instance Evaluate Term'Infer where
  eval term = eval'infer term []

instance Evaluate Term'Check where
  eval term = eval'check term []
