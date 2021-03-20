module SystemF.Substitute where

import SystemF.AST


subst'infer :: Int -> Term'Infer -> Term'Infer -> Term'Infer
subst'infer level rep (exp ::: type')
  = subst'check level rep exp ::: type'
subst'infer level rep (Bound indx name)
  | level == indx = rep
  | otherwise  = Bound indx name
subst'infer level rep (Free name)
  = Free name
subst'infer level rep (left :@: right)
  = subst'infer level rep left :@: subst'check level rep right
subst'infer level rep (left :$: right't) -- NEW
  = subst'infer level rep left :$: right't -- NEW
subst'infer level rep (TyLam t'par term) -- NEW
  = TyLam t'par $ subst'infer level rep term -- NEW
subst'infer level rep (LamAnn par in'type body)
  = LamAnn par in'type $ subst'infer (level + 1) rep body


subst'check :: Int -> Term'Infer -> Term'Check -> Term'Check
subst'check level rep (Inf exp)
  = Inf (subst'infer level rep exp)
subst'check level rep (Lam par body)
  = Lam par (subst'check (level + 1) rep body)