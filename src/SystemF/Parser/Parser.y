{
module SystemF.Parser.Parser (parse'expr) where

import Data.List (elemIndex)

import Control.Monad (unless, fail)
import Control.Monad.State hiding (fix)

import qualified SystemF.Parser.Token as Tok
import SystemF.Parser.Lexer
import SystemF.Parser.Utils

import SystemF.Command
import SystemF.AST
import SystemF.Name
import SystemF.Context
import SystemF.Kind
import SystemF.Type
}


%name parserAct
%tokentype { Tok.Token }
%error { parseError }
%monad { P }
%lexer { lexer } { Tok.EOF }
-- %expect 0


%token
  assume        { Tok.Assume }
  var           { Tok.Identifier $$ }
  lambda        { Tok.Lambda }
  typelambda    { Tok.TyLam }
  '::'          { Tok.DoubleColon }
  '('           { Tok.LeftParen }
  ')'           { Tok.RightParen }
  '['           { Tok.LeftBracket }
  ']'           { Tok.RightBracket }
  '*'           { Tok.Star }
  '->'          { Tok.Arrow }
  forall        { Tok.Forall }
  '.'           { Tok.Dot }


%%
Program         ::  { Either Command Term'Check }
                :   TermCheck                                       { Right $1 }
                |   Command                                         { Left $1 }


TypedParams     ::  { [(String, Type)] }
                :   OneOrMany(TypedParam)                           { $1 }


TypedParam      ::  { (String, Type) }
                :   '(' var '::' Type ')'                           { ($2, $4) }


TermInfer       ::  { Term'Infer }
                :   var                                             { Free $ Global $1 }
--                |   '(' TermInfer TermCheck ')'                     { $2 :@: $3 }
                |   AppLeft OneOrMany(AppRight)                     { foldl
                                                                        (\ l r -> case r of
                                                                          { Left t -> l :$: t
                                                                          ; Right term -> l :@: term })
                                                                        $1 $2 }
                |   '(' TermCheck '::' Type ')'                     { $2 ::: $4 }
                |   TermCheck '::' Type                             { $1 ::: $3 }
                -- NOTE: adding parens around removes some conflicts
                |   '(' lambda TypedParams '->' TermInfer ')'       { fix $ foldr
                                                                        (\ (par, type') body -> LamAnn par type' body)
                                                                        $5
                                                                        $3 }
                |   '(' typelambda var '.' TermInfer ')'            { TyLam $3 $5 }
                |   '(' TermInfer ')' {- %shift -}                  { $2 }


AppLeft         ::  { Term'Infer }
                :   var                                             { Free $ Global $1 }
                |   TermCheck '::' Type                             { $1 ::: $3 }
                |   '(' TermCheck '::' Type ')'                     { $2 ::: $4 }

                |   '(' lambda TypedParams '->' TermInfer ')'       { fix $ foldr
                                                                        (\ (par, type') body -> LamAnn par type' body)
                                                                        $5
                                                                        $3 }
                |   '(' TermInfer ')' {- %shift -}                  { $2 }
                |   '(' typelambda var '.' TermInfer ')'            { TyLam $3 $5 }


AppRight        ::  { Either Type Term'Check }
                :   var                                             { Right $ Inf $ Free $ Global $1 }
                |   '(' TermCheck '::' Type ')'                     { Right $  Inf $ $2 ::: $4 }
                -- NOTE: adding parens around removes 9 r/r conflict
                |   '(' lambda TypedParams '->' TermInfer ')'       { Right $ Inf $ fix $ foldr
                                                                        (\ (par, type') body -> LamAnn par type' body)
                                                                        $5
                                                                        $3 }
                |   '(' TermInfer ')' {- %shift -}                  { Right $ Inf $ $2 }
                |   '(' lambda Params '->' TermCheck ')'            { Right $ fix $ foldr
                                                                        (\ arg body -> Lam arg body)
                                                                        $5
                                                                        $3 }
                |   '(' TermCheck ')'                               { Right $2 }
                |   '[' Type ']'                                    { Left $2 }


Params          ::  { [String] }
                :   OneOrMany(var)                                  { $1 }


TermCheck       ::  { Term'Check }
                :   TermInfer                                       { Inf $1 }
                |   '(' lambda Params '->' TermCheck ')'            { fix $ foldr
                                                                        (\ arg body -> Lam arg body)
                                                                        $5
                                                                        $3 }
                |   '(' TermCheck ')'                               { $2 }


Type            ::  { Type }
                :   var                                             { TFree $ Global $1 }
                |   var '->' Type                                   { TFree (Global $1) :-> $3 }
                |   forall var '.' Type                             { Forall $2 $4 }
                |   '(' Type ')' '->' Type                          { $2 :-> $5 }
                |   '(' Type ')'                                    { $2 }


Command         ::  { Command }
                :   assume var '::' '*'                             { Assume [ (Global $2, HasKind Star) ] }
                |   assume var '::' Type                            { Assume [ (Global $2, HasType $4) ] }
                |   assume OneOrMany(AssumeWrapped)                 { Assume $2 }


AssumeWrapped   ::  { (Name, Info) }
                :   '(' var '::' '*' ')'                            { (Global $2, HasKind Star) }
                |   '(' var '::' Type ')'                           { (Global $2, HasType $4) }




NoneOrMany(tok)
                :   {- empty -}                                     { [] }
                |   tok NoneOrMany(tok)                             { $1 : $2 }

OneOrMany(tok)
                :   tok NoneOrMany(tok)                             { $1 : $2 }

{

class Fix a where
  fix :: a -> a


instance Fix Term'Check where
  fix lambda = fix'check lambda []


instance Fix Term'Infer where
  fix lambda = fix'infer lambda []


fix'check :: Term'Check -> [String] -> Term'Check
fix'check (Lam par body) context
  = Lam par $ fix'check body (par : context)
fix'check (Inf term) context
  = Inf $ fix'infer term context


fix'infer :: Term'Infer -> [String] -> Term'Infer
fix'infer (term ::: type') context
  = (fix'check term context) ::: type'
fix'infer (Bound i n) _
  = Bound i n
fix'infer (Free (Global name)) context
  = case elemIndex name context of
      Just ind -> Bound ind name
      Nothing -> Free (Global name)
fix'infer (left :@: right) context
  = (fix'infer left context) :@: (fix'check right context)
fix'infer (LamAnn par type' body) context
  = LamAnn par type' $ fix'infer body (par : context)


parseError _ = do
  lno <- getLineNo
  colno <- getColNo
  s <- get
  error $ "Parse error on line " ++ show lno ++ ", column " ++ show colno ++ "." ++ "\n" ++ (show s)


parse'expr :: String -> Either Command Term'Check
parse'expr s =
  evalP parserAct s
}