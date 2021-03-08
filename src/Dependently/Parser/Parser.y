{
module Dependently.Parser.Parser (parse'expr) where

import Data.List (elemIndex)
import Data.Bifunctor (first)

import Control.Monad (unless, fail)
import Control.Monad.State hiding (fix)

import qualified Dependently.Parser.Token as Tok
import Dependently.Parser.Lexer
import Dependently.Parser.Utils

import Dependently.Command
import Dependently.AST
import Dependently.Name
import Dependently.Context
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
  '::'          { Tok.DoubleColon }
  '('           { Tok.LeftParen }
  ')'           { Tok.RightParen }
  '*'           { Tok.Star }
  '->'          { Tok.Arrow }
  '.'           { Tok.Dot }
  forall        { Tok.Forall }


%%
Program         ::  { Either Command Term'Check }
                :   TermCheck                                       { Right $1 }
                |   Command                                         { Left $1 }


TypedParams     ::  { [(String, Term'Check)] }
                :   var '::' TermCheck                              { [($1, $3)] }
                |   OneOrMany(TypedParam)                           { $1 }



TypedParam      ::  { (String, Term'Check) }
                :   '(' var '::' TermCheck ')'                      { ($2, $4) }


TermInfer       ::  { Term'Infer }
                :   TermAnn '::' TermCheck                          { $1 ::: $3 }
                |   TermInfer2                                      { $1 }


TermInfer2      ::  { Term'Infer }
                :   '*'                                             { Star }
                |   Forall                                          { $1 }
                |   var                                             { Free $ Global $1 }
                |   '(' TermInfer ')' {- %shift -}                  { $2 }
                |   TermInfer2 OneOrMany(AppRight)                  { foldl (:@:) $1 $2 }
                |   '(' lambda TypedParams '->' TermInfer ')'       { fix $ foldr
                                                                       (\ (par, type') body -> LamAnn par type' body)
                                                                       $5
                                                                       $3 }


TermAnn         ::  { Term'Check }
                :   TermInfer2                                      { Inf $1 }
                
                -- :   '*'                                             { Inf Star }
                -- |   var                                             { Inf $ Free $ Global $1 }
                -- |   '(' TermInfer ')' {- %shift -}                  { $2 }
                -- |   AppLeft OneOrMany(AppRight)                     { foldl (:@:) $1 $2 }
                -- |   '(' lambda TypedParams '->' TermInfer ')'       { Inf $ fix $ foldr
                --                                                        (\ (par, type') body -> LamAnn par type' body)
                --                                                        $5
                --                                                        $3 }
                |   '(' lambda Params '->' TermCheck ')'            { fix $ foldr
                                                                        (\ arg body -> Lam arg body)
                                                                        $5
                                                                        $3 }
                |   '(' TermCheck ')'                               { $2 }



AppLeft         ::  { Term'Infer }
                :   TermInfer2                                      { $1 }
                -- :   TermAnn '::' TermCheck                          { $1 ::: $3 }
                -- |   '*'                                             { Star }
                -- |   Forall                                          { $1 }
                -- |   var                                             { Free $ Global $1 }
                -- |   '(' TermInfer ')' {- %shift -}                  { $2 }
                -- |   '(' lambda TypedParams '->' TermInfer ')'       { fix $ foldr
                --                                                         (\ (par, type') body -> LamAnn par type' body)
                --                                                         $5
                --                                                         $3 }


AppRight        ::  { Term'Check }
                :   TermAnn '::' TermCheck                          { Inf $ $1 ::: $3 }
                |   '*'                                             { Inf $ Star }
                |   Forall                                          { Inf $1 }
                |   var                                             { Inf $ Free $ Global $1 }
                |   '(' lambda TypedParams '->' TermInfer ')'       { Inf $ fix $ foldr
                                                                        (\ (par, type') body -> LamAnn par type' body)
                                                                        $5
                                                                        $3 }
                |   '(' TermInfer ')' {- %shift -}                  { Inf $ $2 }
                |   '(' lambda Params '->' TermCheck ')'            { fix $ foldr
                                                                        (\ arg body -> Lam arg body)
                                                                        $5
                                                                        $3 }
                |   '(' TermCheck ')'                               { $2 }


Forall          ::  { Term'Infer }
                :   forall TypedParams '.' TermCheck                { unwrap $ foldl
                                                                        (\ body (name, type') ->
                                                                          case body of
                                                                          { Check ch -> Infer $ Pi name type' ch
                                                                          ; Infer i -> Infer $ Pi name type' $ Inf i })
                                                                        (Check $4)
                                                                        $2 }


Params          ::  { [String] }
                :   OneOrMany(var)                                  { $1 }


TermCheck       ::  { Term'Check }
                :   TermInfer                                       { Inf $1 }
                |   '(' lambda Params '->' TermCheck ')'            { fix $ foldr
                                                                        (\ arg body -> Lam arg body)
                                                                        $5
                                                                        $3 }
                |   '(' TermCheck ')'                               { $2 }


Command         ::  { Command }
                :   assume TypedParams                              { Assume map (first Global) $2 }




NoneOrMany(tok)
                :   {- empty -}                                     { [] }
                |   tok NoneOrMany(tok)                             { $1 : $2 }

OneOrMany(tok)
                :   tok NoneOrMany(tok)                             { $1 : $2 }

{

data Inf'or'Check
  = Infer Term'Infer
  | Check Term'Check


unwrap :: Inf'or'Check -> Term'Infer
unwrap (Infer inf) = inf


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