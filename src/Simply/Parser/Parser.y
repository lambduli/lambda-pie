{
module Simply.Parser.Parser (parse'expr) where

import Data.List (elemIndex)

import Control.Monad (unless, fail)
import Control.Monad.State hiding (fix)

import qualified Simply.Parser.Token as Tok
import Simply.Parser.Lexer
import Simply.Parser.Utils

import Simply.Command
import Simply.AST
import Simply.Name
import Simply.Context
import Simply.Kind
import Simply.Type

import Debug.Trace
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


%%
Program         ::  { Either Command Term'Check }
                :   TermCheck                                       { Right $1 }
                |   Command                                         { Left $1 }


TermInfer       ::  { Term'Infer }
                :   var                                             { Free $ Global $1 }
--                |   TermInfer OneOrMany(TermCheck)                  { foldl (:@:) $1 (trace ("____ zbytek listu?  " ++ show $2) $2) }
-- NOTE: disabling the `a b c d` syntax for now due to shift/reduce conflicts and incorrect associativity
                |   '(' TermInfer TermCheck ')'                     { $2 :@: $3 }
                |   TermCheck '::' Type                             { $1 ::: $3 }
                |   '(' TermInfer ')' {- %shift -}                  { $2 }


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

fix :: Term'Check -> Term'Check
fix lambda = fix'check lambda []


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


parseError _ = do
  lno <- getLineNo
  colno <- getColNo
  s <- get
  error $ "Parse error on line " ++ show lno ++ ", column " ++ show colno ++ "." ++ "\n" ++ (show s)


parse'expr :: String -> Either Command Term'Check
parse'expr s =
  evalP parserAct s
}