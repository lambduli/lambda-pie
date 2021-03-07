{
module Dependently.Parser.Lexer (lexer, readToken) where

import Control.Monad.State

import Dependently.Parser.Utils
import Dependently.Parser.Token
}


$lower  = [a-z]
$upper  = [A-Z]


@variableident      = [$lower $upper]+

$space  = [\ \t\f\v]


--
-- lexical grammar
--

token :-

<0>         assume          { plainTok Assume }

<0>         "λ"             { plainTok Lambda }
<0>         lambda          { plainTok Lambda }
<0>         "\"             { plainTok Lambda }


-- special symbols
<0>         "("             { plainTok LeftParen }
<0>         ")"             { plainTok RightParen }
<0>         "::"            { plainTok DoubleColon }
<0>         "*"             { plainTok Star }
<0>         "->"            { plainTok Arrow }
<0>         "."             { plainTok Dot }
<0>         forall          { plainTok Forall }


-- variables
<0>         @variableident  { parametrizedTok Identifier id }


<0>         \n              ;
<0>         $space+         ;

{

type NumberOfCharsMatched = Int

type MatchedSequence = String

type LexAction = NumberOfCharsMatched -> MatchedSequence -> P (Maybe Token)


plainTok :: Token -> LexAction
plainTok token _ _ = do
  return $ Just token


parametrizedTok :: (a -> Token) -> (String -> a) -> LexAction
parametrizedTok tc read' _ matched = do
  let token = tc (read' matched)
  return $ Just token


readToken :: P Token
readToken = do
  s <- get
  case alexScan (input s) (lexSC s) of
    AlexEOF -> do
      return EOF

    AlexError inp' ->
      error $ "Lexical error on line " ++ (show $ ai'line'number inp')
    
    AlexSkip inp' _ -> do
      put s{ input = inp' }
      readToken
    
    AlexToken inp' n act -> do
      let (AlexInput{ ai'rest = buf }) = input s -- TODO: rename airest
      put s{ input = inp' }
      res <- act n (take n buf)
      case res of
        Nothing -> readToken
        Just t -> return t


lexer :: (Token -> P a) -> P a
lexer cont = do
  tok <- readToken
  cont tok
}
