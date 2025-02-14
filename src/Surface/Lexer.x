{
module Surface.Lexer where

import Prelude hiding (EQ)
}

%wrapper "posn"

$digit = 0-9
$alphanum = [a-zA-Z0-9_]

tokens :-
  $white+ ;

  "+"             { \p s -> Token p PLUS }
  "-"             { \p s -> Token p MINUS }
  "*"             { \p s -> Token p STAR }
  "/"             { \p s -> Token p SLASH }
  "="             { \p s -> Token p EQ }
  "<"             { \p s -> Token p LANGLE }
  ">"             { \p s -> Token p RANGLE }
  "<="            { \p s -> Token p LE }
  ">="            { \p s -> Token p GE }
  "||"            { \p s -> Token p OR }
  "&&"            { \p s -> Token p AND }
  "("             { \p s -> Token p LPAREN }
  ")"             { \p s -> Token p RPAREN }
  "->"            { \p s -> Token p ARROW }
  ","             { \p s -> Token p COMMA }
  "|"             { \p s -> Token p PIPE } 
  ":"             { \p s -> Token p COLON } 

  "let"           { \p s -> Token p LET } 
  "in"            { \p s -> Token p IN } 
  "data"          { \p s -> Token p DATA } 
  "of"            { \p s -> Token p OF } 
  "int"           { \p s -> Token p INT } 
  "match"         { \p s -> Token p MATCH } 
  "with"          { \p s -> Token p WITH } 
  "if"            { \p s -> Token p IF } 
  "then"          { \p s -> Token p THEN } 
  "else"          { \p s -> Token p ELSE } 

  $digit+         { \p s -> Token p (INTEGER $ read s) }
  [a-z]$alphanum* { \p s -> Token p (LID s) }
  [A-Z]$alphanum* { \p s -> Token p (UID s) }

{
data Token
  = Token AlexPosn Token'
  deriving (Show)

data Token'
  = PLUS
  | MINUS
  | STAR
  | SLASH
  | EQ
  | LPAREN
  | RPAREN
  | LANGLE
  | RANGLE
  | LE
  | GE
  | OR
  | AND
  | ARROW
  | COMMA
  | PIPE
  | COLON

  | LET
  | IN
  | DATA
  | OF
  | INT
  | MATCH
  | WITH
  | IF
  | THEN
  | ELSE

  | INTEGER Int
  | LID String
  | UID String

  deriving (Show)

tokenToPos :: Token -> AlexPosn
tokenToPos (Token p _) = p
}
