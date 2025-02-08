{
module Surface.Parser where

import Prelude hiding (EQ)
import Surface.Lexer
import Surface.Syntax
import qualified Common.Node as Node
}

%name parseProgram
%tokentype { Token }
%error { parseError }

%token
    '+'     { Token _ PLUS }
    '*'     { Token _ STAR }
    '='     { Token _ EQ }
    '('     { Token _ LPAREN }
    ')'     { Token _ RPAREN }
    "->"    { Token _ ARROW }
    ','     { Token _ COMMA }
    '|'     { Token _ PIPE }

    LET     { Token _ LET }
    IN      { Token _ IN }
    DATA    { Token _ DATA }
    OF      { Token _ OF }
    INT     { Token _ INT }
    MATCH   { Token _ MATCH }
    WITH    { Token _ WITH }

    INTEGER { Token _ (INTEGER _) }
    LID     { Token _ (LID _) }
    UID     { Token _ (UID _) }

%left '+'
%nonassoc LID INTEGER '('
%nonassoc APP

%%

program 
    : typedefs expr { Program $1 $2 }

typedef 
    : DATA LID '=' constructors { mkNode $2 (TypeDef (tokenToVar $2) $4) }

typedefs
    : { [] }
    | typedef typedefs { $1 : $2 }

constructor
    : '|' UID           { mkNode $2 (Constructor (tokenToVar $2) []) }
    | '|' UID OF types1 { mkNode $2 (Constructor (tokenToVar $2) $4) }

constructors
    : { [] }
    | constructor constructors { $1 : $2 }

type 
    : INT            { mkNode $1 TInt }
    | LID            { mkNode $1 (TADT $ tokenToVar $1) }
    | type "->" type { mkNode $2 (TArrow $1 $3) }

types1
    : type           { [$1] }
    | type '*' types1 { $1 : $3 }

expr
    : INTEGER                      { mkNode $1 (EInt $ tokenToInt $1) }
    | LID                          { mkNode $1 (EVar $ tokenToVar $1) }
    | expr '+' expr                { mkNode $2 (EBinop Add $1 $3) }
    | LET LID '=' expr IN expr     { mkNode $1 (ELet (tokenToVar $2) $4 $6) }
    | LET LID LID '=' expr IN expr { mkNode $1 (EFun (tokenToVar $2) (tokenToVar $3) $5 $7) }
    | expr expr %prec APP          { mkNode2 $1 (EApp $1 $2) } 
    | '(' expr ')'                 { $2 }
    | UID                          { mkNode $1 (ECtor (tokenToVar $1) []) }
    | UID '(' exprs1 ')'           { mkNode $1 (ECtor (tokenToVar $1) $3) }
    | MATCH expr WITH clauses      { mkNode $1 (EMatch $2 $4) }

exprs1
    : expr            { [$1] }
    | expr ',' exprs1 { $1 : $3 }

clause 
    : '|' pat "->" expr { Clause $2 $4 }

clauses
    :                { [] }
    | clause clauses { $1 : $2 }

pat 
    : LID               { mkNode $1 (PVar (tokenToVar $1)) }
    | UID               { mkNode $1 (PCtor (tokenToVar $1) []) }
    | UID '(' pats1 ')' { mkNode $1 (PCtor (tokenToVar $1) $3) }

pats1
    : pat           { [$1] }
    | pat ',' pats1 { $1 : $3 }

{
mkNode :: Token -> a -> Node a
mkNode t x = Node.Node { pos = tokenToPos t, value = x }

mkNode2 :: Node b -> a -> Node a
mkNode2 n x = Node.Node { pos = pos n, value = x }

tokenToInt :: Token -> Int
tokenToInt (Token _ (INTEGER n)) = n

tokenToVar :: Token -> Var
tokenToVar (Token _ (LID x)) = x
tokenToVar (Token _ (UID x)) = x

parseError :: [Token] -> a
parseError ((Token p t) : _) = error $ "Parse error at position " ++ show p ++ " with token: " ++ show t
parseError _ = error "Parse error"
}
