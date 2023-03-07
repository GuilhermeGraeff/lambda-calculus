{
module Parser where

import Lexer
}

%name parser 
%tokentype { Token }
%error { parseError }

%token 
    true    { TokenTrue }
    false   { TokenFalse }
    num     { TokenNum $$ }
    '('     { TokenLParen }
    ')'     { TokenRParen }
    '+'     { TokenPlus }
    '*'     { TokenTimes }
    '-'     { TokenMinus }
    "&&"    { TokenAnd }
    "||"    { TokenOr }    
    if      { TokenIf }
    then    { TokenThen }
    else    { TokenElse }
    var     { TokenVar $$ }
    '\\'    { TokenLam }
    ':'     { TokenColon }
    "->"    { TokenArrow }
    Bool    { TokenBoolean }
    Number  { TokenNumber }
    let     { TokenLet }
    in      { TokenIn }
    '='     { TokenAssign }
    ','     { TokenComma }
    '.'     { TokenDot }
    '{'     { TokenLBrack }
    '}'     { TokenRBrack }
    fix     { TokenFix }
    letrec  { TokenLetRec }
    "=="    { TokenEq }
    '!'     { TokenNot }


%nonassoc if then else
%left '+' '-'
%left '*'

%% 

Exp     : true                          { BTrue }
        | false                         { BFalse }
        | num                           { Num $1 }
        | var                           { Var $1 }
        | Exp '+' Exp                   { Plus $1 $3 }
        | Exp '-' Exp                   { Minus $1 $3 }
        | Exp '*' Exp                   { Times $1 $3 }
        | Exp "&&" Exp                  { And $1 $3 }
        | Exp "||" Exp                  { Or $1 $3 }
        | '(' Exp ')'                   { Paren $2 }
        | if Exp then Exp else Exp      { If $2 $4 $6 }
        | '\\' var ':' Type "->" Exp    { Lam $2 $4 $6 }
        | Exp Exp                       { App $1 $2 }
        | let var '=' Exp in Exp        { Let $2 $4 $6 }
        | '(' ExpList ')'               { Tuple $2 }
        | Exp '.' num                   { TupleProj $1 $3 }
        | '{' RecList '}'               { Record $2 }
        | Exp '.' var                   { RecordProj $1 $3 }
        | fix Exp                       { Fix $2 }
        | letrec var ':' Type '=' Exp in Exp { Let $2 (Fix (Lam $2 $4 $6)) $8 }
        | Exp "==" Exp                  { Eq $1 $3 }
        | '!' Exp                       { Not $2 }

ExpList : Exp                           { [$1] }
        | Exp ',' ExpList               { $1 : $3 }

RecList : var '=' Exp                   { [($1,$3)] }
        | var '=' Exp ',' RecList       { ($1, $3) : $5 }

Type    : Bool                          { TBool }
        | Number                        { TNum }
        | '(' Type "->" Type ')'        { TFun $2 $4 }

{

parseError :: [Token] -> a
parseError ts = error "Syntax error: sequência de instruções inválida!"

}
