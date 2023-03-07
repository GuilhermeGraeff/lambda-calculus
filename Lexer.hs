module Lexer where

import Data.Char

-- Type definitions

data Ty = TBool
        | TNum
        | TFun Ty Ty
        | TTuple [Ty]
        | TRecord [(String,Ty)]
        deriving (Show, Eq)

-- Expression definitions

data Expr = BTrue
          | BFalse
          | Num Int
          | Paren Expr
          | Plus Expr Expr
          | Times Expr Expr
          | Minus Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | If Expr Expr Expr
          | Var String
          | Lam String Ty Expr
          | App Expr Expr 
          | Let String Expr Expr 
          | Tuple [Expr]
          | TupleProj Expr Int 
          | Record [(String,Expr)] 
          | RecordProj Expr String
          | Fix Expr 
          | Eq Expr Expr
          | Not Expr
          deriving (Show, Eq)

-- Token definitions

data Token = TokenTrue
           | TokenFalse
           | TokenNum Int
           | TokenLParen
           | TokenRParen
           | TokenPlus
           | TokenTimes
           | TokenMinus
           | TokenAnd 
           | TokenOr
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenVar String 
           | TokenLam
           | TokenColon
           | TokenArrow 
           | TokenBoolean
           | TokenNumber
           | TokenLet 
           | TokenIn 
           | TokenAssign
           | TokenComma
           | TokenDot
           | TokenLBrack 
           | TokenRBrack
           | TokenFix
           | TokenLetRec
           | TokenEq
           | TokenNot
           deriving Show

-- Lexer functions

isToken :: Char -> Bool
isToken c = elem c "->&|="

lexer :: String -> [Token]
lexer [] = []
lexer ('+':cs) = TokenPlus : lexer cs 
lexer ('*':cs) = TokenTimes : lexer cs 
lexer ('(':cs) = TokenLParen : lexer cs 
lexer (')':cs) = TokenRParen : lexer cs 
lexer (':':cs) = TokenColon : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('.':cs) = TokenDot : lexer cs
lexer ('{':cs) = TokenLBrack : lexer cs 
lexer ('}':cs) = TokenRBrack : lexer cs 
lexer ('!':cs) = TokenNot : lexer cs
lexer ('\\':cs) = TokenLam : lexer cs
lexer (c:cs) 
    | isSpace c = lexer cs
    | isDigit c = lexNum (c:cs)
    | isAlpha c = lexKW (c:cs)
    | isToken c = lexSymbol (c:cs)
lexer _ = error "Lexical error: caracter inválido!"

lexNum cs = case span isDigit cs of
                (num, rest) -> TokenNum (read num) : lexer rest

lexKW cs = case span isAlpha cs of
                ("false", rest)  -> TokenFalse : lexer rest 
                ("true", rest)   -> TokenTrue : lexer rest 
                ("if", rest)     -> TokenIf : lexer rest 
                ("then", rest)   -> TokenThen : lexer rest
                ("else", rest)   -> TokenElse : lexer rest
                ("Bool", rest)   -> TokenBoolean : lexer rest
                ("Number", rest) -> TokenNumber : lexer rest
                ("let", rest)    -> TokenLet : lexer rest 
                ("in", rest)     -> TokenIn : lexer rest 
                ("fix", rest)    -> TokenFix : lexer rest 
                ("letrec", rest) -> TokenLetRec : lexer rest 
                (var, rest)      -> TokenVar var : lexer rest

lexSymbol cs = case span isToken cs of
                   ("->", rest)         -> TokenArrow  : lexer rest
                   ("&&", rest)         -> TokenAnd    : lexer rest
                   ("||", rest)         -> TokenOr     : lexer rest 
                   ("-", rest)          -> TokenMinus  : lexer rest
                   ("==", rest)         -> TokenEq     : lexer rest
                   ("=", rest)          -> TokenAssign : lexer rest
                   _ -> error "Lexical error: símbolo inválido!"
