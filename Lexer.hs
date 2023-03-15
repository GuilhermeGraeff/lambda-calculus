module Lexer where

import Data.Char
import Data.List (intercalate)

-- Type definitions

data Ty = TBool
        | TNum
        | TFun Ty Ty
        | TTuple [Ty]
        | TRecord [(String,Ty)]
        | List Ty 
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
          | Nil Ty
          | Cons Ty Expr Expr
          | IsNil Ty Expr 
          | Head Ty Expr 
          | Tail Ty Expr
          deriving Eq

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
           | TokenList
           | TokenNil
           | TokenCons 
           | TokenIsNil
           | TokenHead
           | TokenTail
           | TokenLSqBrack
           | TokenRSqBrack 
           deriving Show

-- Compiling to concrete syntax

instance Show Expr where 
    show BTrue = "true"
    show BFalse = "false"
    show (Num n) = show n
    show (Lam v t b) = "\\" ++ v ++ ":" ++ show t ++ " -> " ++ show b 
    show (Tuple l) = "(" ++ intercalate ", " (map show l) ++ ")"
    show (Record l) = "{" ++ intercalate ", " (map (\(k,p) -> k ++ " = " ++ show p) l) ++ "}"
    show (Nil _) = "[]"
    show (Cons _ e1 e2) = show e1 ++ ":" ++ show e2 


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
lexer ('[':cs) = TokenLSqBrack : lexer cs 
lexer (']':cs) = TokenRSqBrack : lexer cs 
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
                ("List", rest) -> TokenList : lexer rest 
                ("nil", rest) -> TokenNil : lexer rest 
                ("cons", rest) -> TokenCons : lexer rest 
                ("isnil", rest) -> TokenIsNil : lexer rest 
                ("head", rest) -> TokenHead : lexer rest 
                ("tail", rest) -> TokenTail : lexer rest 
                (var, rest)      -> TokenVar var : lexer rest

lexSymbol cs = case span isToken cs of
                   ("->", rest)         -> TokenArrow  : lexer rest
                   ("&&", rest)         -> TokenAnd    : lexer rest
                   ("||", rest)         -> TokenOr     : lexer rest 
                   ("-", rest)          -> TokenMinus  : lexer rest
                   ("==", rest)         -> TokenEq     : lexer rest
                   ("=", rest)          -> TokenAssign : lexer rest
                   _ -> error "Lexical error: símbolo inválido!"
