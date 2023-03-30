module ExprGenerator where

import Test.QuickCheck

import Lexer
import ExprGenerator 

-- -- Type definitions

-- data Ty = TBool
--         | TNum
--         | TFun Ty Ty
--         | TTuple [Ty]
--         | TRecord [(String,Ty)]
--         | List Ty 
--         deriving (Show, Eq)

-- -- Expression definitions

-- data Expr = BTrue
--           | BFalse
--           | Num Int
--           | Paren Expr
--           | Plus Expr     
--           | Times Expr Expr
--           | Minus Expr Expr
--           | And Expr Expr
--           | Or Expr Expr
--           | If Expr Expr Expr
--           | Var String
--           | Lam String Ty Expr
--           | App Expr Expr 
--           | Let String Expr Expr 
--           | Tuple [Expr]
--           | TupleProj Expr Int 
--           | Record [(String,Expr)] 
--           | RecordProj Expr String
--           | Fix Expr 
--           | Eq Expr Expr
--           | Not Expr
--           | Nil Ty
--           | Cons Ty Expr Expr
--           | IsNil Ty Expr 
--           | Head Ty Expr 
--           | Tail Ty Expr
--           deriving Eq


translate:: Int -> Gen Expr
translate depth = do typeTree    <- generator depth
                     expr        <- genExpr typeTree
                     return expr

-- Nessa função aqui vai todas as substituições
genExpr:: Ty -> Gen Expr
genExpr typeTree | typeTree == TBool = genBoolean
                 | typeTree == TNum  = genNum
                 | otherwise         = genNil



genBoolean :: Gen Expr
genBoolean = do expr <- elements [BTrue, BFalse]
                return expr

genNum :: Gen Expr
genNum = do num <-
     <- do <- choose(1,maxConstuctSize)


genNil :: Gen Expr
genNil = do expr <- elements [Nil TBool]
            return expr