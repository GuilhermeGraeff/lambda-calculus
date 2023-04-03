module ExprGenerator where

import Test.QuickCheck

import Lexer
import TypeGenerator 

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

-- Os que podem retornar um TBool
--           | BTrue
--           | BFalse
--           | Paren Expr
--           | And Expr Expr
--           | Or Expr Expr
--           | If Expr Expr Expr
--           | Var String
--           | App Expr Expr 
--           | Let String Expr Expr
--           | RecordProj Expr String
--           | TupleProj Expr Int 
--           | IsNil Ty Expr 
--           | Head Ty Expr 
--           | Tail Ty Expr
--           | Not Expr
--           | Fix Expr 
--           | Eq Expr Expr

-- Os que podem retornar o TNum
--           | Num Int
--           | Paren Expr
--           | Plus Expr     
--           | Times Expr Expr
--           | Minus Expr Expr
--           | If Expr Expr Expr
--           | Var String
--           | App Expr Expr 
--           | Let String Expr Expr 
--           | RecordProj Expr String
--           | TupleProj Expr Int 
--           | Head Ty Expr 
--           | Tail Ty Expr

-- As comuns entre o TBool e o TNum
--           | Paren Expr
--           | If Expr Expr Expr
--           | Var String
--           | App Expr Expr 
--           | Let String Expr Expr
--           | RecordProj Expr String
--           | TupleProj Expr Int 
--           | Head Ty Expr 
--           | Tail Ty Expr


translate:: Int -> Gen Expr
translate depth = do types    <- generator depth
                     expr    <- genExpr types
                     return expr

-- Nessa função aqui vai todas as substituições
genExpr:: Ty -> Gen Expr
genExpr types | types == TBool = genBoolean
              | types == TNum  = genTNumExpr
              | otherwise     = genNil

genTNumExpr :: Gen Expr
genTNumExpr = do expr   <- frequency (genParen : genCommonExpr)
                 return expr


-- genTBoolExpr :: Gen Expr


genCommonExpr :: [Gen Expr]
genCommonExpr = [genParen,
                 genIf,      
                 genVar,
                 genApp,
                 genLet,
                 genRecordPro,
                 genTupleProj,
                 genHead,
                 genTail]  

genParen :: Gen Expr
genParen = do expr <- elements [Paren BTrue]
              return expr

genIf :: Gen Expr
genIf = do expr <- elements [If BTrue BTrue BTrue]
           return expr

genVar :: Gen Expr
genVar = do expr <- elements [Var "boi"]
            return expr

genApp :: Gen Expr
genApp = do expr <- elements [App BTrue BTrue]
            return expr

genLet :: Gen Expr
genLet = do expr <- elements [Let "vaca" BTrue BTrue]
            return expr

genRecordPro :: Gen Expr
genRecordPro = do expr <- elements [RecordProj BTrue "terneiro"]
                  return expr
          
genTupleProj :: Gen Expr
genTupleProj = do expr <- elements [TupleProj BTrue 0]
                  return expr

genHead :: Gen Expr
genHead = do expr <- elements [Head TBool BTrue]
             return expr
            

genTail :: Gen Expr
genTail = do expr <- elements [Tail TBool BTrue]
             return expr

genBoolean :: Gen Expr
genBoolean = do expr <- elements [BTrue, BFalse]
                return expr

genNum :: Gen Int
genNum = do num <- choose(1,maxConstuctSize)
            return num


genNil :: Gen Expr
genNil = do expr <- elements [Nil TBool]
            return expr