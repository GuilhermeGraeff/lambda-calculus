module ExprGenerator where

import Test.QuickCheck

import Lexer
import TypeGenerator 

 -- Type definitions

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
                     expr     <- genExpr types depth
                     return expr

-- Nessa função aqui vai todas as substituições
genExpr:: Ty -> Int -> Gen Expr
genExpr types depth | types == TBool && depth > 0 = genBoolBranch TBool depth
                    | types == TBool && depth <= 0 = genBooleanLeaf
                    | types == TNum && depth > 0 = genNumBranch TNum depth
                    | types == TNum && depth <= 0 = genNumLeaf
                    | otherwise = genBoolBranch TBool depth -- temporário, o otherwise aqui tem que ser uma exception sepa

                    -- | TTuple [Ty]
                    -- | TRecord [(String,Ty)]
                    -- | List Ty 

-- Eu posso fazer vários expressions, um pra cada tipo, onde precisar ser função por exemplo, ou algo do tipo
genBoolBranch:: Ty -> Int -> Gen Expr
genBoolBranch types depth = do expression_1 <- genExpr TBool (depth-1)
                               expression_2 <- genExpr TBool (depth-1)
                               expression_3 <- genExpr TBool (depth-1)
                               branch <- frequency [(1, genBooleanLeaf),
                                                    (20, elements [
                                                        (Paren (expression_1)),
                                                        (And (expression_1) (expression_2)),
                                                        (Or (expression_1) (expression_2)),
                                                        (If (expression_1) (expression_2) (expression_3)),
                                                        (Var "bool_var"),
                                                        (App (expression_1) (expression_2) ),
                                                        (Let "bool_let" (expression_1) (expression_2)),
                                                        (RecordProj (expression_1) "recor_proj_key_bool"),
                                                        (TupleProj (expression_1) 10),
                                                        (IsNil TBool (expression_1) ),
                                                        (Head TBool (expression_1) ),
                                                        (Tail TBool (expression_1)),
                                                        (Not (expression_1)),
                                                        (Fix (expression_1) ),
                                                        (Eq (expression_1) (expression_2))])
                                                    ]
                               return branch

genNumBranch:: Ty -> Int -> Gen Expr
genNumBranch types depth = do expression_1 <- genExpr TNum (depth-1)
                              expression_2 <- genExpr TNum (depth-1)
                              expression_3 <- genExpr TNum (depth-1)
                              branch <- frequency [(1, genNumLeaf),
                                                   (20, elements [
                                                       (Paren (expression_1)),
                                                       (Plus (expression_1) (expression_2)),
                                                       (Times (expression_1) (expression_2)),
                                                       (Minus (expression_1) (expression_2)),
                                                       (If (expression_1) (expression_2) (expression_3)),
                                                       (Var "num_var"),
                                                       (App (expression_1) (expression_2) ),
                                                       (Let "num_let" (expression_1) (expression_2)),
                                                       (RecordProj (expression_1) "record_proj_key_num"),
                                                       (TupleProj (expression_1) 10),
                                                       (Head TNum (expression_1)),
                                                       (Tail TNum (expression_1))
                                                    ])
                                                  ]
                              return branch

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

genBooleanLeaf :: Gen Expr
genBooleanLeaf = do expr <- elements [BTrue, BFalse]
                    return expr

genNumLeaf :: Gen Expr
genNumLeaf = do inteiro <- genNum   
                expr <- elements [Num inteiro]
                return expr

genNum :: Gen Int
genNum = do num <- choose(1,maxConstuctSize)
            return num

genNil :: Gen Expr
genNil = do expr <- elements [Nil TBool]
            return expr