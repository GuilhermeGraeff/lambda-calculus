module ExprGenerator where

import Test.QuickCheck

import Lexer
import TypeGenerator 

generator:: Int -> Gen Expr
generator depth = do types    <- typeGenerator depth
                     expr     <- genExpr depth types
                     return expr

genExpr:: Int -> Ty -> Gen Expr
genExpr 0 ty             = genLeaf ty
genExpr depth ty@(TFun _ _) = genBranch depth ty 
genExpr depth ty@TBool      = genBranch depth ty
genExpr depth ty@TNum       = genBranch depth ty 
genExpr depth _          = genLeaf TBool

genBranch :: Int -> Ty -> Gen Expr
genBranch depth ty = do branch <- frequency [ 
                                             (1, genLet depth ty ty),
                                             (1, genApp depth ty),
                                             (1, genAnd depth),
                                             (1, genOr depth),
                                             (1, genParen depth ty),
                                             (1, genIf depth ty),
                                             (1, genLam depth ty ty)
                                             -- (1, genVar)
                                            ]
                        return branch

genLeaf :: Ty -> Gen Expr
genLeaf ty = do leaf <- frequency [ 
                                   (1, genBoolean),
                                   (1, genNum)
                                   -- (1, genVar)
                                  ]
                return leaf

genBoolean :: Gen Expr
genBoolean = do expr <- elements [BTrue, BFalse]
                return expr

genNum :: Gen Expr
genNum = do num <- choose(1,maxConstuctSize)
            return (Num num)

genLet :: Int -> Ty -> Ty -> Gen Expr
genLet depth ty ty' = do vname <- (genRandomName)
                         expr1 <- genExpr (depth - 1) ty
                         expr2 <- genExpr (depth - 1) ty'
                         return (Let vname expr1 expr2)

genApp :: Int -> Ty -> Gen Expr
genApp depth ty = do parameterType   <- typeGenerator (depth - 1)
                     body            <- genExpr (depth - 1) (TFun parameterType ty)
                     parameter       <- genExpr (depth - 1) parameterType
                     return (App body parameter)


genAnd :: Int -> Gen Expr
genAnd depth = do expr1 <- genExpr (depth - 1) TBool
                  expr2 <- genExpr (depth - 1) TBool
                  return (And expr1 expr2)

genOr :: Int -> Gen Expr
genOr depth = do expr1 <- genExpr (depth - 1) TBool
                 expr2 <- genExpr (depth - 1) TBool
                 return (Or expr1 expr2)


genParen :: Int -> Ty -> Gen Expr
genParen depth ty = do expr <- genExpr (depth - 1) ty
                       return (Paren expr)

genIf :: Int -> Ty -> Gen Expr 
genIf depth ty = do cond  <- genExpr (depth - 1) TBool
                    bthen <- genExpr (depth - 1) ty
                    belse <- genExpr (depth - 1) ty
                    return (If cond bthen belse)


genLam :: Int -> Ty -> Ty -> Gen Expr 
genLam depth int outt = do vname <- (genRandomName) 
                           body <- genExpr depth outt -- alimentar o ctx de variaveis
                           return (Lam vname int body)

-- genVar :: Int -> Ctx -> Ty -> Gen Expr 
-- genVar depth ctx ty = elements [Var x | (x, ty') <- ctx, ty' == ty]

----------------------------------------------------------------------------------