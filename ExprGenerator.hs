module ExprGenerator where

import Test.QuickCheck

import Lexer
import TypeGenerator 

type Ctx = [(String, Ty)]

generator:: Int -> Gen Expr
generator depth = do types    <- typeGenerator depth
                     expr     <- genExpr depth types
                     return expr

genExpr:: Int -> Ty -> Gen Expr
genExpr 0 ty                = genLeaf ty
genExpr depth ty@(TFun _ _) = genBranch depth ty 
genExpr depth ty@TBool      = genBranch depth ty
genExpr depth ty@TNum       = genBranch depth ty 
genExpr depth ty             = genLeaf ty
-- É desse jeito ainda pra não catar os problema dos tipos que ainda não existem

genBranch :: Int -> Ty -> Gen Expr
genBranch depth ty = do commonBranch <- frequency [(1, genLet depth ty), (1, genApp depth ty), (1, genParen depth ty), (1, genIf depth ty), (1, genLam depth ty)] -- genvar vai aq tmbm
                        boolBranch   <- frequency [(1, genLet depth ty), (1, genApp depth ty), (1, genParen depth ty), (1, genIf depth ty), (1, genLam depth ty), (1, genAnd depth), (1, genOr depth)]  -- genvar vai aq tmbm
                        numBranch    <- frequency [(1, genLet depth ty), (1, genApp depth ty), (1, genParen depth ty), (1, genIf depth ty), (1, genLam depth ty), (1, genPlus depth), (1, genMinus depth), (1, genTimes depth)]  -- genvar vai aq tmbm
                        return (if ty == TBool then
                                  boolBranch
                                else if ty == TNum then
                                  numBranch
                                else
                                  case ty of
                                    TFun _ _ -> commonBranch
                                    _        -> UnknownExpression
                               )


genLeaf :: Ty -> Gen Expr
genLeaf TBool = do leaf <- frequency [(1, genBoolean)] -- genvar vai aqi também sepa
                   return leaf
genLeaf TNum = do leaf <- frequency [(1, genNum)] -- genvar vai aqi também sepa
                  return leaf
genLeaf (TFun a b) = genExpr 0 b
genLeaf _ = genUnknown
                

genUnknown :: Gen Expr
genUnknown = do expression <- elements [UnknownExpression]
                return expression

genBoolean :: Gen Expr
genBoolean = do expression <- elements [BTrue, BFalse]
                return expression

genNum :: Gen Expr
genNum = do num <- choose(1,maxConstuctSize)
            return (Num num)

genPlus :: Int -> Gen Expr
genPlus depth = do leftExpression <- genExpr (depth - 1) TNum
                   rightExpression <- genExpr (depth - 1) TNum
                   return (Plus leftExpression rightExpression)

genMinus :: Int -> Gen Expr
genMinus depth = do leftExpression <- genExpr (depth - 1) TNum
                    rightExpression <- genExpr (depth - 1) TNum
                    return (Minus leftExpression rightExpression)

genTimes :: Int -> Gen Expr
genTimes depth = do leftExpression <- genExpr (depth - 1) TNum
                    rightExpression <- genExpr (depth - 1) TNum
                    return (Times leftExpression rightExpression)

genLet :: Int -> Ty -> Gen Expr
genLet depth expressionType = do vname <- (genRandomName)
                                 variableType <- typeGenerator (depth - 1)
                                 variableExpression <- genExpr (depth - 1) variableType  
                                 body <- genExpr (depth - 1) expressionType -- Alimenta o contexto aqui praga
                                 return (Let vname variableExpression body)


genApp :: Int -> Ty -> Gen Expr
genApp depth expressionType = do parameterType   <- typeGenerator (depth - 1)
                                 body            <- genExpr (depth - 1) (TFun parameterType expressionType)
                                 parameter       <- genExpr (depth - 1) parameterType
                                 return (App body parameter)


genAnd :: Int -> Gen Expr
genAnd depth = do leftExpression <- genExpr (depth - 1) TBool
                  rightExpression <- genExpr (depth - 1) TBool
                  return (And leftExpression rightExpression)

genOr :: Int -> Gen Expr
genOr depth = do leftExpression <- genExpr (depth - 1) TBool
                 rightExpression <- genExpr (depth - 1) TBool
                 return (Or leftExpression rightExpression)


genParen :: Int -> Ty -> Gen Expr
genParen depth expressionType = do expr <- genExpr (depth - 1) expressionType
                                   return (Paren expr)

genIf :: Int -> Ty -> Gen Expr 
genIf depth expressionType = do condition  <- genExpr (depth - 1) TBool
                                branchThen <- genExpr (depth - 1) expressionType
                                branchElse <- genExpr (depth - 1) expressionType
                                return (If condition branchThen branchElse)


genLam :: Int -> Ty -> Gen Expr 
genLam depth expressionType = do vname <- (genRandomName) 
                                 inputType <- typeGenerator (depth - 1)
                                 body <- genExpr depth expressionType -- alimentar o ctx de variaveis com o vname e o tipo deste vname que é o inputType
                                 return (Lam vname inputType body)  -- Talvez isso aqui tenha que ser garantido que o que vai ser aplicado ao lam tem esse inputType


-- genVar :: Int -> Ctx -> Ty -> Gen Expr 
-- genVar depth ctx ty = elements [Var x | (x, ty') <- ctx, ty' == ty]

----------------------------------------------------------------------------------