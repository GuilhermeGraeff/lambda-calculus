module ExprGenerator where

import Test.QuickCheck

import Lexer
import TypeGenerator 

type Ctx = [(String, Ty)]

isEmptyCtx :: [(String, Ty)] -> Bool
isEmptyCtx [] = True
isEmptyCtx _ = False

generator:: Int -> Gen Expr
generator depth = do types    <- typeGenerator depth
                     expr     <- genExpr depth [] types
                     return expr

genExpr:: Int -> Ctx -> Ty -> Gen Expr
genExpr 0 ctx ty                 = genLeaf ctx ty
genExpr depth ctx ty@(TFun _ _) = genBranch depth ctx ty 
genExpr depth ctx ty@TBool       = genBranch depth ctx ty
genExpr depth ctx ty@TNum        = genBranch depth ctx ty 
genExpr depth ctx ty             = genLeaf ctx ty
-- É desse jeito ainda pra não catar os problema dos tipos que ainda não existem

genBranch :: Int -> Ctx -> Ty -> Gen Expr
genBranch depth ctx ty = do commonBranch <- frequency [(10, genVar depth ctx ty), (1, genLet depth ctx ty), (1, genApp depth ctx ty), (1, genParen depth ctx ty), (1, genIf depth ctx ty), (1, genLam depth ctx ty)] -- genvar vai aq tmbm
                            boolBranch   <- frequency [(10, genVar depth ctx ty), (1, genLet depth ctx ty), (1, genApp depth ctx ty), (1, genParen depth ctx ty), (1, genIf depth ctx ty), (1, genLam depth ctx ty), (1, genAnd depth ctx), (1, genOr depth ctx)]  -- genvar vai aq tmbm
                            numBranch    <- frequency [(10, genVar depth ctx ty), (1, genLet depth ctx ty), (1, genApp depth ctx ty), (1, genParen depth ctx ty), (1, genIf depth ctx ty), (1, genLam depth ctx ty), (1, genPlus depth ctx), (1, genMinus depth ctx), (1, genTimes depth ctx)]  -- genvar vai aq tmbm
                            return (
                                    if ty == TBool then
                                      boolBranch
                                    else if ty == TNum then
                                      numBranch
                                    else
                                      case ty of
                                        TFun _ _ -> commonBranch
                                        _        -> UnknownExpression
                                   )


genLeaf :: Ctx -> Ty -> Gen Expr
genLeaf ctx TBool = do leaf <- frequency [(1, genBoolean)] -- genvar vai aqi também sepa
                       return leaf
genLeaf ctx TNum = do leaf <- frequency [(1, genNum)] -- genvar vai aqi também sepa
                      return leaf
genLeaf ctx (TFun inputType outputType) = do vname <- (genRandomName) 
                                             body <- genExpr 1 ((vname, inputType):ctx) outputType -- alimentar o ctx de variaveis com o vname e o tipo deste vname que é o inputType
                                             return (Lam vname inputType body)
genLeaf _ _ = genUnknown
                

genUnknown :: Gen Expr
genUnknown = do expression <- elements [UnknownExpression]
                return expression

genBoolean :: Gen Expr
genBoolean = do expression <- elements [BTrue, BFalse]
                return expression

genNum :: Gen Expr
genNum = do num <- choose(1,maxConstuctSize)
            return (Num num)

genPlus :: Int -> Ctx -> Gen Expr
genPlus depth ctx = do leftExpression <- genExpr (depth - 1) ctx TNum
                       rightExpression <- genExpr (depth - 1) ctx TNum
                       return (Plus leftExpression rightExpression)

genMinus :: Int -> Ctx -> Gen Expr
genMinus depth ctx = do leftExpression <- genExpr (depth - 1) ctx TNum
                        rightExpression <- genExpr (depth - 1) ctx TNum
                        return (Minus leftExpression rightExpression)

genTimes :: Int -> Ctx -> Gen Expr
genTimes depth ctx = do leftExpression <- genExpr (depth - 1) ctx TNum
                        rightExpression <- genExpr (depth - 1) ctx TNum
                        return (Times leftExpression rightExpression)

genLet :: Int -> Ctx -> Ty -> Gen Expr
genLet depth ctx expressionType = do vname <- (genRandomName)
                                     variableType <- typeGenerator (depth - 1)
                                     variableExpression <- genExpr (depth - 1) ctx variableType  
                                     body <- genExpr (depth - 1) ((vname, variableType):ctx) expressionType -- Alimenta o contexto aqui praga
                                     return (Let vname variableExpression body)

genLam :: Int -> Ctx -> Ty -> Gen Expr 
genLam depth ctx expressionType = do vname <- (genRandomName) 
                                     inputType <- typeGenerator (depth - 1)
                                     body <- genExpr (depth - 1) ((vname, inputType):ctx) expressionType -- alimentar o ctx de variaveis com o vname e o tipo deste vname que é o inputType
                                     return (Lam vname inputType body)  -- Talvez isso aqui tenha que ser garantido que o que vai ser aplicado ao lam tem esse inputType

genApp :: Int -> Ctx -> Ty -> Gen Expr
genApp depth ctx expressionType = do parameterType   <- typeGenerator (depth - 1)
                                     body            <- genExpr (depth - 1) ctx (TFun parameterType expressionType)
                                     parameter       <- genExpr (depth - 1) ctx parameterType
                                     return (App body parameter)


genAnd :: Int -> Ctx -> Gen Expr
genAnd depth ctx = do leftExpression <- genExpr (depth - 1) ctx TBool
                      rightExpression <- genExpr (depth - 1) ctx TBool
                      return (And leftExpression rightExpression)

genOr :: Int -> Ctx -> Gen Expr
genOr depth ctx = do leftExpression <- genExpr (depth - 1) ctx TBool
                     rightExpression <- genExpr (depth - 1) ctx TBool
                     return (Or leftExpression rightExpression)


genParen :: Int -> Ctx -> Ty -> Gen Expr
genParen depth ctx expressionType = do expr <- genExpr (depth - 1) ctx expressionType
                                       return (Paren expr)

genIf :: Int -> Ctx -> Ty -> Gen Expr 
genIf depth ctx expressionType = do condition  <- genExpr (depth - 1) ctx TBool
                                    branchThen <- genExpr (depth - 1) ctx expressionType
                                    branchElse <- genExpr (depth - 1) ctx expressionType
                                    return (If condition branchThen branchElse)

genVar :: Int -> Ctx -> Ty -> Gen Expr 
genVar depth [] ty = genExpr 1 [] ty
genVar depth ((name,expressionType):xs) ty = do variable <- elements [Var name]
                                                getNext <- genVar depth xs ty
                                                return (
                                                        if expressionType == ty then
                                                          variable
                                                        else 
                                                          getNext
                                                       )


