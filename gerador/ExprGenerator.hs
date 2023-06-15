module ExprGenerator where

import Test.QuickCheck

import Lexer
import TypeGenerator 

type Ctx = [(String, Ty)]

isEmptyCtx :: [(String, Ty)] -> Bool
isEmptyCtx [] = True
isEmptyCtx _ = False

genName :: Ctx -> String
genName ctx = "x" ++ show (length ctx)

generator:: Int -> Gen Expr
generator depth = do types    <- typeGenerator depth
                     expr     <- genExpr depth [] types
                     return expr

genExpr:: Int -> Ctx -> Ty -> Gen Expr
genExpr 0 ctx ty                 = genLeaf ctx ty
genExpr depth ctx ty@(TFun _ _)  = genBranch depth ctx ty 
genExpr depth ctx ty@(List _)   = genBranch depth ctx ty 
genExpr depth ctx ty@TBool       = genBranch depth ctx ty
genExpr depth ctx ty@TNum        = genBranch depth ctx ty 
genExpr depth ctx ty             = genLeaf ctx ty
-- É desse jeito ainda pra não catar os problema dos tipos que ainda não existem

genBranch :: Int -> Ctx -> Ty -> Gen Expr
genBranch depth ctx ty = do commonBranch <- frequency [(7, genVar depth ctx ty), (1, genLet depth ctx ty), (1, genApp depth ctx ty), (1, genParen depth ctx ty), (1, genIf depth ctx ty), (1, genHeadList depth ctx ty)]
                            boolBranch   <- frequency [(7, genVar depth ctx ty), (1, genLet depth ctx ty), (1, genApp depth ctx ty), (1, genParen depth ctx ty), (1, genIf depth ctx ty), (1, genHeadList depth ctx ty), (1, genAnd depth ctx), (1, genOr depth ctx), (1, genBoolean)] 
                            numBranch    <- frequency [(7, genVar depth ctx ty), (1, genLet depth ctx ty), (1, genApp depth ctx ty), (1, genParen depth ctx ty), (1, genIf depth ctx ty), (1, genHeadList depth ctx ty), (1, genPlus depth ctx), (1, genMinus depth ctx), (1, genTimes depth ctx), (1, genNum)] 
                            return (
                                    if ty == TBool then
                                      boolBranch
                                    else if ty == TNum then
                                      numBranch
                                    else
                                      case ty of
                                        TFun _ _ -> commonBranch
                                        List _   -> commonBranch
                                        _        -> UnknownExpression
                                   )


genLeaf :: Ctx -> Ty -> Gen Expr
genLeaf ctx TBool = do leaf <- frequency [
                                            (1, genBoolean),
                                            (1, genVar 0 ctx TBool)
                                          ]
                       return leaf
genLeaf ctx TNum = do leaf <- frequency [
                                          (1, genNum),
                                          (1, genVar 0 ctx TNum)
                                        ]
                      return leaf
genLeaf ctx (TFun inputType outputType) = do body <- genExpr 0 ((vname, inputType):ctx) outputType
                                             parameter <- genExpr 0 ctx inputType
                                             return (App (Lam vname inputType body) (parameter))
                                          where 
                                            vname = genName ctx
genLeaf ctx (List listType) = do list <- genList 0 ctx (List listType)
                                 return (Paren list)
genLeaf _ _ = genUnknown
                

genList :: Int -> Ctx -> Ty -> Gen Expr 
genList depth ctx (List listType) = do stop <- genRandomNaturalSize
                                       expression <- genExpr (div depth 2) ctx listType 
                                       continueGeneration <- genList (div depth 2) ctx (List listType)
                                       nil <- genNil listType
                                       return (
                                                if stop < 2 then
                                                  Cons listType (Paren expression) nil
                                                else 
                                                  Cons listType (Paren expression) continueGeneration
                                              )
                                         

genHeadList :: Int -> Ctx -> Ty -> Gen Expr
genHeadList depth ctx listType = do list <- genList depth ctx (List listType)
                                    return (Head listType (Paren list))

genTailList :: Int -> Ctx -> Ty -> Gen Expr
genTailList depth ctx expressionType = do expr <- genExpr (div depth 2) ctx expressionType
                                          return (Paren expr)

genUnknown :: Gen Expr
genUnknown = do expression <- elements [UnknownExpression]
                return expression

genBoolean :: Gen Expr
genBoolean = do expression <- elements [BTrue, BFalse]
                return expression

genNum :: Gen Expr
genNum = do num <- choose(1,maxConstuctSize)
            return (Num num)

genNil :: Ty -> Gen Expr
genNil nilType = elements[Nil nilType]        

genPlus :: Int -> Ctx -> Gen Expr
genPlus depth ctx = do leftExpression <- genExpr (div depth 2) ctx TNum
                       rightExpression <- genExpr (div depth 2) ctx TNum
                       return (Plus leftExpression rightExpression)

genMinus :: Int -> Ctx -> Gen Expr
genMinus depth ctx = do leftExpression <- genExpr (div depth 2) ctx TNum
                        rightExpression <- genExpr (div depth 2) ctx TNum
                        return (Minus leftExpression rightExpression)

genTimes :: Int -> Ctx -> Gen Expr
genTimes depth ctx = do leftExpression <- genExpr (div depth 2) ctx TNum
                        rightExpression <- genExpr (div depth 2) ctx TNum
                        return (Times leftExpression rightExpression)

genLet :: Int -> Ctx -> Ty -> Gen Expr
genLet depth ctx expressionType = do variableType <- typeGenerator (div depth 2)
                                     variableExpression <- genExpr (div depth 2) ctx variableType  
                                     body <- genExpr (div depth 2) ((vname, variableType):ctx) expressionType
                                     return (Let vname variableExpression body)
                                  where
                                    vname = genName ctx


genLam :: Int -> Ctx -> Ty -> Ty -> Gen Expr 
genLam depth ctx parameterType expressionType = do body <- genExpr (div depth 2) ((vname, parameterType):ctx) expressionType 
                                                   return (Lam vname parameterType body) 
                                                where 
                                                   vname = genName ctx
                                    
genApp :: Int -> Ctx -> Ty -> Gen Expr
genApp depth ctx expressionType = do parameterType   <- typeGenerator (div depth 2)
                                     body            <- genLam (div depth 2) ctx parameterType expressionType
                                     parameter       <- genExpr (div depth 2) ctx parameterType
                                     return (App body parameter)


genAnd :: Int -> Ctx -> Gen Expr
genAnd depth ctx = do leftExpression <- genExpr (div depth 2) ctx TBool
                      rightExpression <- genExpr (div depth 2) ctx TBool
                      return (And leftExpression rightExpression)

genOr :: Int -> Ctx -> Gen Expr
genOr depth ctx = do leftExpression <- genExpr (div depth 2) ctx TBool
                     rightExpression <- genExpr (div depth 2) ctx TBool
                     return (Or leftExpression rightExpression)


genParen :: Int -> Ctx -> Ty -> Gen Expr
genParen depth ctx expressionType = do expr <- genExpr (div depth 2) ctx expressionType
                                       return (Paren expr)

genIf :: Int -> Ctx -> Ty -> Gen Expr 
genIf depth ctx expressionType = do condition  <- genExpr (div depth 2) ctx TBool
                                    branchThen <- genExpr (div depth 2) ctx expressionType
                                    branchElse <- genExpr (div depth 2) ctx expressionType
                                    return (If condition branchThen branchElse)

genVar :: Int -> Ctx -> Ty -> Gen Expr 
genVar depth [] ty = genExpr depth [] ty
genVar depth ((name,expressionType):xs) ty = do variable <- elements [Var name]
                                                getNext <- genVar depth xs ty
                                                return (
                                                        if expressionType == ty then
                                                          variable
                                                        else 
                                                          getNext
                                                       )
