module Generator where

import Test.QuickCheck

-- Deixando aqui para visualizar melhor ..
data Ty = TBool
        | TNum
        | TFun Ty Ty
        | TTuple [Ty]
        | TRecord [(String,Ty)]
        deriving (Show, Eq)

genType :: Gen Ty
genType = do t <- elements [TBool, TNum]
             return (TFun t t)


-- genBasicTypes :: Gen Ty     
-- genBasicTypes = do t <- elements [TBool, TNum]
--                  return TBool

genRecordType :: Gen Ty
genRecordType = do { t1 <- genTupleType
                   ; t2 <- genFunType
                   ; t3 <- genBasicType
                   ; return (Record [('A', t1), ('B', t2), ('C', t3)]) }

genTupleType :: Gen Ty
genTupleType = do {  t1 <- genBasicType
                   ; t2 <- genBasicType
                   ; t3 <- genBasicType
                   ; return (TTuple [t1, t2, t3]) }

genFunType :: Gen Ty
genFunType = do { t1 <- genBasicType
                ; t2 <- genTupleType
                ; return (TFun t1 t2) }


genBasicType :: Gen Ty
genBasicType = do t <- elements [TBool, TNum]
                  return t

