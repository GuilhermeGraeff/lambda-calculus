module Generator where

import Test.QuickCheck

-- Deixando aqui para visualizar melhor ..
data Ty = TBool
        | TNum
        | TFun Ty Ty
        | TTuple [Ty]
        | TRecord [(String,Ty)]
        deriving (Show, Eq)





--        Depth -> max Size -> Árvore de Sintaxe Abstrata
generator :: Int -> Int -> Gen Ty
generator d s = do generation <- genRandomType d s
                   return generation

-- d => Depth, s => Size
genRandomType :: Int -> Int -> Gen Ty
genRandomType d s = do t <- if d /= 0 
                            then frequency [(2, genRecordType d s), 
                                            (2, genTupleType d s), 
                                            (2, genFunType d s), 
                                            (2, genBasicType)]   
                            else genBasicType
                       return t 
-- genRandomType :: Int -> Int -> Gen Ty
-- genRandomType d s = do t <- if d /= 0 
--                             then frequency [(2, genRecordType d (do g <- genRandomNatural s
--                                                                     return g
--                                                                   )),
--                                             (2, genTupleType d (do g <- genRandomNatural s
--                                                                    return g
--                                                                   )),
--                                             (2, genFunType d (do g <- genRandomNatural s
--                                                                  return g
--                                                                   )),
--                                             (2, genBasicType)]   
--                             else genBasicType
--                        return t 


-- Tipos que podem ser gerados: 
genRecordType :: Int -> Int -> Gen Ty
genRecordType d s = do { t1 <- genRandomType (d - 1) s
                       ; t2 <- genRandomType (d - 1) s
                       ; t3 <- genRandomType (d - 1) s
                       ; return (TRecord [("A", t1), ("B", t2), ("C", t3)]) }

-- genRecordType :: Int -> Int -> Gen Ty
-- genRecordType d s = do { return (TRecord ( do list <- generateMultipleRecordItems d s 
--                                               return map getGenOutOfTheWay list 
--                                          )) }

-- getGenOutOfTheWay :: Gen Ty -> Ty
-- getGenOutOfTheWay (Gen t) = t


-- generateMultipleRecordItems :: Int -> Int -> [([Char], Gen Ty)]
-- generateMultipleRecordItems d s = (("a", (genRandomType (d - 1) s)) : (generateMultipleRecordItems d (s -1)))



genTupleType :: Int -> Int -> Gen Ty
genTupleType d s = do { t1 <- genRandomType (d - 1) s
                      ; t2 <- genRandomType (d - 1) s
                      ; t3 <- genRandomType (d - 1) s
                      ; return (TTuple [t1, t2, t3]) }

genFunType :: Int -> Int -> Gen Ty
genFunType d s = do { t1 <- genRandomType (d - 1) s
                    ; t2 <- genRandomType (d - 1) s
                    ; return (TFun t1 t2) }


genBasicType :: Gen Ty
genBasicType = do t <- elements [TBool, TNum]
                  return t

-- Utils:
--     genRandomNatural'


genRandomNatural :: Int -> Gen Int
genRandomNatural max = do r <- choose(1,max)
                          return r
