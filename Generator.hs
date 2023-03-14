module Generator where

import Test.QuickCheck

-- Deixando aqui para visualizar melhor ..
data Ty = TBool
        | TNum
        | TFun Ty Ty
        | TTuple [Ty]
        | TRecord [(String, Ty)]
        deriving (Show, Eq)


maxConstuctSize :: Int 
maxConstuctSize = 10

nameList :: String
nameList = "ab"


--        Depth -> max Size -> Árvore de Sintaxe Abstrata
generator :: Int -> Gen Ty
generator d = do g      <- genRandomType d
                 return g

-- d => Depth, s => Size
-- genRandomType :: Int -> Int -> Gen Ty
-- genRandomType d s = do t <- if d /= 0 
--                             then frequency [(2, genRecordType d s), 
--                                             (2, genTupleType d s), 
--                                             (2, genFunType d s), 
--                                             (2, genBasicType)]   
--                             else genBasicType
--                        return t 
genRandomType :: Int -> Gen Ty
genRandomType d = do s     <- genRandomNaturalSize
                     t     <- if d > 0  
                              then frequency [(2, genTupleType d s),
                                              (2, genRecordType d s),
                                              (2, genFunType d s),
                                              (2, genBasicType)]  
                              else genBasicType
                     return t 


-- Tipos que podem ser gerados: 
-- genRecordType :: Int -> Int -> Gen Ty
-- genRecordType d s = do t1 <- genRandomType (d - 1)
--                        t2 <- genRandomType (d - 1)
--                        t3 <- genRandomType (d - 1)
--                        return (TRecord [("A", t1), ("B", t2), ("C", t3)])

genRecordTypeItem :: Int -> Gen (String, Ty)
genRecordTypeItem d = do n      <- genRandomName
                         t      <- genRandomType (d - 1)
                         return (n, t)

genRecordType :: Int -> Int -> Gen Ty
genRecordType d s = do t      <- vectorOf s (genRecordTypeItem d)
                       return (TRecord t)


-- generateMultipleRecordItems :: Int -> Int -> [(String, Ty)]
-- generateMultipleRecordItems d 0 = []
-- generateMultipleRecordItems d s = do n      <- genRandomName
--                                      t      <- genRandomType (d - 1)
--                                      g      <- (n, t) 
--                                      gs     <- (generateMultipleRecordItems d (s - 1))
--                                      return (g : gs)
                                

genTupleType :: Int -> Int -> Gen Ty
genTupleType d s = do t      <- vectorOf s (genRandomType (d -1))
                      return (TTuple t)


genFunType :: Int -> Int -> Gen Ty
genFunType d s = do t1     <- genRandomType (d - 1)
                    t2     <- genRandomType (d - 1)
                    return (TFun t1 t2)


genBasicType :: Gen Ty
genBasicType = do t <- elements [TBool, TNum]
                  return t

-- Utils:
--     genRandomNaturalSize'


genRandomNaturalSize :: Gen Int
genRandomNaturalSize = choose(1,maxConstuctSize)
                        


genRandomName :: Gen String
genRandomName = listOf (elements nameList)

                          
