module Generator where

import Test.QuickCheck

import Lexer 

-- Define: 
maxConstuctSize :: Int 
maxConstuctSize = 10

nameList :: String
nameList = "oiboivacaterneiro"

--        Depth -> Árvore de Sintaxe Abstrata Diricionada pelos tipos
generator :: Int -> Gen Ty
generator d = do g      <- genRandomType d
                 return g

genRandomType :: Int -> Gen Ty
genRandomType d = do s     <- genRandomNaturalSize
                     t     <- if d > 0  
                              then frequency [(2, genRecordType d s),
                                              (2, genTupleType d s),
                                              (2, genListType d s),
                                              (2, genFunType d s),
                                              (2, genBasicType)]  
                              else genBasicType
                     return t 

-- Tipos que podem ser gerados: 

genRecordTypeItem :: Int -> Gen (String, Ty)
genRecordTypeItem d = do n      <- genRandomName
                         t      <- genRandomType (d - 1)
                         return (n, t)

genRecordType :: Int -> Int -> Gen Ty
genRecordType d s = do t      <- vectorOf s (genRecordTypeItem d)
                       return (TRecord t)

genTupleType :: Int -> Int -> Gen Ty
genTupleType d s = do t      <- vectorOf s (genRandomType (d -1))
                      return (TTuple t)

genListType :: Int -> Int -> Gen Ty
genListType d s = do t      <- genRandomType (d -1)
                     return (List t)

genFunType :: Int -> Int -> Gen Ty
genFunType d s = do t1     <- genRandomType (d - 1)
                    t2     <- genRandomType (d - 1)
                    return (TFun t1 t2)

genBasicType :: Gen Ty
genBasicType = do t <- elements [TBool, TNum]
                  return t

-- Utils:
--     genRandomNaturalSize at most maxConstuctSize
--     genRandomName from nameList

genRandomNaturalSize :: Gen Int
genRandomNaturalSize = choose(1,maxConstuctSize)

genRandomName :: Gen String
genRandomName = sublistOf (nameList)

                          




































































































-- Trash Can
-- generateMultipleRecordItems :: Int -> Int -> [(String, Ty)]
-- generateMultipleRecordItems d 0 = []
-- generateMultipleRecordItems d s = do n      <- genRandomName
--                                      t      <- genRandomType (d - 1)
--                                      g      <- (n, t) 
--                                      gs     <- (generateMultipleRecordItems d (s - 1))
--                                      return (g : gs)
                                
-- genRecordType :: Int -> Int -> Gen Ty
-- genRecordType d s = do t1 <- genRandomType (d - 1)
--                        t2 <- genRandomType (d - 1)
--                        t3 <- genRandomType (d - 1)
--                        return (TRecord [("A", t1), ("B", t2), ("C", t3)])

-- d => Depth, s => Size
-- genRandomType :: Int -> Int -> Gen Ty
-- genRandomType d s = do t <- if d /= 0 
--                             then frequency [(2, genRecordType d s), 
--                                             (2, genTupleType d s), 
--                                             (2, genFunType d s), 
--                                             (2, genBasicType)]   
--                             else genBasicType
--                        return t 


-- Deixando aqui para visualizar melhor ..
-- data Ty = TBool
--         | TNum
--         | TFun Ty Ty
--         | TTuple [Ty]
--         | TRecord [(String, Ty)]
--         | List Ty
--         deriving (Show, Eq)




