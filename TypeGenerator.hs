module TypeGenerator where

import Test.QuickCheck

import Lexer 

-- Define: 
maxConstuctSize :: Int 
maxConstuctSize = 10

nameList :: String
nameList = "oiboivacaterneiro"

-- No ghci: generate $ generator d
--                             /    \
--                           (1,2,3...)
--         Depth -> Árvore de Sintaxe Abstrata Diricionada pelos tipos
generator :: Int -> Gen Ty
generator depth = do g      <- genRandomType depth
                     return g

genRandomType :: Int -> Gen Ty
genRandomType depth = do size    <- genRandomNaturalSize
                         ty      <- if depth > 0  
                                    then frequency [(2, genRecordType depth size),
                                                    (2, genTupleType depth size),
                                                    (2, genListType depth size),
                                                    (2, genFunType depth size),
                                                    (4, genBasicType)]  
                                    else genBasicType
                         return ty

-- Tipos que podem ser gerados: 

genRecordTypeItem :: Int -> Gen (String, Ty)
genRecordTypeItem depth = do name      <- genRandomName
                             tyType    <- genRandomType (depth - 1)
                             return (name, tyType)

genRecordType :: Int -> Int -> Gen Ty
genRecordType depth size = do tyRecordVector    <- vectorOf size (genRecordTypeItem depth)
                              return (TRecord tyRecordVector)

genTupleType :: Int -> Int -> Gen Ty
genTupleType depth size = do tyVector    <- vectorOf size (genRandomType (depth -1))
                             return (TTuple tyVector)

genListType :: Int -> Int -> Gen Ty
genListType depth size = do tyType    <- genRandomType (depth -1)
                            return (List tyType)

genFunType :: Int -> Int -> Gen Ty
genFunType depth size = do argumentTy    <- genRandomType (depth - 1)
                           returnTy      <- genRandomType (depth - 1)
                           return (TFun argumentTy returnTy)

genBasicType :: Gen Ty
genBasicType = do ty <- elements [TBool, TNum]
                  return ty

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




