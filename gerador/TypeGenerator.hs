module TypeGenerator where

import Test.QuickCheck

import Lexer 

-- Define: 
-- depth = Max^^^ Depth of the generated type

maxConstuctSize :: Int 
maxConstuctSize = 10

nameList :: String
nameList = "pqiweruoalsdfjkczxcvnmurtyfghjcvbn"
--

typeGenerator :: Int -> Gen Ty
typeGenerator depth = do g          <- genRandomType depth
                         return g

genRandomType :: Int -> Gen Ty
genRandomType depth = do size    <- genRandomNaturalSize
                         ty      <- if depth > 0  
                                    then frequency [(2, genListType depth size),
                                                    (2, genFunType depth size),
                                                    (2, genBasicType)]  
                                    else genBasicType
                         return ty

-- Removi dá func de cima as tuplas, listas e records - Trabalhos futuros
-- genRandomType :: Int -> Gen Ty
-- genRandomType depth = do size    <- genRandomNaturalSize
--                          ty      <- if depth > 0  
--                                     then frequency [(2, genRecordType depth size),
--                                                     (2, genTupleType depth size),
--                                                     (2, genListType depth size),
--                                                     (2, genFunType depth size),
--                                                     (4, genBasicType)]  
--                                     else genBasicType
--                          return ty

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
--

genRandomNaturalSize :: Gen Int
genRandomNaturalSize = choose(1,maxConstuctSize)

genRandomName :: Gen String
genRandomName = sublistOf (nameList)