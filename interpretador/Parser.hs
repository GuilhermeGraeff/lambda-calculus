{-# OPTIONS_GHC -w #-}
module Parser where

import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.11

data HappyAbsSyn t4 t5 t6 t7
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,423) ([1920,33842,8214,0,0,0,0,56832,4303,123,0,0,0,0,30720,17184,360,51230,23056,0,0,0,1024,0,0,1,0,64,32768,12807,5764,0,4,30720,17184,360,0,0,0,64,0,0,0,0,4096,0,16384,0,0,16,0,16,30720,17279,492,53214,31632,2048,0,0,0,0,8312,26691,7681,4296,32858,12807,5764,33248,41228,30725,17184,360,16392,0,1920,33842,22,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,8192,0,0,0,0,0,0,7680,4296,32858,12807,5764,256,192,30720,17184,360,51230,23056,0,0,0,49153,0,0,256,4096,3072,0,0,0,0,0,30720,17215,494,53214,31536,0,128,57344,3837,1969,16248,60995,1,0,32768,12807,5764,33248,41228,30725,17184,360,16384,0,0,128,57344,3201,1441,16248,60611,4097,3072,0,0,0,0,0,0,0,0,448,0,2048,0,57344,3201,1441,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","Exp","ExpList","RecList","Type","true","false","num","'('","')'","'+'","'*'","'-'","\"&&\"","\"||\"","if","then","else","var","'\\\\'","':'","\"->\"","Bool","Number","let","in","'='","','","'.'","'{'","'}'","fix","letrec","\"==\"","'!'","%eof"]
        bit_start = st * 38
        bit_end = (st + 1) * 38
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..37]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (8) = happyShift action_2
action_0 (9) = happyShift action_4
action_0 (10) = happyShift action_5
action_0 (11) = happyShift action_6
action_0 (18) = happyShift action_7
action_0 (21) = happyShift action_8
action_0 (22) = happyShift action_9
action_0 (27) = happyShift action_10
action_0 (32) = happyShift action_11
action_0 (34) = happyShift action_12
action_0 (35) = happyShift action_13
action_0 (37) = happyShift action_14
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (8) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (8) = happyShift action_2
action_3 (9) = happyShift action_4
action_3 (10) = happyShift action_5
action_3 (11) = happyShift action_6
action_3 (13) = happyShift action_26
action_3 (14) = happyShift action_27
action_3 (15) = happyShift action_28
action_3 (16) = happyShift action_29
action_3 (17) = happyShift action_30
action_3 (18) = happyShift action_7
action_3 (21) = happyShift action_8
action_3 (22) = happyShift action_9
action_3 (27) = happyShift action_10
action_3 (31) = happyShift action_31
action_3 (32) = happyShift action_11
action_3 (34) = happyShift action_12
action_3 (35) = happyShift action_13
action_3 (36) = happyShift action_32
action_3 (37) = happyShift action_14
action_3 (38) = happyAccept
action_3 (4) = happyGoto action_25
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_2

action_5 _ = happyReduce_3

action_6 (8) = happyShift action_2
action_6 (9) = happyShift action_4
action_6 (10) = happyShift action_5
action_6 (11) = happyShift action_6
action_6 (18) = happyShift action_7
action_6 (21) = happyShift action_8
action_6 (22) = happyShift action_9
action_6 (27) = happyShift action_10
action_6 (32) = happyShift action_11
action_6 (34) = happyShift action_12
action_6 (35) = happyShift action_13
action_6 (37) = happyShift action_14
action_6 (4) = happyGoto action_23
action_6 (5) = happyGoto action_24
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (8) = happyShift action_2
action_7 (9) = happyShift action_4
action_7 (10) = happyShift action_5
action_7 (11) = happyShift action_6
action_7 (18) = happyShift action_7
action_7 (21) = happyShift action_8
action_7 (22) = happyShift action_9
action_7 (27) = happyShift action_10
action_7 (32) = happyShift action_11
action_7 (34) = happyShift action_12
action_7 (35) = happyShift action_13
action_7 (37) = happyShift action_14
action_7 (4) = happyGoto action_22
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_4

action_9 (21) = happyShift action_21
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (21) = happyShift action_20
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (21) = happyShift action_19
action_11 (6) = happyGoto action_18
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (8) = happyShift action_2
action_12 (9) = happyShift action_4
action_12 (10) = happyShift action_5
action_12 (11) = happyShift action_6
action_12 (18) = happyShift action_7
action_12 (21) = happyShift action_8
action_12 (22) = happyShift action_9
action_12 (27) = happyShift action_10
action_12 (32) = happyShift action_11
action_12 (34) = happyShift action_12
action_12 (35) = happyShift action_13
action_12 (37) = happyShift action_14
action_12 (4) = happyGoto action_17
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (21) = happyShift action_16
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (8) = happyShift action_2
action_14 (9) = happyShift action_4
action_14 (10) = happyShift action_5
action_14 (11) = happyShift action_6
action_14 (18) = happyShift action_7
action_14 (21) = happyShift action_8
action_14 (22) = happyShift action_9
action_14 (27) = happyShift action_10
action_14 (32) = happyShift action_11
action_14 (34) = happyShift action_12
action_14 (35) = happyShift action_13
action_14 (37) = happyShift action_14
action_14 (4) = happyGoto action_15
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (8) = happyShift action_2
action_15 (9) = happyShift action_4
action_15 (10) = happyShift action_5
action_15 (11) = happyShift action_6
action_15 (13) = happyShift action_26
action_15 (14) = happyShift action_27
action_15 (15) = happyShift action_28
action_15 (16) = happyShift action_29
action_15 (17) = happyShift action_30
action_15 (18) = happyShift action_7
action_15 (21) = happyShift action_8
action_15 (22) = happyShift action_9
action_15 (27) = happyShift action_10
action_15 (31) = happyShift action_31
action_15 (32) = happyShift action_11
action_15 (34) = happyShift action_12
action_15 (35) = happyShift action_13
action_15 (36) = happyShift action_32
action_15 (37) = happyShift action_14
action_15 (4) = happyGoto action_25
action_15 _ = happyReduce_22

action_16 (23) = happyShift action_49
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (8) = happyShift action_2
action_17 (9) = happyShift action_4
action_17 (10) = happyShift action_5
action_17 (11) = happyShift action_6
action_17 (13) = happyShift action_26
action_17 (14) = happyShift action_27
action_17 (15) = happyShift action_28
action_17 (16) = happyShift action_29
action_17 (17) = happyShift action_30
action_17 (18) = happyShift action_7
action_17 (21) = happyShift action_8
action_17 (22) = happyShift action_9
action_17 (27) = happyShift action_10
action_17 (31) = happyShift action_31
action_17 (32) = happyShift action_11
action_17 (34) = happyShift action_12
action_17 (35) = happyShift action_13
action_17 (36) = happyShift action_32
action_17 (37) = happyShift action_14
action_17 (4) = happyGoto action_25
action_17 _ = happyReduce_19

action_18 (33) = happyShift action_48
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (29) = happyShift action_47
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (29) = happyShift action_46
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (23) = happyShift action_45
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (8) = happyShift action_2
action_22 (9) = happyShift action_4
action_22 (10) = happyShift action_5
action_22 (11) = happyShift action_6
action_22 (13) = happyShift action_26
action_22 (14) = happyShift action_27
action_22 (15) = happyShift action_28
action_22 (16) = happyShift action_29
action_22 (17) = happyShift action_30
action_22 (18) = happyShift action_7
action_22 (19) = happyShift action_44
action_22 (21) = happyShift action_8
action_22 (22) = happyShift action_9
action_22 (27) = happyShift action_10
action_22 (31) = happyShift action_31
action_22 (32) = happyShift action_11
action_22 (34) = happyShift action_12
action_22 (35) = happyShift action_13
action_22 (36) = happyShift action_32
action_22 (37) = happyShift action_14
action_22 (4) = happyGoto action_25
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (8) = happyShift action_2
action_23 (9) = happyShift action_4
action_23 (10) = happyShift action_5
action_23 (11) = happyShift action_6
action_23 (12) = happyShift action_42
action_23 (13) = happyShift action_26
action_23 (14) = happyShift action_27
action_23 (15) = happyShift action_28
action_23 (16) = happyShift action_29
action_23 (17) = happyShift action_30
action_23 (18) = happyShift action_7
action_23 (21) = happyShift action_8
action_23 (22) = happyShift action_9
action_23 (27) = happyShift action_10
action_23 (30) = happyShift action_43
action_23 (31) = happyShift action_31
action_23 (32) = happyShift action_11
action_23 (34) = happyShift action_12
action_23 (35) = happyShift action_13
action_23 (36) = happyShift action_32
action_23 (37) = happyShift action_14
action_23 (4) = happyGoto action_25
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (12) = happyShift action_41
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (8) = happyShift action_2
action_25 (9) = happyShift action_4
action_25 (10) = happyShift action_5
action_25 (11) = happyShift action_6
action_25 (13) = happyShift action_26
action_25 (14) = happyShift action_27
action_25 (15) = happyShift action_28
action_25 (16) = happyShift action_29
action_25 (17) = happyShift action_30
action_25 (18) = happyShift action_7
action_25 (21) = happyShift action_8
action_25 (22) = happyShift action_9
action_25 (27) = happyShift action_10
action_25 (31) = happyShift action_31
action_25 (32) = happyShift action_11
action_25 (34) = happyShift action_12
action_25 (35) = happyShift action_13
action_25 (36) = happyShift action_32
action_25 (37) = happyShift action_14
action_25 (4) = happyGoto action_25
action_25 _ = happyReduce_13

action_26 (8) = happyShift action_2
action_26 (9) = happyShift action_4
action_26 (10) = happyShift action_5
action_26 (11) = happyShift action_6
action_26 (18) = happyShift action_7
action_26 (21) = happyShift action_8
action_26 (22) = happyShift action_9
action_26 (27) = happyShift action_10
action_26 (32) = happyShift action_11
action_26 (34) = happyShift action_12
action_26 (35) = happyShift action_13
action_26 (37) = happyShift action_14
action_26 (4) = happyGoto action_40
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (8) = happyShift action_2
action_27 (9) = happyShift action_4
action_27 (10) = happyShift action_5
action_27 (11) = happyShift action_6
action_27 (18) = happyShift action_7
action_27 (21) = happyShift action_8
action_27 (22) = happyShift action_9
action_27 (27) = happyShift action_10
action_27 (32) = happyShift action_11
action_27 (34) = happyShift action_12
action_27 (35) = happyShift action_13
action_27 (37) = happyShift action_14
action_27 (4) = happyGoto action_39
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (8) = happyShift action_2
action_28 (9) = happyShift action_4
action_28 (10) = happyShift action_5
action_28 (11) = happyShift action_6
action_28 (18) = happyShift action_7
action_28 (21) = happyShift action_8
action_28 (22) = happyShift action_9
action_28 (27) = happyShift action_10
action_28 (32) = happyShift action_11
action_28 (34) = happyShift action_12
action_28 (35) = happyShift action_13
action_28 (37) = happyShift action_14
action_28 (4) = happyGoto action_38
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (8) = happyShift action_2
action_29 (9) = happyShift action_4
action_29 (10) = happyShift action_5
action_29 (11) = happyShift action_6
action_29 (18) = happyShift action_7
action_29 (21) = happyShift action_8
action_29 (22) = happyShift action_9
action_29 (27) = happyShift action_10
action_29 (32) = happyShift action_11
action_29 (34) = happyShift action_12
action_29 (35) = happyShift action_13
action_29 (37) = happyShift action_14
action_29 (4) = happyGoto action_37
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (8) = happyShift action_2
action_30 (9) = happyShift action_4
action_30 (10) = happyShift action_5
action_30 (11) = happyShift action_6
action_30 (18) = happyShift action_7
action_30 (21) = happyShift action_8
action_30 (22) = happyShift action_9
action_30 (27) = happyShift action_10
action_30 (32) = happyShift action_11
action_30 (34) = happyShift action_12
action_30 (35) = happyShift action_13
action_30 (37) = happyShift action_14
action_30 (4) = happyGoto action_36
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (10) = happyShift action_34
action_31 (21) = happyShift action_35
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (8) = happyShift action_2
action_32 (9) = happyShift action_4
action_32 (10) = happyShift action_5
action_32 (11) = happyShift action_6
action_32 (18) = happyShift action_7
action_32 (21) = happyShift action_8
action_32 (22) = happyShift action_9
action_32 (27) = happyShift action_10
action_32 (32) = happyShift action_11
action_32 (34) = happyShift action_12
action_32 (35) = happyShift action_13
action_32 (37) = happyShift action_14
action_32 (4) = happyGoto action_33
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (8) = happyShift action_2
action_33 (9) = happyShift action_4
action_33 (10) = happyShift action_5
action_33 (11) = happyShift action_6
action_33 (13) = happyShift action_26
action_33 (14) = happyShift action_27
action_33 (15) = happyShift action_28
action_33 (16) = happyShift action_29
action_33 (17) = happyShift action_30
action_33 (18) = happyShift action_7
action_33 (21) = happyShift action_8
action_33 (22) = happyShift action_9
action_33 (27) = happyShift action_10
action_33 (31) = happyShift action_31
action_33 (32) = happyShift action_11
action_33 (34) = happyShift action_12
action_33 (35) = happyShift action_13
action_33 (36) = happyShift action_32
action_33 (37) = happyShift action_14
action_33 (4) = happyGoto action_25
action_33 _ = happyReduce_21

action_34 _ = happyReduce_16

action_35 _ = happyReduce_18

action_36 (8) = happyShift action_2
action_36 (9) = happyShift action_4
action_36 (10) = happyShift action_5
action_36 (11) = happyShift action_6
action_36 (13) = happyShift action_26
action_36 (14) = happyShift action_27
action_36 (15) = happyShift action_28
action_36 (16) = happyShift action_29
action_36 (17) = happyShift action_30
action_36 (18) = happyShift action_7
action_36 (21) = happyShift action_8
action_36 (22) = happyShift action_9
action_36 (27) = happyShift action_10
action_36 (31) = happyShift action_31
action_36 (32) = happyShift action_11
action_36 (34) = happyShift action_12
action_36 (35) = happyShift action_13
action_36 (36) = happyShift action_32
action_36 (37) = happyShift action_14
action_36 (4) = happyGoto action_25
action_36 _ = happyReduce_9

action_37 (8) = happyShift action_2
action_37 (9) = happyShift action_4
action_37 (10) = happyShift action_5
action_37 (11) = happyShift action_6
action_37 (13) = happyShift action_26
action_37 (14) = happyShift action_27
action_37 (15) = happyShift action_28
action_37 (16) = happyShift action_29
action_37 (17) = happyShift action_30
action_37 (18) = happyShift action_7
action_37 (21) = happyShift action_8
action_37 (22) = happyShift action_9
action_37 (27) = happyShift action_10
action_37 (31) = happyShift action_31
action_37 (32) = happyShift action_11
action_37 (34) = happyShift action_12
action_37 (35) = happyShift action_13
action_37 (36) = happyShift action_32
action_37 (37) = happyShift action_14
action_37 (4) = happyGoto action_25
action_37 _ = happyReduce_8

action_38 (8) = happyShift action_2
action_38 (9) = happyShift action_4
action_38 (10) = happyShift action_5
action_38 (11) = happyShift action_6
action_38 (14) = happyShift action_27
action_38 (16) = happyShift action_29
action_38 (17) = happyShift action_30
action_38 (21) = happyShift action_8
action_38 (22) = happyShift action_9
action_38 (27) = happyShift action_10
action_38 (31) = happyShift action_31
action_38 (32) = happyShift action_11
action_38 (34) = happyShift action_12
action_38 (35) = happyShift action_13
action_38 (36) = happyShift action_32
action_38 (37) = happyShift action_14
action_38 (4) = happyGoto action_25
action_38 _ = happyReduce_6

action_39 (8) = happyShift action_2
action_39 (9) = happyShift action_4
action_39 (10) = happyShift action_5
action_39 (11) = happyShift action_6
action_39 (16) = happyShift action_29
action_39 (17) = happyShift action_30
action_39 (21) = happyShift action_8
action_39 (22) = happyShift action_9
action_39 (27) = happyShift action_10
action_39 (31) = happyShift action_31
action_39 (32) = happyShift action_11
action_39 (34) = happyShift action_12
action_39 (35) = happyShift action_13
action_39 (36) = happyShift action_32
action_39 (37) = happyShift action_14
action_39 (4) = happyGoto action_25
action_39 _ = happyReduce_7

action_40 (8) = happyShift action_2
action_40 (9) = happyShift action_4
action_40 (10) = happyShift action_5
action_40 (11) = happyShift action_6
action_40 (14) = happyShift action_27
action_40 (16) = happyShift action_29
action_40 (17) = happyShift action_30
action_40 (21) = happyShift action_8
action_40 (22) = happyShift action_9
action_40 (27) = happyShift action_10
action_40 (31) = happyShift action_31
action_40 (32) = happyShift action_11
action_40 (34) = happyShift action_12
action_40 (35) = happyShift action_13
action_40 (36) = happyShift action_32
action_40 (37) = happyShift action_14
action_40 (4) = happyGoto action_25
action_40 _ = happyReduce_5

action_41 _ = happyReduce_15

action_42 _ = happyReduce_10

action_43 (8) = happyShift action_2
action_43 (9) = happyShift action_4
action_43 (10) = happyShift action_5
action_43 (11) = happyShift action_6
action_43 (18) = happyShift action_7
action_43 (21) = happyShift action_8
action_43 (22) = happyShift action_9
action_43 (27) = happyShift action_10
action_43 (32) = happyShift action_11
action_43 (34) = happyShift action_12
action_43 (35) = happyShift action_13
action_43 (37) = happyShift action_14
action_43 (4) = happyGoto action_58
action_43 (5) = happyGoto action_59
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (8) = happyShift action_2
action_44 (9) = happyShift action_4
action_44 (10) = happyShift action_5
action_44 (11) = happyShift action_6
action_44 (18) = happyShift action_7
action_44 (21) = happyShift action_8
action_44 (22) = happyShift action_9
action_44 (27) = happyShift action_10
action_44 (32) = happyShift action_11
action_44 (34) = happyShift action_12
action_44 (35) = happyShift action_13
action_44 (37) = happyShift action_14
action_44 (4) = happyGoto action_57
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (11) = happyShift action_51
action_45 (25) = happyShift action_52
action_45 (26) = happyShift action_53
action_45 (7) = happyGoto action_56
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (8) = happyShift action_2
action_46 (9) = happyShift action_4
action_46 (10) = happyShift action_5
action_46 (11) = happyShift action_6
action_46 (18) = happyShift action_7
action_46 (21) = happyShift action_8
action_46 (22) = happyShift action_9
action_46 (27) = happyShift action_10
action_46 (32) = happyShift action_11
action_46 (34) = happyShift action_12
action_46 (35) = happyShift action_13
action_46 (37) = happyShift action_14
action_46 (4) = happyGoto action_55
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (8) = happyShift action_2
action_47 (9) = happyShift action_4
action_47 (10) = happyShift action_5
action_47 (11) = happyShift action_6
action_47 (18) = happyShift action_7
action_47 (21) = happyShift action_8
action_47 (22) = happyShift action_9
action_47 (27) = happyShift action_10
action_47 (32) = happyShift action_11
action_47 (34) = happyShift action_12
action_47 (35) = happyShift action_13
action_47 (37) = happyShift action_14
action_47 (4) = happyGoto action_54
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_17

action_49 (11) = happyShift action_51
action_49 (25) = happyShift action_52
action_49 (26) = happyShift action_53
action_49 (7) = happyGoto action_50
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (29) = happyShift action_65
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (11) = happyShift action_51
action_51 (25) = happyShift action_52
action_51 (26) = happyShift action_53
action_51 (7) = happyGoto action_64
action_51 _ = happyFail (happyExpListPerState 51)

action_52 _ = happyReduce_27

action_53 _ = happyReduce_28

action_54 (8) = happyShift action_2
action_54 (9) = happyShift action_4
action_54 (10) = happyShift action_5
action_54 (11) = happyShift action_6
action_54 (13) = happyShift action_26
action_54 (14) = happyShift action_27
action_54 (15) = happyShift action_28
action_54 (16) = happyShift action_29
action_54 (17) = happyShift action_30
action_54 (18) = happyShift action_7
action_54 (21) = happyShift action_8
action_54 (22) = happyShift action_9
action_54 (27) = happyShift action_10
action_54 (30) = happyShift action_63
action_54 (31) = happyShift action_31
action_54 (32) = happyShift action_11
action_54 (34) = happyShift action_12
action_54 (35) = happyShift action_13
action_54 (36) = happyShift action_32
action_54 (37) = happyShift action_14
action_54 (4) = happyGoto action_25
action_54 _ = happyReduce_25

action_55 (8) = happyShift action_2
action_55 (9) = happyShift action_4
action_55 (10) = happyShift action_5
action_55 (11) = happyShift action_6
action_55 (13) = happyShift action_26
action_55 (14) = happyShift action_27
action_55 (15) = happyShift action_28
action_55 (16) = happyShift action_29
action_55 (17) = happyShift action_30
action_55 (18) = happyShift action_7
action_55 (21) = happyShift action_8
action_55 (22) = happyShift action_9
action_55 (27) = happyShift action_10
action_55 (28) = happyShift action_62
action_55 (31) = happyShift action_31
action_55 (32) = happyShift action_11
action_55 (34) = happyShift action_12
action_55 (35) = happyShift action_13
action_55 (36) = happyShift action_32
action_55 (37) = happyShift action_14
action_55 (4) = happyGoto action_25
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (24) = happyShift action_61
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (8) = happyShift action_2
action_57 (9) = happyShift action_4
action_57 (10) = happyShift action_5
action_57 (11) = happyShift action_6
action_57 (13) = happyShift action_26
action_57 (14) = happyShift action_27
action_57 (15) = happyShift action_28
action_57 (16) = happyShift action_29
action_57 (17) = happyShift action_30
action_57 (18) = happyShift action_7
action_57 (20) = happyShift action_60
action_57 (21) = happyShift action_8
action_57 (22) = happyShift action_9
action_57 (27) = happyShift action_10
action_57 (31) = happyShift action_31
action_57 (32) = happyShift action_11
action_57 (34) = happyShift action_12
action_57 (35) = happyShift action_13
action_57 (36) = happyShift action_32
action_57 (37) = happyShift action_14
action_57 (4) = happyGoto action_25
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (8) = happyShift action_2
action_58 (9) = happyShift action_4
action_58 (10) = happyShift action_5
action_58 (11) = happyShift action_6
action_58 (13) = happyShift action_26
action_58 (14) = happyShift action_27
action_58 (15) = happyShift action_28
action_58 (16) = happyShift action_29
action_58 (17) = happyShift action_30
action_58 (18) = happyShift action_7
action_58 (21) = happyShift action_8
action_58 (22) = happyShift action_9
action_58 (27) = happyShift action_10
action_58 (30) = happyShift action_43
action_58 (31) = happyShift action_31
action_58 (32) = happyShift action_11
action_58 (34) = happyShift action_12
action_58 (35) = happyShift action_13
action_58 (36) = happyShift action_32
action_58 (37) = happyShift action_14
action_58 (4) = happyGoto action_25
action_58 _ = happyReduce_23

action_59 _ = happyReduce_24

action_60 (8) = happyShift action_2
action_60 (9) = happyShift action_4
action_60 (10) = happyShift action_5
action_60 (11) = happyShift action_6
action_60 (18) = happyShift action_7
action_60 (21) = happyShift action_8
action_60 (22) = happyShift action_9
action_60 (27) = happyShift action_10
action_60 (32) = happyShift action_11
action_60 (34) = happyShift action_12
action_60 (35) = happyShift action_13
action_60 (37) = happyShift action_14
action_60 (4) = happyGoto action_71
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (8) = happyShift action_2
action_61 (9) = happyShift action_4
action_61 (10) = happyShift action_5
action_61 (11) = happyShift action_6
action_61 (18) = happyShift action_7
action_61 (21) = happyShift action_8
action_61 (22) = happyShift action_9
action_61 (27) = happyShift action_10
action_61 (32) = happyShift action_11
action_61 (34) = happyShift action_12
action_61 (35) = happyShift action_13
action_61 (37) = happyShift action_14
action_61 (4) = happyGoto action_70
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (8) = happyShift action_2
action_62 (9) = happyShift action_4
action_62 (10) = happyShift action_5
action_62 (11) = happyShift action_6
action_62 (18) = happyShift action_7
action_62 (21) = happyShift action_8
action_62 (22) = happyShift action_9
action_62 (27) = happyShift action_10
action_62 (32) = happyShift action_11
action_62 (34) = happyShift action_12
action_62 (35) = happyShift action_13
action_62 (37) = happyShift action_14
action_62 (4) = happyGoto action_69
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (21) = happyShift action_19
action_63 (6) = happyGoto action_68
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (24) = happyShift action_67
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (8) = happyShift action_2
action_65 (9) = happyShift action_4
action_65 (10) = happyShift action_5
action_65 (11) = happyShift action_6
action_65 (18) = happyShift action_7
action_65 (21) = happyShift action_8
action_65 (22) = happyShift action_9
action_65 (27) = happyShift action_10
action_65 (32) = happyShift action_11
action_65 (34) = happyShift action_12
action_65 (35) = happyShift action_13
action_65 (37) = happyShift action_14
action_65 (4) = happyGoto action_66
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (8) = happyShift action_2
action_66 (9) = happyShift action_4
action_66 (10) = happyShift action_5
action_66 (11) = happyShift action_6
action_66 (13) = happyShift action_26
action_66 (14) = happyShift action_27
action_66 (15) = happyShift action_28
action_66 (16) = happyShift action_29
action_66 (17) = happyShift action_30
action_66 (18) = happyShift action_7
action_66 (21) = happyShift action_8
action_66 (22) = happyShift action_9
action_66 (27) = happyShift action_10
action_66 (28) = happyShift action_73
action_66 (31) = happyShift action_31
action_66 (32) = happyShift action_11
action_66 (34) = happyShift action_12
action_66 (35) = happyShift action_13
action_66 (36) = happyShift action_32
action_66 (37) = happyShift action_14
action_66 (4) = happyGoto action_25
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (11) = happyShift action_51
action_67 (25) = happyShift action_52
action_67 (26) = happyShift action_53
action_67 (7) = happyGoto action_72
action_67 _ = happyFail (happyExpListPerState 67)

action_68 _ = happyReduce_26

action_69 (8) = happyShift action_2
action_69 (9) = happyShift action_4
action_69 (10) = happyShift action_5
action_69 (11) = happyShift action_6
action_69 (13) = happyShift action_26
action_69 (14) = happyShift action_27
action_69 (15) = happyShift action_28
action_69 (16) = happyShift action_29
action_69 (17) = happyShift action_30
action_69 (18) = happyShift action_7
action_69 (21) = happyShift action_8
action_69 (22) = happyShift action_9
action_69 (27) = happyShift action_10
action_69 (31) = happyShift action_31
action_69 (32) = happyShift action_11
action_69 (34) = happyShift action_12
action_69 (35) = happyShift action_13
action_69 (36) = happyShift action_32
action_69 (37) = happyShift action_14
action_69 (4) = happyGoto action_25
action_69 _ = happyReduce_14

action_70 (8) = happyShift action_2
action_70 (9) = happyShift action_4
action_70 (10) = happyShift action_5
action_70 (11) = happyShift action_6
action_70 (13) = happyShift action_26
action_70 (14) = happyShift action_27
action_70 (15) = happyShift action_28
action_70 (16) = happyShift action_29
action_70 (17) = happyShift action_30
action_70 (18) = happyShift action_7
action_70 (21) = happyShift action_8
action_70 (22) = happyShift action_9
action_70 (27) = happyShift action_10
action_70 (31) = happyShift action_31
action_70 (32) = happyShift action_11
action_70 (34) = happyShift action_12
action_70 (35) = happyShift action_13
action_70 (36) = happyShift action_32
action_70 (37) = happyShift action_14
action_70 (4) = happyGoto action_25
action_70 _ = happyReduce_12

action_71 (8) = happyShift action_2
action_71 (9) = happyShift action_4
action_71 (10) = happyShift action_5
action_71 (11) = happyShift action_6
action_71 (13) = happyShift action_26
action_71 (14) = happyShift action_27
action_71 (15) = happyShift action_28
action_71 (16) = happyShift action_29
action_71 (17) = happyShift action_30
action_71 (18) = happyFail []
action_71 (21) = happyShift action_8
action_71 (22) = happyShift action_9
action_71 (27) = happyShift action_10
action_71 (31) = happyShift action_31
action_71 (32) = happyShift action_11
action_71 (34) = happyShift action_12
action_71 (35) = happyShift action_13
action_71 (36) = happyShift action_32
action_71 (37) = happyShift action_14
action_71 (4) = happyGoto action_25
action_71 _ = happyReduce_11

action_72 (12) = happyShift action_75
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (8) = happyShift action_2
action_73 (9) = happyShift action_4
action_73 (10) = happyShift action_5
action_73 (11) = happyShift action_6
action_73 (18) = happyShift action_7
action_73 (21) = happyShift action_8
action_73 (22) = happyShift action_9
action_73 (27) = happyShift action_10
action_73 (32) = happyShift action_11
action_73 (34) = happyShift action_12
action_73 (35) = happyShift action_13
action_73 (37) = happyShift action_14
action_73 (4) = happyGoto action_74
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (8) = happyShift action_2
action_74 (9) = happyShift action_4
action_74 (10) = happyShift action_5
action_74 (11) = happyShift action_6
action_74 (13) = happyShift action_26
action_74 (14) = happyShift action_27
action_74 (15) = happyShift action_28
action_74 (16) = happyShift action_29
action_74 (17) = happyShift action_30
action_74 (18) = happyShift action_7
action_74 (21) = happyShift action_8
action_74 (22) = happyShift action_9
action_74 (27) = happyShift action_10
action_74 (31) = happyShift action_31
action_74 (32) = happyShift action_11
action_74 (34) = happyShift action_12
action_74 (35) = happyShift action_13
action_74 (36) = happyShift action_32
action_74 (37) = happyShift action_14
action_74 (4) = happyGoto action_25
action_74 _ = happyReduce_20

action_75 _ = happyReduce_29

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 _
	 =  HappyAbsSyn4
		 (BTrue
	)

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 _
	 =  HappyAbsSyn4
		 (BFalse
	)

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn4
		 (Num happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn4
		 (Var happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  4 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Plus happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  4 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Minus happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  4 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Times happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  4 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (And happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  4 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Or happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  4 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Paren happy_var_2
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happyReduce 6 4 happyReduction_11
happyReduction_11 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 6 4 happyReduction_12
happyReduction_12 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Lam happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_2  4 happyReduction_13
happyReduction_13 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (App happy_var_1 happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happyReduce 6 4 happyReduction_14
happyReduction_14 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Let happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_3  4 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Tuple happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  4 happyReduction_16
happyReduction_16 (HappyTerminal (TokenNum happy_var_3))
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (TupleProj happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  4 happyReduction_17
happyReduction_17 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Record happy_var_2
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  4 happyReduction_18
happyReduction_18 (HappyTerminal (TokenVar happy_var_3))
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (RecordProj happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  4 happyReduction_19
happyReduction_19 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Fix happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 8 4 happyReduction_20
happyReduction_20 ((HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Let happy_var_2 (Fix (Lam happy_var_2 happy_var_4 happy_var_6)) happy_var_8
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_3  4 happyReduction_21
happyReduction_21 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Eq happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  4 happyReduction_22
happyReduction_22 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Not happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  5 happyReduction_23
happyReduction_23 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  5 happyReduction_24
happyReduction_24 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  6 happyReduction_25
happyReduction_25 (HappyAbsSyn4  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn6
		 ([(happy_var_1,happy_var_3)]
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happyReduce 5 6 happyReduction_26
happyReduction_26 ((HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 ((happy_var_1, happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_1  7 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn7
		 (TBool
	)

happyReduce_28 = happySpecReduce_1  7 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn7
		 (TNum
	)

happyReduce_29 = happyReduce 5 7 happyReduction_29
happyReduction_29 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (TFun happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 38 38 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenTrue -> cont 8;
	TokenFalse -> cont 9;
	TokenNum happy_dollar_dollar -> cont 10;
	TokenLParen -> cont 11;
	TokenRParen -> cont 12;
	TokenPlus -> cont 13;
	TokenTimes -> cont 14;
	TokenMinus -> cont 15;
	TokenAnd -> cont 16;
	TokenOr -> cont 17;
	TokenIf -> cont 18;
	TokenThen -> cont 19;
	TokenElse -> cont 20;
	TokenVar happy_dollar_dollar -> cont 21;
	TokenLam -> cont 22;
	TokenColon -> cont 23;
	TokenArrow -> cont 24;
	TokenBoolean -> cont 25;
	TokenNumber -> cont 26;
	TokenLet -> cont 27;
	TokenIn -> cont 28;
	TokenAssign -> cont 29;
	TokenComma -> cont 30;
	TokenDot -> cont 31;
	TokenLBrack -> cont 32;
	TokenRBrack -> cont 33;
	TokenFix -> cont 34;
	TokenLetRec -> cont 35;
	TokenEq -> cont 36;
	TokenNot -> cont 37;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 38 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError ts = error "Syntax error: sequência de instruções inválida!"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}







# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}















{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8336_0/ghc_2.h" #-}
































































































































































































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
