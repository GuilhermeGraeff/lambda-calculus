 if  false  .  true  then  let "bool_let" =  true  ||  false  in  true  .  let "bool_let" =  true  in  false  &&  if  true  then  true  ==  true  else  let "bool_let" =  if  false  then  false  else  true  in  let "bool_let" =  true  in  true  else "bool_var" ||  false  ||  if  false  then  true  else  false  .  true  &&  false  ==  let "bool_let" =  true  in  true  == "bool_var" ||  if  true  then  (  true  &&  true  .  true  . "bool_var" .  (  true  )  )  else  true  ||  true  .  true  .  true  .  if  false  then  true  ||  false  ==  true  ||  true  .  true  ||  true  else  true  ||  false  ==  false  &&  (  let "bool_let" =  true  in  true  &&  true  ==  true  )  ||  false  .  let "bool_let" =  (  true  )  in  (  (  false  )  . "bool_var" )  ==  false  &&  let "bool_let" =  true  in  false  ||  false  .  false  ==  false  ||  false  &&  false  .  true  == "bool_var"


TFun (TFun TNum TBool) TBool
killall -9 ghc
 -- Type definitions

-- data Ty = TBool
--         | TNum
--         | TFun Ty Ty
--         | TTuple [Ty]
--         | TRecord [(String,Ty)]
--         | List Ty 
--         deriving (Show, Eq)

-- -- Expression definitions

-- data Expr = BTrue
--           | BFalse
--           | Num Int
--           | Paren Expr
--           | Plus Expr     
--           | Times Expr Expr
--           | Minus Expr Expr
--           | And Expr Expr
--           | Or Expr Expr 
--           | If Expr Expr Expr
--           | Var String
--           | Lam String Ty Expr
--           | App Expr Expr 
--           | Let String Expr Expr 
--           | Tuple [Expr]
--           | TupleProj Expr Int 
--           | Record [(String,Expr)] 
--           | RecordProj Expr String 
--           | Fix Expr 
--           | Eq Expr Expr
--           | Not Expr
--           | Nil Ty
--           | Cons Ty Expr Expr
--           | IsNil Ty Expr 
--           | Head Ty Expr 
--           | Tail Ty Expr
--           deriving Eq

-- Os que podem retornar um TBool
--           | BTrue
--           | BFalse
--           | Paren Expr
--           | And Expr Expr
--           | Or Expr Expr
--           | If Expr Expr Expr
--           | Var String
--           | App Expr Expr 
--           | Let String Expr Expr
--           | RecordProj Expr String
--           | TupleProj Expr Int 
--           | IsNil Ty Expr 
--           | Head Ty Expr 
--           | Tail Ty Expr
--           | Not Expr
--           | Fix Expr 
--           | Eq Expr Expr

-- Os que podem retornar o TNum
--           | Num Int
--           | Paren Expr
--           | Plus Expr     
--           | Times Expr Expr
--           | Minus Expr Expr
--           | If Expr Expr Expr
--           | Var String
--           | App Expr Expr 
--           | Let String Expr Expr 
--           | RecordProj Expr String
--           | TupleProj Expr Int 
--           | Head Ty Expr 
--           | Tail Ty Expr

-- As comuns entre o TBool e o TNum
--           | Paren Expr
--           | If Expr Expr Expr
--           | Var String
--           | App Expr Expr 
--           | Let String Expr Expr
--           | RecordProj Expr String
--           | TupleProj Expr Int 
--           | Head Ty Expr 
--           | Tail Ty Expr

if  false  .  true  
then  let "oaaterne" = 6 in 3 
else  false  . 4 .  false  &&  \ ioiaterner : TBool  -> if 7 then 9 else  false  . 10 . 2

myos>cabal update
myos>cabal install --lib QuickCheck
myos>ghci
gchi> import Test.QuickCheck

 let "ioariro" =  true in 
    8 
    -  
    ( 
        ( 
            \ oibivni : TNum  ->  (  \ oivaaereir : TFun TBool TBool  ->  9 - 3 )
        ) 
        . 
        (
            9
        ) 
    ) 
    . 
    ( 
        \ ooiaatrnei : TNum  ->   \ iovacatereir : TBool  ->  "iovacatereir"
    ) 

letrec iseven : (Number -> Bool) =
\ x: Number -> 
if x == 0 then true
else if (x - 1) == 0 then false
else iseven (x - 2)
in
iseven 2

(\ x: Number -> 
if x == 0 then true
else if (x - 1) == 0 then false
else true) 2