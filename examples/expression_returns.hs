For the expression(4 vezes depth = (1 and 2), para ctx = [] e tipo = TBool):

generator:: Int -> Gen Expr
██████████ 
(  false  ) 
if  false  then 3 else 6
(  true  ) 

if  false  then 5 else 4
let "iivr" =  ██████████  in (\ oivco : TNum  ->  5) 
(\ iiacaerir : TTuple [TBool,TBool,TBool,TBool,TBool,TNum,TNum,TNum,TNum]  ->   ( 5 ) ) 
██████████ 


genExpr:: Int -> Ctx -> Ty -> Gen Expr
(\ bictenio : TNum  ->   true ) 
((\ iaateo : TBool  ->  "iaateo") ) . ( true ) 
(  false  ) 
(\ ivteneo : TNum  ->   false ) 

((\ oivcan : TBool  ->   false  &&  false ) ) . ( true ) 
(\ oibovacterni : TNum  ->   false ) 
(  false  ||  false  ) 
if  true  then  false  else  false  || (\ oiboivacernero : TBool  ->   true ) 


genBranch :: Int -> Ctx -> Ty -> Gen Expr
true  ||  true 
false  &&  false 
if  true  then  false  else  false 
let "oboiacatrero" = 5 in  false 

if  false  then  false  else  false  &&  (  false  ) 
(\ oivacarnro : TBool  ->   if  false  then  false  else  true ) 
let "acatrnei" =  if  true  then  false  else  false  in "acatrnei"
false  &&  true


genLeaf :: Ctx -> Ty -> Gen Expr
true 
false 
true 
true 


genUnknown :: Gen Expr
██████████ 
██████████ 
██████████ 
██████████ 


genBoolean :: Gen Expr
true 
true 
false 
false 


genNum :: Gen Expr
3
7
3
4


genPlus :: Int -> Ctx -> Gen Expr
5 + 3
9 + 4
5 + 9
6 + 7

2 + 6 + (\ iovcei : TBool  ->  9) 
8 + 6 + 4 + 6
10 - 6 +  if  false  then 9 else 5
2 - 2 + 8 - 7


genMinus :: Int -> Ctx -> Gen Expr
4 - 5
9 - 9
3 - 2
3 - 4

let "oicaterei" = 1 in 9 -  ( 4 ) 
if  true  then 8 else 5 -  ((\ oboivacter : TBool  ->  (\ biacaerneiro : TBool  ->  1) ) ) . ( false ) 
if  false  then 6 else 10 -  ((\ ioiacaerir : TBool  ->   ((\ oibivaaerner : TNum  ->  "oibivaaerner") ) . (9) ) ) . ( true ) 
10 + 6 - 4 - 4


genTimes :: Int -> Ctx -> Gen Expr
4 * 10
3 * 1
1 * 10
10 * 6

(\ obiaeero : TNum  ->  8)  *  if  true  then 5 else 7
1 - 5 * 2 - 9
((\ ovatereiro : TNum  ->   let "ovaatrno" =  false  in 3) ) . (3)  * 4 - 8
let "oiicerne" =  false  in 3 * (\ oboiactrer : TNum  ->  8) 

genLet :: Int -> Ctx -> Ty -> Gen Expr
let "oiiacat" = 8 in  false 
let "oaro" = 8 in  true 
let "obvcatreo" = 5 in  true 
let "ivtio" =  false  in  true

let "ibacatrei" =  if  false  then (\ ernio : TNum  ->   ((\ ioicari : TBool  ->   ( 6 ) ) ) . ( false ) )  else (\ ioivci : TNum  ->  9 * 4)  in  let "boino" = 4 in  true 
let "oiovcn" =  (  true  )  in "oiovcn"
let "oicaernio" =  if  true  then  false  else  false  in "oicaernio"
let "oioiateo" =  ((\ boivaatereir : TNum  ->   true  &&  false ) ) . (8)  in "oioiateo"


genLam :: Int -> Ctx -> Ty -> Gen Expr 
(\ oibivcaere : TBool  ->   false ) 
(\ obivctno : TBool  ->   true ) 
(\ oiivatene : TNum  ->   true ) 
(\ iaaenio : TNum  ->   false ) 

(\ oovcei : TFun TNum TNum  ->   true  ||  true ) 
(\ oioicnir : List TNum  ->   true  &&  true ) 
(\ iboiacaere : TBool  ->   false  &&  false ) 
(\ ivatenero : TBool  ->  "ivatenero") 


genApp :: Int -> Ctx -> Ty -> Gen Expr
((\ iovacenei : TBool  ->  "iovacenei") ) . ( false ) 
((\ oiacte : TBool  ->  "oiacte") ) . ( true ) 
((\ oibvacaener : TNum  ->   true  &&  false ) ) . (8) 
((\ obacernro : TNum  ->   (  false  ) ) ) . (8) 

( let "oiboiacteiro" =  true  in (\ oboivctenr : TNum  ->   false  ||  false ) ) . (6 * 9) 
( let "obivatrro" =  true  in (\ oivaaro : TBool  ->   if  true  then  true  else  false ) ) . ( (  false  ) ) 
((\ boivro : TBool  ->  (\ oioivaarniro : TNum  ->  "boivro") ) ) . (9 + 6) --qq rolou aqui?
((\ oiaatrero : TNum  ->  (\ oiovatrero : TNum  ->   true  &&  true ) ) ) . (8 - 2) 


genAnd :: Int -> Ctx -> Gen Expr
true  &&  false 
true  &&  true 
true  &&  false 
false  &&  false 

true  ||  false  && (\ iiterio : TNum  ->   true ) 
((\ ooiatenio : TNum  ->  (\ bvacernr : TBool  ->   false ) ) ) . (3)  &&  ((\ ai : TNum  ->   let "oicernio" = 4 in  true ) ) . (5) 
true  ||  false  && (\ oibovactneo : TBool  ->   true ) 
if  false  then  false  else  true  &&  false  ||  false 


genOr :: Int -> Ctx -> Gen Expr
false  ||  true 
true  ||  false 
true  ||  false 
false  ||  false 

false  ||  true  ||  (  false  ) 
false  &&  false  ||  ((\ oivareio : TBool  ->  "oivareio") ) . ( true ) 
if  false  then  false  else  false  ||  false  ||  true 
(  true  )  ||  ((\ oivcaeni : TNum  ->   true  ||  false ) ) . (4) 


genParen :: Int -> Ctx -> Ty -> Gen Expr
(  false  ) 
(  false  ) 
(  false  ) 
(  false  )

(  if  true  then  false  else  false  ) 
(  let "iivacarnero" = 3 in  true  ) 
(  let "iocater" = 8 in  false  ) 
(  ((\ oibivacareir : TNum  ->   if  false  then  false  else  false ) ) . (4)  ) 


genIf :: Int -> Ctx -> Ty -> Gen Expr 
if  true  then  false  else  false 
if  false  then  true  else  false 
if  false  then  false  else  true 
if  false  then  false  else  true 

if  ((\ oiterneio : TNum  ->   (  false  ) ) ) . (2)  then (\ oiboivatreiro : TBool  ->   false )  else  if  true  then  true  else  false 
if  if  false  then  true  else  true  then  let "vcener" = 9 in  true  else  if  false  then  true  else  false 
if  if  true  then  true  else  true  then  let "ivcenro" =  false  in  false  else  if  true  then  true  else  true 
if  let "oibivacarei" = 2 in  true  then  let "oiarno" = 3 in  false  else (\ oatir : TNum  ->   false ) 


genVar :: Int -> Ctx -> Ty -> Gen Expr -- Para ctx = []
let "ioicar" =  false  in  true 
false  ||  false 
let "oiair" =  true  in  false 
(  false  ) 

(\ obocatriro : TBool  ->   true ) 
(\ ooaair : TNum  ->   false ) 
((\ ioivactnir : TBool  ->  "ioivactnir") ) . ( false ) 
false  ||  false 
genVar' :: Int -> Ctx -> Ty -> Gen Expr -- Para ctx = [("variavel", TBool)]
"variavel"
"variavel"
"variavel"
"variavel"

"variavel"
"variavel"
"variavel"
"variavel"

