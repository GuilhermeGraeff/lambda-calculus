# An implementation of the lambda-calculus in Haskell.


Meu data type Ty após gerado pelo Type Generator retorna: 
TTuple  
    [   
        TFun 
            (TTuple  [TNum,TNum,TBool,TNum,TBool,TBool,TBool,TNum]) 
            (TFun TBool TBool),
        List (TTuple  [TNum,TNum,TNum,TNum])
    ]
    
Meu Expr data type depois de gerado pelo Expr generator (por enquanto com muitos erros): 
if  false  .  true  
then  let "oaaterne" = 6 in 3 
else  false  . 4 .  false  &&  \ ioiaterner : TBool  -> if 7 then 9 else  false  . 10 . 2
