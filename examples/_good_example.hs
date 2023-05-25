(
    (\ iivacatneo : TBool  ->   
        (
            (\ oarni : TNum  ->   
                ( 
                    (\ ibacternero : TNum  ->   false  &&  false )  
                ) 
            ) 
        ) . 
        ( 
            (
                (\ oiatrno : TNum  ->   ( "oiatrno" ) ) 
            ) . 
            (2)  * 10
        ) 
    ) 
) 
. 
( let "iboivacatneio" =  (  true  )  in "iboivacatneio") 


-- ((\oiatrno : TNum  ->   ( oiatrno ) )) . (2)  * 10
-- ((\oiatrno  ->   ( oiatrno ) ))((2)  * 10)                     <- Isso aqui roda em haskell, é um programa válido...




((\iivacatneo -> ((\oarni -> ((\ibacternero -> False && False))))(((\ oiatrno -> (oiatrno)))(2)  * 10)))( let iboivacatneio = (True) in iboivacatneio)











