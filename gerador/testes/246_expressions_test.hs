main :: IO ()
main = print ( if  let x0 = 5 - 1 in  if  False  then  False  else  True  then  (  let x0 =  True  in  ((\x1 ->   (  (  False  )  :  ( x1 )  :  ( x1 )  :  (  True  )  :  ( x1 )  :  (  True  )  :  (  True  )  :  ( x1 )  :  ( x1 )  :  ( x1 )  :  []  ) ) )(x0)  )  else  ((\x0 ->   ((\x1 ->   (  ( x1 )  :  ( x1 )  :  ( x1 )  :  ( x1 )  :  ( x1 )  :  (  False  )  :  (  False  )  :  (  True  )  :  ( x1 )  :  ( x1 )  :  ( x1 )  :  []  ) ) )( True ) ) )( ( 5 ) ) )