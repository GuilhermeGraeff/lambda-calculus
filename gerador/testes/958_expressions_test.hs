main :: IO ()
main = print ( if  ( head ( (  (  if  False  then  True  else  False  )  :  []  ) ) )  then  (  ((\x0 ->   ((\x1 ->  x0) )( ((\x1 ->  2) )( (  (  False  )  :  (  False  )  :  (  True  )  :  (  True  )  :  (  True  )  :  []  ) ) ) ) )(3)  )  else  let x0 =  if  False  then  ((\x0 ->  3) )( True )  else  ((\x0 ->  3) )( False )  in  let x1 =  True  in  ((\x2 ->  5) )( ((\x2 ->  5) )( (  ( x1 )  :  (  True  )  :  ( x1 )  :  (  False  )  :  ( x1 )  :  (  False  )  :  ( x1 )  :  ( x1 )  :  ( x1 )  :  (  True  )  :  (  True  )  :  (  False  )  :  (  True  )  :  ( x1 )  :  []  ) ) ) )