main :: IO ()
main = print ( if  ( head ( (  (  True  ||  True  )  :  (  True  )  :  (  False  )  :  (  True  )  :  []  ) ) )  then  ( head ( (  (  True  &&  False  )  :  []  ) ) )  else  False )