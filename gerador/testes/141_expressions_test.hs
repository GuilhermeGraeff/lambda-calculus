main :: IO ()
main = print ( if  True  then  False  &&  True  &&  True  else  ( head ( (  (  True  )  :  (  False  )  :  []  ) ) )  &&  False  &&  True )