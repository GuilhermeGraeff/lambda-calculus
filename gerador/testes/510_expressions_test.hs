main :: IO ()
main = print ( let x0 = 10 in  ((\x0 ->   True ) )(5)  &&  ((\x0 ->   True ) )(10)  ||  ( head ( (  (  False  )  :  (  True  )  :  []  ) ) ) )