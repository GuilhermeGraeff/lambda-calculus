main :: IO ()
main = print ( let x0 =  False  in x0 ||  ((\x0 ->   False ) )(10)  ||  (  False  ) )