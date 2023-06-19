main :: IO ()
main = print ( (  let x0 = 8 in  True  )  &&  ((\x0 ->   False ) )(1 + 6) )