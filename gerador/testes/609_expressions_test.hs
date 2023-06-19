main :: IO ()
main = print ( True  &&  let x0 = 5 in  False  ||  True  &&  True )