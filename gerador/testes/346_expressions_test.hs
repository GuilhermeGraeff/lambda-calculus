main :: IO ()
main = print ( let x0 =  if  True  then 2 else 9 * 10 in  ((\x1 ->  x0) )( False )  -  let x1 =  False  in 5)