main :: IO ()
main = print ( let x0 = 6 in  False  ||  if  True  then  False  else  True  &&  let x0 =  True  in x0 &&  ((\x0 ->  x0) )( False ) )