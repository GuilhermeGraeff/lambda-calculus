main :: IO ()
main = print ( let x0 =  if  False  then  True  else  True  in  ((\x0 ->  x0) )(9)  +  if  False  then 6 else 7 * 9 - 1)