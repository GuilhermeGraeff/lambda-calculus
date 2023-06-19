main :: IO ()
main = print ( let x0 = 1 in 7 -  ((\x0 ->  4) )(4)  +  let x0 =  False  in 1 - 5 + 8)