hay0::[Int]-> Bool
hay0 []=False
hay0 (x:xs) | x==0 = True
            | x/=0 = hay0 xs

suma::[Int]-> Int
suma []=0
suma (x:xs) = x + suma xs

algunOrden::(Int,Int,Int)->Bool
algunOrden (a,b,c) = a>=b && b>=c || a<=b && b<=c

absoluto :: [Int] -> [Int] 
absoluto (x:xs) | x >= 0 = x:xs
                | x < 0 = -x : xs

superNota:: Int -> [Int] -> Int
superNota n (x:xs) = x:superNota n xs
