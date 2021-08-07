-- ejercicio 1

-- a

haycero :: Int -> Bool
haycero x | x==0 =True
          | otherwise =False
-- b 

espositivo :: Int -> Bool
espositivo x | x >= 0 = True
             | otherwise = False
-- c

esVocal :: Char -> Bool
esVocal x | x=='a'||x=='e'||x=='i'||x=='o'||x=='u'=True		
          | otherwise = False
-- d

factorial :: Int -> Int
factorial n = product [1..n]

-- e

promedio :: [Int] -> Int
promedio [] = 0
promedio (x:xs) = div (sumatoria xs) (length xs)

-- ejercicio 2

-- a

paratodo :: [Bool] -> Bool
paratodo [] = True 
paratodo (x:xs) = x == True && paratodo xs 

--b

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

--c

productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

-- ejercicio 3

pertenece :: Int -> [Int] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n==x || pertenece n xs

-- ejercicio 4

encuentra :: Int -> [(Int, String)] -> String
encuentra n [] = ""
encuentra n ((a,b):xs) | n==a = b 
                       | otherwise = encuentra n xs

-- ejercicio 5

-- a

paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] t = True
paratodo' (x:xs) t = t x && paratodo' xs t

-- b

existe' :: [a] -> (a -> Bool) -> Bool
existe' [] t = False
existe' (x:xs) t = t x || existe' xs t

-- c

sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] t = 0
sumatoria' (x:xs) t = t x + sumatoria' xs t

-- d

productoria' :: [a] -> (a -> Int) -> Int
productoria' [] t = 1
productoria' (x:xs) t = t x *productoria' xs t

-- ejercicio 6

paratodo'' :: [Bool] -> Bool
paratodo'' xs = paratodo' xs b
                    where b :: Bool -> Bool
                          b x = x 

-- ejercicio 7

-- a

todospares :: [Int] -> Bool
todospares xs = paratodo' xs even
               
-- b

haymultiplo :: Int -> [Int] -> Bool
haymultiplo n xs = existe' xs em
                where em :: Int -> Bool
                      em x = mod x n == 0 

-- c

sumacuadrados :: Int -> Int
sumacuadrados n = sumatoria' [0..n] cuadrado 
                   where cuadrado :: Int -> Int
                         cuadrado n = n * n

-- d

multiplicapares :: [Int] -> Int
multiplicapares xs = productoria' xs pares
                    where pares :: Int -> Int
                          pares x | mod x 2 == 0 = x
                                  | mod x 2 /= 0 = 1 

-- ejercicio 8

-- a: - Las funciones maps recorren la lista modificando elemento por elemento según lo que se busque hacer.
     -- Las funciones filters reccoren toda la lista y devuelven algo que satisface un predicado.

-- b: - La expresión map succ [1, -4, 6, 2, -8] donde succ n = n+1, equivale a la lista [2, -3, 7, 3, -7].
     -- La expresión filter esPositivo [1, -4, 6, 2, -8] equivale a [1, 6, 2].

-- ejercicio 9

-- a

dobleMe :: [Int] -> [Int]
dobleMe [] = []
dobleMe (x :xs) = (x*2) : dobleMe xs

-- b

dobleMe' :: [Int] -> [Int]
dobleMe' xs = map (* 2) xs

-- ejercicio 10

-- a

paresSon :: [Int] -> [Int]
paresSon [] = []
paresSon (x:xs) | mod x 2 == 0 = x : paresSon xs
                | mod x 2 /= 0 = paresSon xs

-- b

paresSon' :: [Int] -> [Int]
paresSon' xs = filter even xs
             
-- c

multiplicapares' :: [Int] -> Int 
multiplicapares' xs = productoria (paresSon' xs) 
                     
                     

-- ejercicio 11

--a

sumarLista :: Num a => a -> [a] -> [a]
sumarLista n [] = []
sumarLista n (x:xs) = (n+x) : sumarLista n xs	

encabezar :: a -> [[a]] -> [[a]]
encabezar n [] = []
encabezar n (xs:xss) = (n:xs): encabezar n xss

mayoresA :: Ord a => a -> [a] -> [a]
mayoresA n [] = []
mayoresA n (x:xs) | x `compare` n == GT = x : mayoresA n xs
                  | otherwise = mayoresA n xs

--b 

sumarLista' :: Num a => a -> [a] -> [a]
sumarLista' n xs = map (+n) xs

encabezar' :: a -> [[a]] -> [[a]]
encabezar' a xss = map (a:) xss

mayoresA' :: Ord a => a -> [a] -> [a]
mayoresA' n xs = filter (>n) xs 
                
--ejercicio 12

sacatup :: (Int, String) -> String
sacatup (x,e) = e 
                

siesM :: Int -> (Int, String) -> Bool
siesM m (k,j) = k == m 

ctu :: [(Int, String)] -> (Int, String)
ctu [] = (0, "")
ctu (y : ys) = y

encuentra' :: Int -> [(Int, String)] -> String
encuentra' n xs =  sacatup ( ctu ( filter (siesM n ) xs ))  

-- ejercicio 13

-- a
primigualesA :: Ord a => a -> [a] -> [a]
primigualesA n [x] = []
primigualesA n (x:e:xs) | n == x = x : primigualesA n (e:xs) 
                        | n == e = primigualesA n xs
                        | n /= x = primigualesA n (e:xs)
                        | n /= e = x :[]
-- ejercicio 13

--b       
primigualesA' :: Ord a => a -> [a] -> [a]
primigualesA' n xs = takeWhile (==n) xs 

-- ejercicio 14

--a
primiguales :: Ord a => [a] -> [a]
primiguales [] = []
primiguales [x] = [x]
primiguales (x:e:xs) | e == x = x : primiguales (e:xs) 
                     | x /= e = x : [] 

--b
primiguales' :: Ord a => [a] -> [a]
primiguales' xs = primigualesA' (head xs) xs

-- ejercicio 15

--a
minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = x`min`minimo xs

-- ejercicio 15

--b
minimo' :: (Bounded a, Ord a) => [a] -> a
minimo'[] = maxBound :: Bounded a => a
minimo' (x:xs) = x`min`minimo' xs  






              
