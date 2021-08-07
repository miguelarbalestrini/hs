sumar1::[Int]->[Int]
sumar1 []=[]
sumar1 (x:xs)=(x+1):sumar1 xs

duplica::[Int]->[Int]
duplica []=[]
duplica (x:xs)=x*2:duplica xs
