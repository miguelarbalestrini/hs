data Carrera = Matematica | Fisica | Computacion | Astronomia | Profesorado deriving (Show, Eq, Ord, Bounded, Enum)

--1
titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matemática"
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en Ciencias de la Computacion" 
titulo Astronomia = "Licenciatura en Astronomia"
titulo Profesorado = "Profesorado de Matematica"

--2
titulo' :: Carrera -> String
titulo' x | x == Matematica = "Licenciatura en Matemática"
titulo' x | x == Fisica = "Licenciatura en Fisica"
titulo' x | x == Computacion = "Licenciatura en Ciencias de la Computacion" 
titulo' x | x == Astronomia = "Licenciatura en Astronomia"
titulo' x | x == Profesorado = "Profesorado de Matematica"
titulo' _ = undefined 


-- Ejercicio 2.

-- 2/a. Ingreso es un sinonimo de tipo.

type Ingreso = Int

-- 2/b. Cargo y Area son tipos enumerados

data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar deriving (Show, Eq, Ord, Bounded, Enum)

data Area = Administrativa | Ensenanza | Economica | Postgrado deriving (Show, Eq, Ord, Bounded, Enum)

-- Rol es un tipo algebraico

data Rol = Decanx Genero
         | Docente Cargo 
         | NoDocente Area 
         | Estudiante [(Carrera, Ingreso)] deriving (Show, Eq, Ord)

data Genero = Masculino | Femenino deriving (Show, Eq, Ord)

-- 2/c.            

cuantos_doc :: [Rol] -> Cargo -> Int
cuantos_doc [] _ = 0 
cuantos_doc (Docente Titular : xs) Titular = 1 + cuantos_doc  xs Titular 
cuantos_doc (Docente Asociado : xs) Asociado = 1 + cuantos_doc xs Asociado 
cuantos_doc (Docente Adjunto : xs) Adjunto = 1 + cuantos_doc xs Adjunto
cuantos_doc (Docente Asistente : xs) Asistente = 1 + cuantos_doc xs Asistente 
cuantos_doc (Docente Auxiliar : xs) Auxiliar  = 1 + cuantos_doc xs Auxiliar 
cuantos_doc ( _ : xs) c = cuantos_doc xs c 

-- 2/d.

cuantos_doc' :: [Rol] -> Cargo -> Int
cuantos_doc' xs c = length (filter (==Docente c) xs)  

-- 2/f.

estudia :: Rol -> Carrera -> Bool
estudia (Estudiante []) _ = False
estudia (Decanx _) _ = False
estudia (NoDocente _) _ = False
estudia (Docente _) _ = False
estudia (Estudiante ((x,_):xs)) c = x == c || estudia (Estudiante xs) c

--Ejercicio 3.

--3/a. 

data Persona = Per String String Int Int Int Int Rol deriving (Show, Eq, Ord)

miguel = Per "rodenas" "miguel" 40683999 15 10 1997 (Estudiante [(Computacion, 2017)])
tome = Per "Uribe" "Tomas" 41624903 12 11 1998 (Estudiante [(Computacion, 2017)])
maria = Per "Herrero" "Maria" 42546777 12 9 1999 (Estudiante [(Astronomia, 2017)])
carlos = Per "Gomez" "Carlos" 32456888 1 5 1985 (NoDocente Administrativa) 
martin = Per "Sandoval" "Martin" 32578999 1 4 1990 (NoDocente Economica)

--3/b. 

-- 3/c.

-- Si se puede.

-- 3/c/1.

edad' :: Persona -> (Int, Int, Int) -> Int
edad' (Per _ _ _ d m a _) (d', m', a') | a >= a' = 0
                                       | m < m' = a'-a
                                       | m > m' = a'-a-1
                                       | m == m' = if d <= d' then a'-a
                                                else if d > d' then a'-a-1 
                                                else a'-a

edad' (Per _ _ _ _ _ _ _) (_, _, _) = undefined

-- 3/c/2.

existe :: String -> [Persona] -> Bool
existe _ [] = False
existe d ((Per a _ _ _ _ _ _):xs) = d == a || existe d xs

-- 3/c/3.

est_Astronomia :: [Persona] -> [Persona]
est_Astronomia xs = filter (est Astronomia) xs

est :: Carrera -> Persona -> Bool
est Astronomia (Per _ _ _ _ _ _ (Estudiante ((Astronomia, _ ) : _))) = True
est Astronomia (Per a b c d e f (Estudiante ((_ , _):xs))) = est Astronomia (Per a b c d e f (Estudiante xs))
est Astronomia (Per _ _ _ _ _ _ (Estudiante [])) = False
est _ _  = False 

-- 3/c/4.

ptupla :: [Persona] -> [(String, Int)]
ptupla [] = []
ptupla ((Per a b c _ _ _ (NoDocente _)) : xs) = (a ++ " " ++ b, c) : ptupla xs
ptupla ((Per _ _ _ _ _ _ _) : _ ) = undefined

nd :: Persona -> Bool
nd (Per _ _ _ _ _ _ (NoDocente _ )) = True
nd (Per _ _ _ _ _ _ _) = False

padron_Nodocente :: [Persona] -> [(String, Int)]
padron_Nodocente xs = ptupla (filter (nd) xs)

-- Ejercicio 4.

data Cola = Vacia | Encolada Persona Cola deriving (Show)

-- 4/a/1.

atender :: Cola -> Cola
atender Vacia = Vacia
atender (Encolada _ x) = x 

-- 4/a/2.

encolar :: Persona -> Cola -> Cola
encolar p Vacia = (Encolada p Vacia)
encolar p (Encolada x u) = Encolada x (encolar p u)

-- 4/a/3.

busca :: Cola -> Cargo -> Persona
busca Vacia _ = error "no hay docente"
busca (Encolada p r) c | boolian p c == True = p
                       | boolian p c == False = busca r c
                       | otherwise = error "No hay docente"

boolian :: Persona -> Cargo -> Bool
boolian (Per _ _ _ _ _ _ (Docente c)) r = c == r
boolian  _ _ = False 

-- 4/c.

-- Cola se parece a las listas de personas.

data ListaP = Vacia' | Cons Persona ListaP

-- Las mismas funciones con el nuevo tipo:

atender' :: ListaP -> ListaP
atender' Vacia' = Vacia'
atender' (Cons _ x) = x 

encolar' :: Persona -> ListaP  -> ListaP 
encolar' p Vacia' = (Cons p Vacia')
encolar' p (Cons x u) = Cons x (encolar' p u)

busca' :: ListaP -> Cargo -> Persona
busca' Vacia' _ = error "no hay docente"
busca' (Cons p r) c | booliar p c == True = p
                    | booliar p c == False = busca' r c
                    | otherwise = error "No hay docente"

booliar :: Persona -> Cargo -> Bool
booliar (Per _ _ _ _ _ _ (Docente c)) r = c == r
booliar  _ _ = False 

-- Ejercicio 5.

data ListaAsoc a b = LVacia | Nodo a b ( ListaAsoc a b ) deriving (Eq, Show)

type Diccionario = ListaAsoc String String

type Padron = ListaAsoc Int String

-- 5/a.

type GuiaTel = ListaAsoc String Int

-- 5/b/1.

la_long :: (Integral c) => ListaAsoc a b -> c
la_long LVacia = 0
la_long (Nodo _ _ c) = 1 + la_long c

-- 5/b/2.

la_concat :: (Eq a, Eq b) => ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat LVacia y = y
la_concat x LVacia = x
la_concat (Nodo a b (z)) y | z == LVacia = (Nodo a b (y))
                           | otherwise = (Nodo a b (la_concat z y))

-- 5/b/3.

la_pares :: ListaAsoc a b -> [(a,b)]
la_pares LVacia = []
la_pares (Nodo a b c) = (a, b) : la_pares c

-- 5/b/4.
la_busca :: (Eq a) => ListaAsoc a b -> a -> Maybe b
la_busca LVacia _ = Nothing
la_busca (Nodo a b c) x | a == x = Just b
                        | otherwise = la_busca c x


-- 6
data Arbol a = Hoja | Rama ( Arbol a ) a ( Arbol a ) deriving (Show, Eq, Ord) 
type Prefijos = Arbol String

can , cana , canario , canas , cant , cantar , canto :: Prefijos

can = Rama cana "can" cant
cana = Rama canario "a" canas
canario = Rama Hoja "rio" Hoja 
canas = Rama Hoja "s" Hoja
cant = Rama cantar "t" canto
cantar = Rama Hoja "ar" Hoja
canto = Rama Hoja "o" Hoja

-- 6/a

a_long :: Integral b => Arbol a -> b
a_long Hoja = 0
a_long (Rama a _ c) = 1 + a_long a + a_long c



-- 6/b

a_hojas :: Integral b => Arbol a -> b
a_hojas Hoja = 1
a_hojas (Rama a _ c) = a_hojas a + a_hojas c

-- 6/c

a_inc :: Num a => Arbol a -> Arbol a
a_inc Hoja = Hoja
a_inc (Rama a b c) = Rama (a_inc a) (b + 1) (a_inc c)



-- 6/d

a_nombre :: Arbol Persona -> Arbol String
a_nombre Hoja = Hoja
a_nombre (Rama x (Per a b _ _ _ _ _ ) f) = (Rama (a_nombre x) (a ++ " " ++ b) (a_nombre f))


-- 6/e

a_map :: (a -> b) -> Arbol a -> Arbol b
a_map _ Hoja = Hoja
a_map r (Rama a b c) = Rama (a_map r a) (r b) (a_map r c)

-- 6/f

a_sum :: Num a => Arbol a -> a
a_sum Hoja = 0
a_sum (Rama a b c) = b+ a_sum a + a_sum c

-- 6/g

a_prod :: Num a => Arbol a -> a
a_prod Hoja = 1
a_prod (Rama a b c) = b * a_prod a * a_prod c



  
















                   
                                        
