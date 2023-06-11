-- [1]
longitud :: [t] -> Integer
longitud [] = 0
longitud [a] = 1
longitud (x:xs) = 1 + longitud xs

ultimo :: [t] -> t
ultimo [t] = t
ultimo (x:xs) = ultimo (xs)

principio :: [t] -> [t]
principio [t] = [] 
principio (x:xs) = x : principio xs

reverso :: [t] -> [t]
reverso [t] = [t]
reverso l = ultimo l : reverso (sacarUltimo l)

sacarUltimo :: [t] -> [t]
sacarUltimo [t] = []
sacarUltimo (x:xs) = x : sacarUltimo xs

-- [2]
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece t [a] = (t == a)
pertenece t (x:xs) | (t == x) = True
                   | otherwise = pertenece t xs     
                    
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [a,b] = (a == b)
todosIguales (x:xs) = (x == head (xs)) && (todosIguales xs)

todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [a,b] = (a /= b)
todosDistintos (x:xs) = not (pertenece x xs) && (todosDistintos xs)

hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos lista = not (todosDistintos lista)

quitar :: (Eq t) => t -> [t] -> [t]
quitar x (first:tail) | (x == first) = tail
                      | otherwise = first : (quitar x tail)

quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos x [a] | (x == a) = []
                  | otherwise = [a]
quitarTodos x (first:tail) | (x == first) = quitarTodos x tail
                           | otherwise = first : (quitarTodos x tail)

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [t] = [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos (quitarTodos x xs)

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos lista_a lista_b = (contiene lista_a lista_b) && (contiene lista_b lista_a)

-- A C B :: # A incluida en B
contiene :: (Eq t) => [t] -> [t] -> Bool
contiene [a] lista_b = pertenece a lista_b
contiene lista_a lista_b = (pertenece (head (lista_a)) lista_b) &&  (contiene (tail lista_a) lista_b)

capicua :: (Eq t) => [t] -> Bool
capicua ls = (reverso ls == ls)

--[3]
sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria (xs)

productoria :: [Integer] -> Integer
productoria [] = 1
productoria (x:xs) = x * productoria (xs)

maximo :: [Integer] -> Integer
maximo [t] = t 
maximo [a,b] | (a >= b) = a 
             | otherwise = b 
maximo (x:xs) = maximo [x, (maximo xs)]

sumarN :: (Num t) => t -> [t] -> [t]
sumarN n [t] = [n+t]
sumarN n (x:xs) = n+x : sumarN n xs

sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero (x:xs) = sumarN x (x:xs)

sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo lista = sumarN (ultimo lista) lista

pares :: [Integer] -> [Integer]
pares lista = multiplosDeN 2 lista

multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN n [t] | (mod t n == 0) = [t]
                   | otherwise = []
multiplosDeN n (x:xs) | (mod x n == 0) = x : multiplosDeN n xs
                      | otherwise = multiplosDeN n xs

ordenar :: [Integer] -> [Integer]
ordenar [t] = [t]
ordenar lista = menor_lista (lista) : ordenar (lista_sin_menor (lista))
            where
                menor_lista (x:xs) = menor [x,(menor xs)] 
                lista_sin_menor lista = quitar (menor_lista lista) lista

menor :: [Integer] -> Integer
menor [t] = t 
menor (x:xs) | (x <= mxs) = x
             | otherwise = mxs
             where mxs = menor (xs)
--[4]

-- 4.1
sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [t] = [t]
sacarBlancosRepetidos (x:xs) | (x == ' ') && (head xs == ' ') = sacarBlancosRepetidos xs
                             | otherwise = x : sacarBlancosRepetidos xs

-- 4.2
-- contarPalabras_v1: (por espacios ~ un caracter es una palabra)
contarPalabras :: [Char] -> Integer
contarPalabras [] = 0
contarPalabras txt = cantidadBlancos (clean_txt) + 1
                     where clean_txt = trim (txt)
                    
trim :: [Char] -> [Char]
trim txt = sacarBlancoPrimero (sacarBlancoUltimo (sacarBlancosRepetidos (txt)))

sacarBlancoPrimero :: [Char] -> [Char]
sacarBlancoPrimero txt | (head txt == ' ') = tail txt
                       | otherwise = txt

sacarBlancoUltimo :: [Char] -> [Char]
sacarBlancoUltimo txt | (ultimo txt == ' ') = sacarUltimo (txt)
                      | otherwise = txt

cantidadBlancos :: [Char] -> Integer
cantidadBlancos [] = 0
cantidadBlancos (x:xs) | (x == ' ') = 1 + cantidadBlancos xs
                       | otherwise = cantidadBlancos xs

-- Extra: contarPalabras_v2:
contarPalabras_v2 :: [Char] -> Integer
contarPalabras_v2 [] = 0
contarPalabras_v2 txt = longitud (palabras txt)

-- 4.3 
palabraMasLarga :: [Char] -> [Char]
palabraMasLarga txt = palabraMasLargaEnLista (palabras txt)

palabraMasLargaEnLista :: [[Char]] -> [Char]
palabraMasLargaEnLista [t] = t 
palabraMasLargaEnLista (w:ws) | (maximo [lw,lws] == lw) = w 
                              | otherwise = palabraMasLargaEnLista (ws)
                              where lw = longitud(w)
                                    lws = longitud(palabraMasLargaEnLista (ws))
-- 4.4
palabras :: [Char] -> [[Char]]
palabras [] = []
palabras t | tieneEspacios (txt) = primerPalabra (txt) : palabras (sacarPrimerPalabra txt) 
           | otherwise = [primerPalabra (txt++" ")]
           where txt = trim (t)

tieneEspacios :: [Char] -> Bool
tieneEspacios txt = (idxPrimerBlanco txt /= -1) 

primerPalabra :: [Char] -> [Char]
primerPalabra txt = subseq 0 ((idxPrimerBlanco txt)-1) txt

sacarPrimerPalabra :: [Char] -> [Char]
sacarPrimerPalabra txt = subseq ((idxPrimerBlanco txt)+1) ((longitud txt)-1) txt

subseq :: Integer -> Integer -> [t] -> [t]
subseq d h lista | (d < h) = (index lista d) : subseq (d+1) h lista
                 | (d == h) = [index lista d]
                 | otherwise = []

index :: [t] -> Integer -> t
index lista i = indexDesde lista 0 i 

indexDesde :: [t] -> Integer -> Integer -> t 
indexDesde (x:xs) k i | (k == i) = x
                      | (k < i) = indexDesde xs (k+1) i

idxPrimerBlanco :: [Char] -> Integer
idxPrimerBlanco txt = idxPrimerBlancoAux txt (longitud txt)

idxPrimerBlancoAux :: [Char] -> Integer -> Integer
-- Params:
--   txt ([Char]): input text,
--   lng (Integer): first text length
idxPrimerBlancoAux [] lng = - (lng+1)
idxPrimerBlancoAux (x:xs) lng | (x == ' ') = 0
                              | otherwise = 1 + idxPrimerBlancoAux xs lng

-- 4.5
aplanar :: [[Char]] -> [Char]
aplanar [a] = a
aplanar (ls:lss) = ls ++ aplanar (lss)

--4.6
aplanarConBlancos :: [[Char]] -> [Char]
aplanarConBlancos [a] = a
aplanarConBlancos (ls:lss) = ls ++ " " ++ aplanarConBlancos (lss)

--4.7
aplanarConNBlancos :: [[Char]] -> Integer -> [Char]
aplanarConNBlancos [a] n = a
aplanarConNBlancos (ls:lss) n = ls ++ nCharArray ' ' n ++ aplanarConNBlancos (lss) n

nCharArray :: Char -> Integer -> [Char]
nCharArray c 1 = [c]
nCharArray c n = c : nCharArray c (n-1)

--[5]

--5.1
nat2bin :: Integer -> [Integer]
nat2bin 0 = [0]
nat2bin 1 = [1]
nat2bin n = nat2bin (div n 2) ++ [mod n 2]

--5.2
bin2nat :: [Integer] -> Integer
bin2nat [t] = t 
bin2nat (x:xs) | (x == 1) = 2^l + bin2nat (xs)
               | otherwise = bin2nat (xs)
               where l = longitud (x:xs)-1

--5.3
-- nat2hex :: Integer -> [Char]

--5.4
sumaAcumulada :: (Num t) => [t] -> [t]
sumaAcumulada [t] = [t]
sumaAcumulada (x:xs) = x : sumarN x (sumaAcumulada xs)

--5.5
descomponerEnPrimos :: [Integer] -> [[Integer]]
descomponerEnPrimos [t] = [descomponerEnPrimosN t]
descomponerEnPrimos (x:xs) = descomponerEnPrimosN x : descomponerEnPrimos xs

descomponerEnPrimosN :: Integer -> [Integer]
descomponerEnPrimosN n = filtrar_por_primos (divisores (n))

-- devuelve una lista con todos los divisores de n
divisores :: Integer -> [Integer] 
divisores n = divisoresDesde n 1

divisoresDesde :: Integer -> Integer -> [Integer]
divisoresDesde n h | (h == n) = [n]
                   | (mod n h == 0) = h : divisoresDesde n (h+1)
                   | otherwise = divisoresDesde n (h+1)

-- devuelve los elementos de la lista que son primos
filtrar_por_primos :: [Integer] -> [Integer]
filtrar_por_primos [x] | esPrimo (x) = [x]
                       | otherwise = []
filtrar_por_primos (x:xs) | esPrimo (x) = x : filtrar_por_primos (xs)
                          | otherwise = filtrar_por_primos (xs)

esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = (menorDivisor n == n)

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | (mod n k == 0) = k
                      | otherwise = menorDivisorDesde n (k+1)

--[6]
type Set a = [a]

agregarATodos :: Integer -> Set (Set Integer) -> Set (Set Integer)
agregarATodos n [t] = [n:t]
agregarATodos n cls = [n:head(cls)] ++ agregarATodos n (tail cls)

partes :: Integer -> Set (Set Integer)
partes 0 = [[]]
partes n = partes (n-1) ++ agregarATodos n (partes (n-1))

productoCartesiano :: Set Int -> Set Int -> Set (Int, Int)
productoCartesiano [] b = []
productoCartesiano a [] = []
productoCartesiano (ax : axs) b = armarTuplas ax b ++ productoCartesiano axs b 

armarTuplas :: Int -> Set Int -> Set (Int, Int)
armarTuplas n [] = []
armarTuplas n (x:xs) = (n, x) : armarTuplas n xs