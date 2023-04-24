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

contiene :: [t] -> [t] -> Bool
contiene lista_a lista_b = 