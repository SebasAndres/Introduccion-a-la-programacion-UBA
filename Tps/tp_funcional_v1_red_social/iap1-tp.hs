-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios
-- [1]
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios ([], _, _) = []
nombresDeUsuarios ((u:us), rs, ps) = snd u : nombresDeUsuarios (us, rs, ps)

-- [2]
-- describir qué hace la función: 
-- devuelve la lista de usuarios con los que se relaciona un usuario,
-- es decir, todos los usuarios que estan una tupla con usuario en la lista de relaciones en la red  
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (us, [], ps) user = []
amigosDe (us, (rs:rss), ps) user = (filtrar_relacion rs user) ++ amigosDe (us, rss, ps) user

filtrar_relacion :: Relacion -> Usuario -> [Usuario]
filtrar_relacion (us1, us2) user | (us1 /= user) && (us2 /= user) = []
                                 | (us1 == user) = [us2]
                                 | (us2 == user) = [us1]
                                 | otherwise = []
-- [3]
-- describir qué hace la función: 
-- devuelve la longitud de la lista de los usuarios que se relacionan con el usuario
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red user = longitud (amigosDe red user)

longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- [4]
-- describir qué hace la función:
-- devuelve el usuario con mayor numero de amigos
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos ([user],rs,ps) = user
usuarioConMasAmigos ((u:us),rs,ps) = masAmigos ((u:us),rs,ps) u (usuarioConMasAmigos (us, rs, ps))

-- masAmigos: devuelve el usuario con mas amigos entre los dos dados
masAmigos :: RedSocial -> Usuario -> Usuario -> Usuario
masAmigos red us1 us2 | (cantidadDeAmigos red us1 >= cantidadDeAmigos red us2) = us1
                      | otherwise = us2
-- [5]
-- describir qué hace la función: 
-- devuelve True <=> existe un usuario con mas de 1 millon de amigos
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red | cantidadDeAmigos red (usuarioConMasAmigos red) > 1000000 = True
                      | otherwise = False

-- [6]
-- describir qué hace la función: 
-- devuelve las publicaciones del usuario dado, en la red dada
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (us, rs, []) user = []
publicacionesDe (us, rs, (p:ps)) user | (usuarioDePublicacion p == user) = p : publicacionesDe (us, rs, ps) user
                                      | otherwise = publicacionesDe (us, rs, ps) user

-- [7]
-- describir qué hace la función: 
-- devuelve todas las publicaciones que le gustan al usuario dado
-- es decir, todas las publicaciones en las que aparece user en la lista de me gusta
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (us, rs, []) user = []
publicacionesQueLeGustanA (us, rs, (p:ps)) user | (elem user (likesDePublicacion p)) = p : (publicacionesQueLeGustanA (us, rs, ps) user)
                                                | otherwise = publicacionesQueLeGustanA (us, rs, ps) user

-- [8]
-- describir qué hace la función: 
-- devuelve True <=> a los dos usuarios dados les gustan los mismos posts
-- es decir, True <=> los conjuntos que vienen de los posts que les gustan a los usuarios son iguales
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red us1 us2 = mismosElementos (publicacionesQueLeGustanA red us1) (publicacionesQueLeGustanA red us2)

-- mismosElementos: devuelve True <=> las listas tienen los mismos elementos (por doble inclusion)
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos lista1 lista2 = (esSubconjunto lista1 lista2) && (esSubconjunto lista2 lista1)

--esSubconjunto: devuelve True <=> la primer lista dada es un subconjunto de (esta incluida en) la segunda 
esSubconjunto :: (Eq t) => [t] -> [t] -> Bool
esSubconjunto [] lista2 = True
esSubconjunto lista1 [] = False
esSubconjunto (x:xs) lista2 | (elem x lista2) = esSubconjunto xs lista2
                            | otherwise = False
-- [9]
-- describir qué hace la función: 
-- devuelve True <=> el usuario dado tiene publicaciones en la red y existe un usuario distinto a el que likeo todas sus publicaciones
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel ([],rs,ps) user = False 
tieneUnSeguidorFiel ((u:us),rs,ps) user | (longitud (publicacionesDe red user) == 0) = False    
                                        | (u /= user) && (likeoTodosLosPostsDe u user (publicacionesDe red user)) = True
                                        | otherwise = tieneUnSeguidorFiel (us,rs,ps) user
                                        where red = ((u:us),rs,ps)

-- likeoTodosLosPostsDe: devuelve True <=> userA likeo todos los posts de userB
likeoTodosLosPostsDe :: Usuario -> Usuario -> [Publicacion] -> Bool
likeoTodosLosPostsDe userA userB [] = True
likeoTodosLosPostsDe userA userB (p:ps) | elem userA (likesDePublicacion p) = likeoTodosLosPostsDe userA userB ps
                                        | otherwise = False

-- [10]
-- describir qué hace la función: 
-- devuelve True <=> Existe una cadena de amigos entre user1 y user2, es decir, si estan en la misma particion
-- o clase de equivalencia, determinada por la relacion "Amigo".
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red user1 user2 = existeSecuenciaDeAmigosSin red user1 user2 []

-- para esto implementamos una funcion auxiliar que toma rastro de los caminos recorridos para evitar bucles infinitos

-- existeSecuenciaDeAmigosSin: devuelve True cuando existe un camino entre el user1 y el user2, sin pasar por los nodos
-- determinados en chks. 
existeSecuenciaDeAmigosSin :: RedSocial -> Usuario -> Usuario -> Set Usuario -> Bool
existeSecuenciaDeAmigosSin red user1 user2 chks | relacionadosDirecto red user1 user2 = True
                                                | not ((tieneAmigos user1) && (tieneAmigos user2)) = False
                                                | otherwise = algunAmigoTieneSecuenciaA red amigosUser1 user2 (user1:chks)
                                                where tieneAmigos userk = (longitud (amigosDe red userk) > 0) 
                                                      amigosUser1 = amigosDe red user1

-- para implementar este registro de los usuarios chequeados es conveniente usar un conjunto 
type Set t = [t]

agregar :: (Eq t) => t -> Set t -> Set t
agregar t set | elem t set = set
              | otherwise = t : set

-- algunAmigoTieneSecuenciaA: dado una lista de amigos con al menos un elemento, un usuario final y los nodos registrados
-- devuelve True <=> existe un camino de amigos entre algun amigo y el usuario final, sin pasar por los nodos ya chequeados
-- params:
--    red: red de la cual vemos las relaciones
--    amigos: lista de amigos donde buscar 
--    usf: usuario destino
--    chks: lista de usuarios ya chequeados
algunAmigoTieneSecuenciaA :: RedSocial -> [Usuario] -> Usuario -> Set Usuario -> Bool
algunAmigoTieneSecuenciaA red [] usf chks = False
algunAmigoTieneSecuenciaA red (a:as) usf chks | not (elem a chks) && existeSecuenciaDeAmigosSin red a usf chks = True
                                              | otherwise = algunAmigoTieneSecuenciaA red as usf (agregar a (chks))

-- relacionadosDirecto: dada una red y dos usuarios validos, devuelve true si son amigos, es decir, si existe una 
-- tupla (u1,u2) o (u2,u1) en las relaciones de la red.
relacionadosDirecto :: RedSocial -> Usuario -> Usuario -> Bool
relacionadosDirecto (_,[],_) u1 u2 = False
relacionadosDirecto (us,(r:rs),pst) u1 u2 | (fst r == u1) && (snd r == u2) = True
                                          | (fst r == u2) && (snd r == u1) = True
                                          | otherwise = relacionadosDirecto (us,rs,pst) u1 u2