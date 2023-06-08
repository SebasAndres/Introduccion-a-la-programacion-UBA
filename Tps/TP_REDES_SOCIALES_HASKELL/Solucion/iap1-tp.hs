-- Nombre de Grupo: AlgoritmosEnAccion 
-- Integrante 1: Sebastian Andres, sebastian.ignacio.andres@gmail.com, LU: 1028/22
-- Integrante 2: Adrian Florio, adrii.21@hotmail.com, LU: 333/23
-- Integrante 3: Moira Agustina Gonzalez, 18agonz@gmail.com, LU: 355/23
-- Integrante 4: Luciano Salvador Contartese, luciano90@gmail.com, LU: 628/14

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

-- EJ. 1:
-- nombreDeUsuarios: dada una RedSocial, devuelve una lista con todos los usuarios en ella.
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios ([],_,_) = []
nombresDeUsuarios ((u:us),rs,ps) = (nombreDeUsuario u) : nombresDeUsuarios (us,rs,ps)

-- EJ. 2:
-- amigosDe: dada una RedSocial, devuelve la lista de usuarios con los que se relaciona un usuario en esa red,
--           es decir, todos los usuarios que estan una tupla con usuario en la lista de relaciones en la red.  
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (_,[],_) user = [] 
amigosDe (us,(r:rs),ps) user = (amigo r user) ++ amigosDe (us, rs, ps) user 

-- amigo: dada una relacion y un usuario: 
--        caso 1: devuelve [] si el usuario no esta en la relacion.
--        caso 2: devuelve el otro usuario en la relacion, si esta presente en la misma. 
amigo :: (Usuario, Usuario) -> Usuario -> [Usuario]
amigo (us1, us2) user | (us1 == user) = [us2]
                      | (us2 == user) = [us1]
                      | otherwise = []

-- EJ. 3: 
-- cantidadDeAmigos: dada una RedSocial y un usuario, devuelve la longitud de la lista de sus amigos en esa red. 
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red user = longitud (amigosDe red user)

longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud (xs)

-- EJ 4: 
-- usuarioConMasAmigos: dada una RedSocial, devuelve el usuario con mas amigos. 
--                      devolvemos el usuario que aparece más veces en la lista de relaciones.           
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos ([u],_,_) = u
usuarioConMasAmigos ((u:us),rs,ps) | (cantidadDeAmigos ((u:us),rs,ps) u) >= (cantidadDeAmigos (us,rs,ps) usrMasAmigosResto) = u
                                   | otherwise = usrMasAmigosResto
                                   where usrMasAmigosResto = usuarioConMasAmigos (us,rs,ps)

-- EJ 5:
-- estaRobertoCarlos: dada una RedSocial devuelve True <=> existe un usuario con mas de "1 millon" de amigos (10).
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = hayUnUsuarioConMasDeNAmigos red 10

hayUnUsuarioConMasDeNAmigos :: RedSocial -> Int -> Bool
hayUnUsuarioConMasDeNAmigos red n = ((cantidadDeAmigos red (usuarioConMasAmigos red)) > n)

-- EJ 6:
-- publicacionesDe: dada una RedSocial y un usuario, devuelve todas las publicaciones posteadas por el usuario en la red.
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (_, _, []) user = []
publicacionesDe (us, rs, (p:ps)) user | (usuarioDePublicacion p == user) = p : publicacionesDe (us, rs, ps) user
                                      | otherwise = publicacionesDe (us, rs, ps) user

-- EJ 7:
-- publicacionesQueLeGustanA: dada una RedSocial y un usuario, devuelve una lista de las publicaciones que le gustan
--                            en esa red.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (_, _, []) user = [] 
publicacionesQueLeGustanA (us, rs, (p:ps)) user | (elem user (likesDePublicacion p)) = p : publicacionesQueLeGustanA (us, rs, ps) user
                                                | otherwise = publicacionesQueLeGustanA (us, rs, ps) user

-- EJ 8: 
-- lesGustanLasMismasPublicaciones: dada una RedSocial y dos usuarios, devuelve True <=> a ambos usuarios les gustan las 
--                                  mismas publicaciones.
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red us1 us2 = mismosElementos (publicacionesQueLeGustanA red us1) (publicacionesQueLeGustanA red us2)

-- mismosElementos: devuelve True <=> las listas tienen los mismos elementos (por doble inclusion)
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos lista1 lista2 = (esSubconjunto lista1 lista2) && (esSubconjunto lista2 lista1)

-- esSubconjunto: devuelve True <=> la primer lista dada es un subconjunto de (esta incluida en) la segunda 
esSubconjunto :: (Eq t) => [t] -> [t] -> Bool
esSubconjunto [] lista2 = True
esSubconjunto _ [] = False
esSubconjunto (x:xs) lista2 = (elem x lista2) && esSubconjunto xs lista2

-- EJ 9: 
-- tieneUnSeguidorFiel: dada una RedSocial y un usuario, devuelve True <=> existe un usuario que le likeo todos los posts.
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel ([],_,_) _ = False
tieneUnSeguidorFiel ((u:us),rs,ps) user | (length (userPosts) == 0) = False
                                        | (u /= user) && (likeoTodosLosPosts u userPosts) = True
                                        | otherwise = tieneUnSeguidorFiel (us, rs, ps) user
                                        where userPosts = publicacionesDe ((u:us),rs,ps) user

-- likeoTodosLosPosts: dado un usuario y una lista de publicaciones, devuelve True <=> el usuario likeo a todas. Tambien devuelve true si la lista de publicaciones esta vacia.
likeoTodosLosPosts :: Usuario -> [Publicacion] -> Bool
likeoTodosLosPosts user [] = True
likeoTodosLosPosts user (p:ps) | elem user (likesDePublicacion p) = likeoTodosLosPosts user ps
                               | otherwise = False
-- EJ 10:
-- existeSecuenciaDeAmigos: dada una RedSocial devuelve True <=> Existe una cadena de amigos entre user1 y user2,
-- es decir, si estan en la misma particion o clase de equivalencia, determinada por la relacion "Amigo".
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red user1 user2 = existeSecuenciaDeAmigosSin red user1 user2 []

-- para esto implementamos una funcion auxiliar que toma rastro de los caminos recorridos para evitar bucles infinitos
-- existeSecuenciaDeAmigosSin: devuelve True cuando existe un camino entre el user1 y el user2, sin pasar por los nodos
-- determinados en chequeados. 
existeSecuenciaDeAmigosSin :: RedSocial -> Usuario -> Usuario -> Set Usuario -> Bool
existeSecuenciaDeAmigosSin red user1 user2 chequeados | relacionadosDirecto user1 user2 red = True
                                                      | not ((tieneAmigos user1) && (tieneAmigos user2)) = False
                                                      | otherwise = algunAmigoTieneSecuenciaA red amigosUser1 user2 (user1:chequeados)
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
--    chequeados: lista de usuarios ya chequeados
algunAmigoTieneSecuenciaA :: RedSocial -> [Usuario] -> Usuario -> Set Usuario -> Bool
algunAmigoTieneSecuenciaA red [] _ _ = False
algunAmigoTieneSecuenciaA red (a:as) usf chequeados = (not (elem a chequeados) && existeSecuenciaDeAmigosSin red a usf chequeados) 
                                                      || (algunAmigoTieneSecuenciaA red as usf (agregar a (chequeados)))

-- relacionadosDirecto: dada una red y dos usuarios validos, devuelve true si son amigos, es decir, si existe una 
-- tupla (u1,u2) o (u2,u1) en las relaciones de la red.
relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto u1 u2 (_,[],_) = False
relacionadosDirecto u1 u2 (us,(r:rs),pst) = (r == (u1,u2)) || (r == (u2,u1)) || (relacionadosDirecto u1 u2 (us,rs,pst))
