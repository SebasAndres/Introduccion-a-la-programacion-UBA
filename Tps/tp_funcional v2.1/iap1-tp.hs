-- Completar con los datos del grupo
--
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
nombresDeUsuarios ((u:us),rs,ps) = snd (u) : nombresDeUsuarios (us,rs,ps)

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
cantidadDeAmigos red user = length (amigosDe red user)

-- EJ 4: 
-- usuarioConMasAmigos: dada una RedSocial, devuelve el usuario con mas amigos. 
--                      devolvemos el usuario que aparece más veces en la lista de relaciones.           
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos ([u],_,_) = u
usuarioConMasAmigos ((u:us),rs,ps) | (cantidadDeAmigos ((u:us),rs,ps) u) >= (cantidadDeAmigos (us,rs,ps) usrMasAmigosResto) = u
                                   | otherwise = usrMasAmigosResto
                                   where usrMasAmigosResto = usuarioConMasAmigos (us,rs,ps)

-- EJ 5:
-- estaRobertoCarlos: dada una RedSocial devuelve True <=> existe un usuario con mas de 1 millon de amigos.
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = ((cantidadDeAmigos red (usuarioConMasAmigos red)) > 1000000)

-- EJ 6:
-- publicacionesDe: dada una RedSocial y un usuario, devuelve todas las publicaciones posteadas por el usuario en la red.
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (_, _, []) user = []
publicacionesDe (us, rs, (p:ps)) user | (usuarioDePublicacion p == user) = p : publicacionesDe (us, rs, ps) user
                                      | otherwise = publicacionesDe (us, rs, ps) user

-- publicacionDeUsuario: dada una publicacion y un usuario, devuelve la misma publicacion si el usuario es el autor,
--                       caso contrario, devuelve [].
publicacionDeUsuario :: Publicacion -> Usuario -> [Publicacion]
publicacionDeUsuario (autor,texto,likes) user | (autor == user) = [(autor,texto,likes)]
                                              | otherwise = []

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
esSubconjunto lista1 [] = False
esSubconjunto (x:xs) lista2 | (elem x lista2) = esSubconjunto xs lista2
                            | otherwise = False

-- EJ 9: 
-- tieneUnSeguidorFiel: dada una RedSocial y un usuario, devuelve True <=> existe un usuario que le likeo todos los posts.
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel ([],rs,ps) user = False
tieneUnSeguidorFiel ((u:us),rs,ps) user | (length (userPosts) == 0) = False
                                        | (u /= user) && (likeoTodosLosPosts u userPosts) = True
                                        | otherwise = tieneUnSeguidorFiel (us, rs, ps) user
                                        where userPosts = publicacionesDe ((u:us),rs,ps) user

-- likeoTodosLosPosts: dado un usuario y una lista de publicaciones, devuelve True <=> el usuario likeo a todas.
likeoTodosLosPosts :: Usuario -> [Publicacion] -> Bool
likeoTodosLosPosts user [] = True
likeoTodosLosPosts user (p:ps) | elem user (likesDePublicacion p) = likeoTodosLosPosts user ps
                               | otherwise = False
-- EJ 10:
-- existeSecuenciaDeAmigos: dada una RedSocial y dos usuarios, devuelve True <=> ambos estan en la misma clase de equivalencia
--                          determinada por la relacion "ser amigo".
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red userI userF | sonAmigos (relaciones red) userI userF = True
                                        | not (tieneAmigos userI && tieneAmigos userF) = False
                                        | otherwise = algunAmigoTieneSecuenciaA red amigos userF
                                        where tieneAmigos user = (cantidadDeAmigos red user > 0)
                                              amigos = amigosDe red userI

-- sonAmigos: dada una lista de relaciones y dos usuarios, devuelve True <=> existe una tupla en la que estan ambos.
sonAmigos :: [Relacion] -> Usuario -> Usuario -> Bool
sonAmigos [] u1 u2 = False
sonAmigos (r:rs) u1 u2 | (fst r == u1) && (snd r == u2) = True 
                       | (fst r == u2) && (snd r == u1) = True
                       | otherwise = sonAmigos rs u1 u2

-- algunAmigoTieneSecuenciaA: dada una lista de usuarios (al menos hay uno) y un usuario final, devuelve True <=> alguno esta en la misma 
--                            clase de equivalencia.
algunAmigoTieneSecuenciaA :: RedSocial -> [Usuario] -> Usuario -> Bool
algunAmigoTieneSecuenciaA red [] user = False
algunAmigoTieneSecuenciaA red (a:as) user | existeSecuenciaDeAmigos red a user = True
                                          | otherwise = algunAmigoTieneSecuenciaA red as user 

-- Borrar:
usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)
