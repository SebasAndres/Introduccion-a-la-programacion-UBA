import Test.HUnit
import Solucion

-- tipear run ejecuta todos los testsSuite que armamos
run = runTestTT todosLosTest

todosLosTest = test [testSuite1,testSuite2,testSuite3,testSuite4,testSuite5,testSuite6,testSuite7,testSuite8,testSuite9,testSuite10]

-- Funciones auxiliares usadas:
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- Variables comunes usadas
usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (6, "Cristina")
usuario7 = (7, "Diego")
usuario8 = (8, "Armando")
usuario9 = (9, "Carlos")
usuario10 = (10, "Saul")
usuario11 = (11, "Roberto Carlos")
usuario12 = (12, "Fideo")

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion2_1 = (usuario2, "Post", [usuario1])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])

redVacia = ([],[],[])
-- Ej 1: nombresDeUsuarios
redCon1Usuario = ([usuario1], [], [])
redConMasde1Usuario = ([usuario1,usuario2,usuario3], [], [])

testSuite1 = test [
    -- a. Cuando no hay usuarios en la red debe devolver una lista vacia
    "nombresDeUsuarios en red vacia " ~: nombresDeUsuarios redVacia ~?= [],
    -- b. Hay un usuario solo en la red social debe devolver una lista con el nombre del unico usuario.
    "nombresDeUsuarios en red con 1 usuario" ~: nombresDeUsuarios redCon1Usuario ~?= [nombreDeUsuario (usuario1)],
    -- c. Hay mas de un usuario, debe devolver los nombres de todos esos usuarios en la red
    "nombresDeUsuarios en red con mas de 1 usuario" ~: mismosElementos (nombresDeUsuarios redConMasde1Usuario) [nombreDeUsuario (usuario1),nombreDeUsuario (usuario2), nombreDeUsuario (usuario3)] ~?= True
    ]

-- Ej 2: amigosDe
redSinRelaciones = ([usuario1], [], [])
redConRelaciones = ([usuario1, usuario2, usuario3], [(usuario2, usuario3)], [])
redUsuario1Con1Amigo1C = ([usuario1, usuario2], [(usuario1, usuario2)], []) 
redUsuario1Con1Amigo2C = ([usuario1, usuario2], [(usuario1, usuario2)], []) 
redUsuario1ConVariosAmigos1C = ([usuario1, usuario2, usuario3], [(usuario1, usuario2), (usuario1, usuario3)], [])
redUsuario1ConVariosAmigos2C = ([usuario1, usuario2, usuario3], [(usuario2, usuario1), (usuario3, usuario1)], [])
redUsuario1ConVariosAmigosMixto = ([usuario1, usuario2, usuario3], [(usuario1, usuario2), (usuario3, usuario1)], [])

testSuite2 = test [
    -- a. La red no tiene relaciones
    "amigosDe en red sin relaciones" ~: amigosDe redSinRelaciones usuario1 ~?= [],
    -- b. La red tiene relaciones pero el usuario no tiene amigos,
    "amigosDe red con relaciones, usuario sin amigos" ~: amigosDe redConRelaciones usuario1 ~?= [],
    -- c. La red tiene relaciones, el usuario tiene 1 amigo y este aparece en la segunda coordenada de la relacion
    "amigosDe red con relaciones, usuario con 1 amigo 2da coordenada" ~: amigosDe redUsuario1Con1Amigo2C usuario1 ~?= [usuario2],
    -- d. La red tiene relaciones, el usuario tiene 1 amigo y este aparece en la primera coordenada de la relacion
    "amigosDe red con relaciones, usuario con 1 amigo para coordenada" ~: amigosDe redUsuario1Con1Amigo1C usuario1 ~?= [usuario2],
    -- e. La red tiene relaciones, el usuario tiene mas de 1 amigo y este aparece siempre en la primera coordenada de la relacion
    "amigosDe red con relaciones, usuario con mas de 1 amigo, aparece siempre en 1C" ~: expectAny (amigosDe redUsuario1ConVariosAmigos1C usuario1) [[usuario2, usuario3],[usuario3,usuario2]], 
    -- f. La red tiene relaciones, el usuario tiene mas de 1 amigo y este aparece siempre en la segunda coordenada de la relacion
    "amigosDe red con relaciones, usuario con mas de 1 amigo, aparece siempre en 2C" ~: expectAny (amigosDe redUsuario1ConVariosAmigos2C usuario1) [[usuario2, usuario3],[usuario3,usuario2]], 
    -- g. La red tiene relaciones, el usuario tiene mas de 1 amigo y este aparece tanto en la primera como en la segunda coordenada
    "amigosDe red con relaciones, usuario con mas de 1 amigo, aparece en coordenadas mixtas" ~: expectAny (amigosDe redUsuario1ConVariosAmigosMixto usuario1) [[usuario2, usuario3],[usuario3,usuario2]]
    ]

-- Ej 3: cantidadDeAmigos
redConRelacionesU1SinAmigos = redConRelaciones
redConRelacionesU1VariosAmigos = redUsuario1ConVariosAmigosMixto
redUsuario1Con1Amigo = redUsuario1Con1Amigo1C

testSuite3 = test [
    -- a. La red no tiene relaciones
    "cantidadDeAmigos red sin relaciones" ~: cantidadDeAmigos redSinRelaciones usuario1 ~?= 0, 
    -- b. La red tiene relaciones y el usuario no tiene amigos:
    "cantidadDeAmigos red con relaciones y usuario sin amigos" ~: cantidadDeAmigos redConRelacionesU1SinAmigos usuario1 ~?= 0,
    -- c. La red tiene relaciones y el usuario tiene 1 amigo:
    "cantidadDeAmigos red con relaciones y usuario con 1 amigo" ~: cantidadDeAmigos redUsuario1Con1Amigo usuario1 ~?= 1,
    -- d. La red tiene relaciones y el usuario tiene mas de 1 amigo:
    "cantidadDeAmigos red con relaciones y usuario con mas de 1 amigo" ~: cantidadDeAmigos redConRelacionesU1VariosAmigos usuario1 ~?= 2
    ]

-- Ej 4: usuarioConMasAmigos
redUsuario1ConMasAmigos = ([usuario1, usuario2, usuario3], [(usuario1, usuario2), (usuario1, usuario3)], []) 
redHayMasDe1MaximoAmigos = ([usuario1, usuario2, usuario3, usuario4], [(usuario1, usuario2), (usuario1, usuario3), (usuario2, usuario3)], []) 
redCon1UsuarioSinRelaciones = redCon1Usuario
redConVariosUsuarioSinRelaciones = redConMasde1Usuario

testSuite4 = test [
    -- a. La red tiene 1 solo usuario
    "usuarioConMasAmigos red con 1 usuario" ~: usuarioConMasAmigos redCon1Usuario ~?= usuario1,
    -- b. La red tiene mas de 1 usuario, pero no tiene relaciones
    "usuarioConMasAmigos red con mas de 1 usuario, sin relaciones" ~: expectAny (usuarioConMasAmigos redConVariosUsuarioSinRelaciones) (usuarios redConVariosUsuarioSinRelaciones),
    -- c. La red tiene mas de 1 usuario y hay un unico usuario con mas amigos
    "usuarioConMasAmigos red con mas de 1, hay un solo maximo" ~: usuarioConMasAmigos redUsuario1ConMasAmigos ~?= usuario1,
    -- d. La red tiene mas de 1 usuario y hay mas de un usuario con la max cantidad de amigos
    "usuarioConMasAmigos red con mas de 1, hay mas de un maximo" ~: expectAny (usuarioConMasAmigos redHayMasDe1MaximoAmigos) [usuario1,usuario2,usuario3]
 ]

-- Ej 5: estaRobertoCarlos
usuariosMasDe10 = [usuario1,usuario2,usuario3,usuario4,usuario5,usuario6,usuario7,usuario8,usuario9,usuario10,usuario11,usuario12]
redNoHayUnUsuarioCon11Amigos = ([usuario1,usuario2,usuario3,usuario4],[],[])
redHayUnUsuarioCon11Amigos = (usuariosMasDe10,
                              [(usuario1,usuario2),(usuario1,usuario3),(usuario1,usuario4),(usuario1,usuario5),
                              (usuario1,usuario6),(usuario1,usuario7),(usuario1,usuario8),(usuario1,usuario9),
                              (usuario1,usuario10),(usuario1,usuario11),(usuario1,usuario12)],
                              [])

redHayVariosUsuariosCon11Amigos = ([usuario1,usuario2,usuario3,usuario4],
                                    [(usuario1,usuario2),(usuario1,usuario3),(usuario1,usuario4),(usuario1,usuario5),
                                    (usuario1,usuario6),(usuario1,usuario7),(usuario1,usuario8),(usuario1,usuario9),
                                    (usuario1,usuario10),(usuario1,usuario11),(usuario1, usuario12), (usuario2,usuario3),
                                    (usuario2,usuario4),(usuario2,usuario5),(usuario2,usuario6),(usuario2,usuario7),
                                    (usuario2,usuario8),(usuario2,usuario9),(usuario2,usuario10),(usuario2,usuario11),
                                    (usuario2,usuario12)],
                                    [])

testSuite5 = test [
    -- a. La red no tiene un Roberto Carlos
    "estaRobertoCarlos (n=10), no esta" ~: estaRobertoCarlos redNoHayUnUsuarioCon11Amigos ~?= False,
    -- b. La red tiene un Roberto Carlos
    "estaRobertoCarlos (n=10), hay uno" ~: estaRobertoCarlos redHayUnUsuarioCon11Amigos ~?= True,
    -- c. La red tiene mas de un Roberto Carlos
    "estaRobertoCarlos (n=10), hay mas de uno" ~: estaRobertoCarlos redHayVariosUsuariosCon11Amigos ~?= True
    ]   

-- Ej 6: publicacionesDe
redSinPublicaciones = redCon1Usuario
redConPublicacionesUsuario1SinPost = ([usuario1], [], [publicacion2_1])
redCon1UsuarioCon1Post = ([usuario1], [], [publicacion1_1])
redCon1UsuarioConVariosPost = ([usuario1], [], [publicacion1_1, publicacion1_2])

testSuite6 = test [
    -- a. La red no tiene publicaciones
    "publicacionesDe red sin publicaciones" ~: publicacionesDe redSinPublicaciones usuario1 ~?= [],
    -- b. PublicacionesDe red con publicaciones, usuario sin posts
    "publicacionesDe red con publicaciones  usuario sin posts" ~: publicacionesDe redConPublicacionesUsuario1SinPost usuario1 ~?= [],
    -- c. PublicacionesDe red con publicaciones usuario con un post 
    "publicacionesDe red con publicaciones usuario con 1 post" ~: publicacionesDe redCon1UsuarioCon1Post usuario1 ~?= [publicacion1_1],
    -- d. PublicacionesDe red con publicaciones usuario con mas de un post 
    "publicacionesDe red con publicaciones usuario con mas de 1 post" ~: expectAny (publicacionesDe redCon1UsuarioConVariosPost usuario1) [[publicacion1_1, publicacion1_2], [publicacion1_2, publicacion1_1]] 
    ]

-- Ej 7: publicacionesQueLeGustanA
publicacionB = (usuario1, "I am Alice. Not", [usuario1, usuario2])
publicacionA = (usuario1, "I am Bob. Not", [usuario1])

redUsuario1SinLikes = ([usuario1, usuario2, usuario3], [], [publicacion3_2])
redUsuario1Con1Like = ([usuario1, usuario2], [], [publicacionA])
redUsuario1ConVariosLikes = ([usuario1, usuario2], [], [publicacionA, publicacionB])

testSuite7 = test [
    -- a. La red no tiene publicaciones 
    "publicacionesQueLeGustanA red sin publicaciones" ~: publicacionesQueLeGustanA redSinPublicaciones usuario1 ~?= [],
    -- b. El usuario no likeo ningun post
    "publicacionesQueLeGustanA red con publicaciones, us1 sin likes" ~: publicacionesQueLeGustanA redUsuario1SinLikes usuario1 ~?= [],
    -- c. El usuario likeo 1 post
    "publicacionesQueLeGustanA red con publicaciones, us1 con 1 like" ~: publicacionesQueLeGustanA redUsuario1Con1Like usuario1 ~?= [publicacionA],
    -- d. El usuario likeo mas de 1 post, resultado esperado = una lista con todas las publicaciones que gustaron al usuario (en cualquier orden)
    "publicacionesQueLeGustanA red con publicaciones, us1 con mas de 1 like" ~: expectAny (publicacionesQueLeGustanA redUsuario1ConVariosLikes usuario1) [[publicacionA,publicacionB], [publicacionB,publicacionA]]
    ]

-- Ej 8: lesGustanLasMismasPublicaciones
publicacion1 = (usuario1, "Este es mi primer post", [usuario1, usuario2])
publicacion1b = (usuario2, "Este es mi primer post", [usuario1, usuario2])
publicacion2 = (usuario1, "Este es mi segundo post", [usuario2])
publicacion3 = (usuario1, "Tercer post", [])

redMismoLikeU1U2 = ([usuario1, usuario2], [], [publicacion1])
redMismosLikesU1U2 = ([usuario1, usuario2], [], [publicacion1, publicacion1b])
redDistintosLikesU1U2 = ([usuario1, usuario2], [], [publicacion1, publicacion2])
redSinLikesU1U2 = ([usuario1, usuario2], [], [publicacion3])

testSuite8 = test [
    -- a. A ambos usuarios le gustan las mismas publicaciones (1 sola)
    "lesGustanLasMismasPublicaciones likearon un post, likes iguales" ~: lesGustanLasMismasPublicaciones redMismoLikeU1U2 usuario1 usuario2 ~?= True, 
    -- b. A ambos usuarios le gustan las mismas publicaciones (mas de 1)
    "lesGustanLasMismasPublicaciones likearon mas de un post, likes iguales" ~: lesGustanLasMismasPublicaciones redMismosLikesU1U2 usuario1 usuario2 ~?= True, 
    -- c. A los usuarios no les gustan las mismas publicaciones
    "lesGustanLasMismasPublicaciones distintos likes" ~: lesGustanLasMismasPublicaciones redDistintosLikesU1U2 usuario1 usuario2 ~?= False, 
    -- d. No hay publicaciones
    "lesGustanLasMismasPublicaciones no hay publicaciones" ~: lesGustanLasMismasPublicaciones redSinLikesU1U2 usuario1 usuario2 ~?= True 
    ]

-- Ej 9: tieneUnSeguidorFiel
publicacion1_23 = (usuario1, "a", [usuario2,usuario3])

redUsuario1SinSeguidorFiel = ([usuario1,usuario2],[],[publicacion1,publicacion3])
redUsuario1ConSeguidorFiel = redMismosLikesU1U2
redUsuario1ConSeguidoresFieles = ([usuario1,usuario2,usuario3],[],[publicacion1_23])

testSuite9 = test [
    -- a. Usuario sin publicaciones
    "tieneUnSeguidorFiel red sin publicaciones" ~: tieneUnSeguidorFiel redSinPublicaciones usuario1 ~?= False,
    -- b. Usuario con publicaciones, sin seguidor fiel
    "tieneUnSeguidorFiel red con publicaciones, usuario1 sin seguidor fiel" ~: tieneUnSeguidorFiel redUsuario1SinSeguidorFiel usuario1 ~?= False,
    --c. Usuario con publicaciones, con seguidor fiel 
    "tieneUnSeguidorFiel red con publicaciones, usuario1 con seguidor fiel" ~: tieneUnSeguidorFiel redUsuario1ConSeguidorFiel usuario1 ~?= True,
    --c. Usuario con publicaciones, con mas de 1 seguidor fiel 
    "tieneUnSeguidorFiel red con publicaciones, usuario1 con seguidor fiel" ~: tieneUnSeguidorFiel redUsuario1ConSeguidoresFieles usuario1 ~?= True
    ]

-- Ej 10: existeSecuenciaDeAmigos
redSinRelacionesU1U2 = ([usuario1,usuario2],[],[])
redU1U2Amigos = ([usuario1,usuario2],[(usuario1,usuario2)],[])
redU1U4RelacionIndirecta = ([usuario1,usuario2,usuario3,usuario4],[(usuario1,usuario2),(usuario2,usuario3),(usuario3,usuario4)],[])
redU1U2distintaParticion = ([usuario1,usuario2,usuario3,usuario4],[(usuario1,usuario3),(usuario2,usuario4)],[])
redU1U5RelacionIndirecta2daRama = ([usuario1,usuario2,usuario3,usuario4,usuario5],[(usuario1,usuario2),(usuario1,usuario4),(usuario2,usuario3),(usuario4,usuario5)],[])

testSuite10 = test [
    -- a. Red sin relaciones
    "existeSecuenciaDeAmigos red sin relaciones" ~: existeSecuenciaDeAmigos redSinRelacionesU1U2 usuario1 usuario2 ~?= False, 
    -- b. Con relaciones, ambos usuarios en la misma particion, relacionados directamente
    "existeSecuenciaDeAmigos red con relaciones, user1 user2 amigos" ~: existeSecuenciaDeAmigos redU1U2Amigos usuario1 usuario2 ~?= True,
    -- c. Con relaciones, ambos usuarios en la misma particion, relacionados indirectamente
    "existeSecuenciaDeAmigos red con relaciones, user1 user2 relacionados indirectamente" ~: existeSecuenciaDeAmigos redU1U4RelacionIndirecta usuario1 usuario4 ~?= True,
    -- d. Con relaciones, usuarios en distinta particion
   "existeSecuenciaDeAmigos red con relaciones, user1 user2 en distinta particion" ~: existeSecuenciaDeAmigos redU1U2distintaParticion usuario1 usuario2 ~?= False,
   -- e. Con relaciones, usuarios con relacion indirecta por segundo camino
   "existeSecuenciaDeAmigos red con relaciones, usuarios con relacion indirecta por segundo camino" ~: existeSecuenciaDeAmigos redU1U5RelacionIndirecta2daRama usuario1 usuario5 ~?= True
   ]
