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

-- particion 1
u1 = (1,"alice")
a = (2, "juan")
b = (3, "sebas")
c = (4, "messi")
d = (5, "teo")
e = (6, "monica")
u2 = (7, "bob")

-- particion 2
f = (8, "f")
g = (9, "g")
h = (10, "h")

relaciones = [(u1,a),(u1,b),(u1,c),(a,d),(b,e),(e,u2),(f,g),(g,h)]
usuarios = [u1,a,b,c,d,e,f,g,u2]
posts = []
redF = (usuarios, relaciones, posts)

existeSecuenciaDeAmigos redF f e 