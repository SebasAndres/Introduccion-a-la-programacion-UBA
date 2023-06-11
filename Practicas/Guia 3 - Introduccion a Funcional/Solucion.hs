-- Introduccion a la Programacion 
-- Guia 3 - Intro Haskell

-- [1] 
f :: Integer -> Integer
f 1 = 8
f 4 = 131
f 16 = 16

g :: Integer -> Integer
g 8 = 16
g 16 = 4
g 131 = 1

h :: Integer -> Integer
h x = f (g x)

k :: Integer -> Integer
k x = g (f x)

-- [2]
absoluto :: Integer -> Integer
absoluto x | x >= 0 = x
           | otherwise = -x

maximoabsoluto :: Integer -> Integer -> Integer
maximoabsoluto x y | absoluto x > absoluto y = absoluto x
                   | absoluto y > absoluto x = absoluto y
                   | otherwise = undefined 

maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 a b c = maximoabsoluto a (maximoabsoluto b c)

algunoEs0 :: Float -> Float -> Bool
algunoEs0 a b = (a == 0) || (b == 0)

mismoIntervalo :: Float -> Float -> Bool
mismoIntervalo x y | (x <= 3) && (y <= 3) = True
                   | (x > 3 && x <=7) && (y > 3 && y <=7) = True
                   | (x > 7) && (y > 7) = True
                   | otherwise = False

sumaDistintos :: Integer -> Integer -> Integer -> Integer
sumaDistintos x y z = sumaDistintos2 x (sumaDistintos2 y z)

sumaDistintos2 :: Integer -> Integer -> Integer
sumaDistintos2 x y | (x /= y) = x + y
                   | otherwise = x

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe a b = (mod a b == 0)

digitoUnidades :: Integer -> Integer
digitoUnidades x = mod x 10

digitoDecenas :: Integer -> Integer
digitoDecenas x = mod x 12

-- [3]
estanRelacionados :: Integer -> Integer -> Bool
estanRelacionados a b | (a == 0) || (b == 0) = undefined
                      | esMultiploDe a b = True -- <=> esInteger(k=-a/b) 
                      | otherwise = False

-- [4]
prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt (w,x) (y,z) = w*y + x*z

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (a,b) (c,d) = (a < c) && (b < d)

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (a,b) (c,d) = sqrt ((a-c)^2 - (b-d)^2)

sumaTerna :: (Integer, Integer, Integer) -> Integer
sumaTerna (a,b,c) = a + b + c

sumarSoloMultiplos :: (Integer, Integer, Integer) -> Integer -> Integer
sumarSoloMultiplos (a,b,c) m = sumarSoloMultiplos2Tupla (a,(sumarSoloMultiplos2Tupla (b,c) m)) m

sumarSoloMultiplos2Tupla :: (Integer, Integer) -> Integer -> Integer
sumarSoloMultiplos2Tupla (a,b) m | (mod a m == 0) && (mod b m == 0) = a+b
                                 | (mod a m == 0) && (mod b m /= 0) = a 
                                 | (mod a m /= 0) && (mod b m == 0) = b
                                 | otherwise = 0

posPrimerPar :: (Integer, Integer, Integer) -> Integer
posPrimerPar (a,b,c) | (mod a 2 == 0) = 1
                     | (mod b 2 == 0) = 2
                     | (mod c 2 == 0) = 3
                     | otherwise = 4

crearPar :: a -> b -> (a, b)
crearPar a b = (a, b)

invertir :: (a, b) -> (b, a)
invertir (a, b) = (b, a)

--[5]
todosMenores :: (Integer, Integer, Integer) -> Bool
todosMenores (a,b,c) | (ff a > gg a) && (ff b > gg b) && (ff c > gg c) = True
                     | otherwise = False

ff :: Integer -> Integer
ff n | n <= 7 = n^2
    | n > 7 = 2*n -1

gg :: Integer -> Integer
gg n | (mod n 2 == 0) = div n 2
     | otherwise = 3 * n + 1

--[6]
bisiesto :: Integer -> Bool
bisiesto y | (not (esMultiploDe y 4) || ((esMultiploDe y 100) && not (esMultiploDe y 400))) = False
           | otherwise = True

--[7]
distanciaManhattan :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distanciaManhattan (a,b,c) (d,e,f) = abs (a - d) + abs (b - e) + abs (c - f)

--[8]
comparar :: Integer -> Integer -> Integer
comparar a b | sumaUltimosDosDigitos a < sumaUltimosDosDigitos b = 1
             | sumaUltimosDosDigitos a > sumaUltimosDosDigitos b = -1
             | sumaUltimosDosDigitos a == sumaUltimosDosDigitos b = 0

sumaUltimosDosDigitos :: Integer -> Integer
sumaUltimosDosDigitos x = (mod x 10) + (mod (div x 10) (10))
