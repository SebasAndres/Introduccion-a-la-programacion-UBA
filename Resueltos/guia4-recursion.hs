-- [1]
fibonacci :: Integer -> Integer
fibonacci 0 = 1
fibonacci 1 = 1 
fibonacci n = fibonacci(n-2) + fibonacci(n-1)

-- [2]
parteEnteraPositiva :: Float -> Integer
parteEnteraPositiva n | (0 <= n) && (n < 1) = 0
                      | otherwise = 1 + parteEnteraPositiva(n-1)

parteEnteraNegativa :: Float -> Integer
parteEnteraNegativa n | (-1 <= n) && (n < 0) = -1
                      | otherwise = -1 - parteEnteraNegativa (n+1) 

parteEntera :: Float -> Integer
parteEntera n | (n >= 0) = parteEnteraPositiva n 
              | otherwise = parteEnteraNegativa n

-- [3]
-- esDivisible :: Integer -> Integer -> Bool
-- esDivisible a b = (parteEntera (a / b) == div a b)

-- [4]
sumaImpares :: Integer -> Integer
sumaImpares 1 = 1
sumaImpares n = sumaImpares (n-1) + impar (n)

impar :: Integer -> Integer
impar 1 = 1
impar n = impar (n-1) + 2

--[5]
medioFact :: Integer -> Integer
medioFact 0 = 0
medioFact 1 = 1
medioFact 2 = 2
medioFact n = n * medioFact (n-2)

--[6]
sumaDigitos :: Integer -> Integer
sumaDigitos n | (cantidadDigitos n == 1) = n
              | otherwise = ultimoDigito n + sumaDigitos (sacarUltimo n)
              where sacarUltimo n = div n 10

--[7] (copiar el mas clean_code de la guia resuelta)
todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n | abs(n) < 10 = True
                      | (ultimoDigito n == segundoUltimoDigito n) = todosDigitosIguales (div n 10)
                      | otherwise = False

ultimoDigito :: Integer -> Integer
ultimoDigito n | n < 10 = n     
               | otherwise = mod n 10 

segundoUltimoDigito :: Integer -> Integer
segundoUltimoDigito n = mod (div n 10) 10

--[8]
iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i | cantidadDigitos n == i = ultimoDigito (n)
                 | otherwise = iesimoDigito (sacarUltimo n) i
                 where sacarUltimo n = div n 10
                       ultimoDigito n = mod n 10 

cantidadDigitos :: Integer -> Integer
cantidadDigitos n | n < 10 = 1
                  | otherwise = 1 + cantidadDigitos (div n 10)

--[9]
esCapicua :: Integer -> Bool
esCapicua n | (cantidadDigitos n == 1) = True
            | (ultimoDigito n == primerDigito n) = esCapicua (sacarExtermos n) 
            | otherwise = 


            | ( n < 10) = True
            | invertir(n) == n = True
            