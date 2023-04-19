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
{-
esDivisible :: Integer -> Integer -> Bool
esDivisible a b = (parteEntera (a / b) == div a b)
-}

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

cantidadDigitos :: Integer -> Integer
cantidadDigitos n | n < 10 = 1
                  | otherwise = 1 + cantidadDigitos (div n 10)

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
iesimoDigito n i | (cantidadDigitos n == i) = ultimoDigito (n)
                 | otherwise = iesimoDigito (sacarUltimo n) i
                 where sacarUltimo n = div n 10
                       ultimoDigito n = mod n 10 
--[9]
esCapicua :: Integer -> Bool
esCapicua n | (n < 10) = True
            | (invertir (n) == n) = True
            | otherwise = False

invertir :: Integer -> Integer
invertir n | (cantidadDigitos (n) == 1) = n
           | otherwise = (ultimoDigito (n) * 10^(cantidadDigitos(n)-1)) + invertir (sacarUltimo (n))
           where ultimoDigito n = mod n 10
                 sacarUltimo n = div n 10

--[10]
f1 :: Integer -> Integer
f1 0 = 1
f1 n = f1 (n-1) + 2^n

f2 :: Integer -> Integer -> Integer
f2 0 _ = 0
f2 _ 0 = 0
f2 n q = f2 (n-1) q + q^n 

f3 :: Integer -> Integer -> Integer
f3 0 _ = 0
f3 _ 0 = 0
f3 n q = f2 (2*n) q

f4 :: Integer -> Integer -> Integer
f4 n q = f3 n q - f2 n q

--[11]
eAprox :: Integer -> Float 
eAprox 0 = 1
eAprox n = eAprox (n-1) + 1.0 / fromInteger (factorial (n))

e = eAprox 10

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = factorial (n-1) * n

--[12]
raizDe2Aprox :: Integer -> Float
raizDe2Aprox n = a (n) - 1

a :: Integer -> Float
a 1 = 2
a n = 2 + 1 / a (n-1)

--[13]
f :: Integer -> Integer -> Integer 
f 1 m = m
f n 1 = div (n * (n+1)) 2
f n m = (f (n-1) m) + n^m

--[14]
sumaPotencias :: Integer -> Integer -> Integer -> Float
sumaPotencias q m n = fromInteger ((f2 m q) * (f2 n q))

--[15]
{-
sumaRacionales :: Integer -> Integer -> Float
sumaRacionales n m = (n*(n+1)/2) * suma_fracc m 

suma_fracc :: Integer -> Float
suma_fracc 1 = 1
suma_fracc m = suma_fracc (m-1) + 1/m
-}

--[16]
--(A) 
menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | (mod n k == 0) = k
                      | otherwise = menorDivisorDesde n (k+1)

--(B)
esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = (menorDivisor n == n)

--(C)
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b = (mcd a b == 1)

mcd :: Integer -> Integer -> Integer
mcd x 0 = x
mcd a b = mcd b (mod a b)

--(D)
nEsimoPrimo :: Integer -> Integer
nEsimoPrimo 1 = 2
nEsimoPrimo k = proximoPrimoDesde (nEsimoPrimo (k-1)) 

proximoPrimoDesde :: Integer -> Integer 
proximoPrimoDesde n | esPrimo (n+1) = n + 1
                    | otherwise = proximoPrimoDesde (n+1)

--[17]
esFibonacci :: Integer -> Bool
esFibonacci n = esFibonacciDesde n 1

esFibonacciDesde :: Integer -> Integer -> Bool
esFibonacciDesde n k | (fibonacci(k) == n) = True           
                     | (fibonacci(k) > n) = False
                     | (fibonacci(k) < n) = esFibonacciDesde n (k+1)

--[18]
mayorDigitoPar :: Integer -> Integer
mayorDigitoPar n | (n < 10) && (mod n 2 /= 0) = -1
                 | (n < 10) && (mod n 2 == 0) = n
                 | (ultimoDigito n >= mayorDigitoPar (sacarUltimo n)) && (mod (ultimoDigito n) 2 == 0) = ultimoDigito n 
                 | (ultimoDigito n < mayorDigitoPar (sacarUltimo n)) = mayorDigitoPar (sacarUltimo n)
                 | otherwise = mayorDigitoPar (sacarUltimo n)
                 where sacarUltimo n = div n 10

--[19]
esSumaInicialDePrimos :: Integer -> Bool
esSumaInicialDePrimos n = esSumaInicialDePrimosDesde n 1

esSumaInicialDePrimosDesde :: Integer -> Integer -> Bool
esSumaInicialDePrimosDesde n i | sumaKprimos (i) > n = False
                               | sumaKprimos (i) < n = esSumaInicialDePrimosDesde n (i+1)
                               | otherwise = True

sumaKprimos :: Integer -> Integer
sumaKprimos 1 = 2
sumaKprimos n = sumaKprimos (n-1) + nEsimoPrimo n

--[20]
tomaValorMax :: Integer -> Integer -> Integer
tomaValorMax n m | (n == m) = sumaDivisores(n)
                 | otherwise = max (sumaDivisores (n)) (tomaValorMax (n+1) m)

sumaDivisores :: Integer -> Integer
sumaDivisores n = sumaDivisoresDesde n 1

sumaDivisoresDesde :: Integer -> Integer -> Integer
sumaDivisoresDesde n k | (k == n) = n 
                       | (k < n) && (mod n k == 0) = k + sumaDivisoresDesde n (k+1)
                       | (k > n) = 0
                       | otherwise = sumaDivisoresDesde n (k+1)
--[21]
pitagoras :: Integer -> Integer -> Integer -> Integer
pitagoras 0 n r = cantidadXmenorY n (r^2)   
pitagoras m 0 r = cantidadXmenorY m (r^2) 
pitagoras m n r = pitagoras (m-1) n r + cantidadXmenorY n (r^2 - m^2)

cantidadXmenorY :: Integer -> Integer -> Integer
cantidadXmenorY x y = cantidadXmenorYDesde x y 0

cantidadXmenorYDesde :: Integer -> Integer -> Integer -> Integer
cantidadXmenorYDesde x y k | (k < x) && (k^2 <= y) = 1 + cantidadXmenorYDesde x y (k+1)
                           | (k < x) = cantidadXmenorYDesde x y (k+1)
                           | (k == x) && (k^2 <= y) = 1
                           | (k == x) = 0 