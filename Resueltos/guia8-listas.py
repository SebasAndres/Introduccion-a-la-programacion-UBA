
def pertenece(seq:list, e:int) -> bool:
    for elem in seq:
        if elem == e:
            return True
    return False

def divideATodos(seq:list, n:int) -> bool:
    for elem in seq:
        if elem % n != 0:
            return False
    return True

def ordenados(seq:list) -> bool:
    longitud:int = len(seq)
    for i in range(longitud-1):
        if seq[i] > seq[i+1]:
            return False
    return True 

def esPalindromo(palabra:str) -> bool:
    # o implementar reverse(palabra)
    return palabra[::-1] == palabra

def reverse(palabra:str) -> str:
    reversed:str = ""
    longitud:int = len(palabra)
    for i in range(longitud):
        reversed += palabra[longitud-i]
    return reversed
    
def strenghtPassword(password:str) -> str:
    longitud = len(password)
    if longitud < 5:
        return "ROJO"
    elif longitud > 8:
        if tieneMayuscula(password) and \
           tieneMinuscula(password) and tieneDigNumerico(password):
            return "VERDE"
        else:
            return "AMARILLO"
        
def tieneMinuscula(palabra:str) -> bool:
    palabra_filtrada:str = eliminarDigNum(palabra)
    for char in palabra_filtrada:
        if "a" <= char <= "z":
            return True
    return False

def tieneMayuscula(palabra:str) -> bool:
    palabra_filtrada:str = eliminarDigNum(palabra)
    for char in palabra_filtrada:
        if "A" <= char <= "Z":
            return True
    return False

def tieneDigNumerico(password:str) -> bool:
    return not(password == eliminarDigNum(password))

def eliminarDigNum(palabra:str) -> str:
    nums:list = [str(i) for i in range(0,10)]
    cjt:set = set(palabra)-set(nums)
    return list(cjt)

def eliminarDigNum(palabra:str) -> str:
    nums:list = [str(i) for i in range(0,10)]
    rt:str = ""
    for k in palabra:
        if not (k in nums):
            rt += k
    return rt

def strenghtPasswordV2(password:str) -> str:
    longitud = len(password)
    if longitud < 5:
        return "ROJO"
    elif longitud > 8:
        cantidad_mayusculas:int = 0
        cantidad_minusculas:int = 0
        cantidad_numeros:int = 0
        for char in password:
            if "a" <= char <= "z":
                cantidad_minusculas += 1
            elif "A" <= char <= "Z":
                cantidad_mayusculas += 1
            elif "1" <= char <= "9":
                cantidad_numeros += 1
        
        if cantidad_mayusculas > 0 and \
           cantidad_minusculas > 0 and \
           cantidad_numeros > 0:
            return "VERDE"
        else:
            return "AMARILLO"    

def saldo(movimientos:list) -> int:
    saldo = 0
    for tipo, monto in movimientos:
        if tipo == 'I':
            saldo += monto
        else:
            saldo -= monto
    return saldo

def saldoRecursivo(movimientos:list) -> int:
    if len(movimientos) == 0:
        return 0
    else:
        tipo, monto = movimientos[0]
        fstmov = int(tipo=='I')*monto - int(tipo=='R')*monto
        return fstmov + saldoRecursivo(movimientos[1:])

"""
COMPARAR TIEMPOS FOR vs RECURSION

from timer import timer

with timer() as t:
    s = saldo([('I', 2000), ('R',20),('R', 1000),('I', 300)])
    t_saldo_for = t.elapse
with timer() as t:
    s = saldoRecursivo([('I', 2000), ('R',20),('R', 1000),('I', 300)])
    t_saldo_recursivo = t.elapse

if t_saldo_for < t_saldo_recursivo:
    print(":clock: for es mas rapido que recursion en este caso")
elif t_saldo_for == t_saldo_recursivo:
    print(":clock: tardan igual")
else:
    print(":clock: recursion es más rapida")
"""

def palabraTieneAlMenos3VocalesDistintas(palabra:str) -> bool:
    return cantidadVocalesDistintas(palabra) >= 3

def cantidadVocalesDistintas(palabra:str) -> int:
    vocales:list = ['a', 'e', 'i', 'o', 'u']
    for chr in palabra:
        if chr.lower() in vocales:
            vocales.remove(chr)
    return 5 - len(vocales)
