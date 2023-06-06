import sys

def fibonacciNoRecursivo(n: int) -> int:
    # devuelve el n-esimo elemento en la seq fibonacci   
    if n == 0:
        return 0
    
    referencia:int = 0
    ultimo:int = 1
    penultimo:int = 0

    for _ in range(0,n-2):
        referencia = ultimo + penultimo
        penultimo = ultimo
        ultimo = referencia

    return ultimo + penultimo

if __name__ == '__main__':
  x = int(input())  
  print(fibonacciNoRecursivo(x))

