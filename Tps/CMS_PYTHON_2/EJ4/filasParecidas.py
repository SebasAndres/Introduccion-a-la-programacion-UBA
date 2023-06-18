from typing import List

# Aclaración: Debido a la versión de Python del CMS, para el tipo Lista, la sintaxis de la definición de tipos que deben usar es la siguiente:
# l: List[int]  <--Este es un ejemplo para una lista de enteros.
# Respetar esta sintaxis, ya que el CMS dirá que no pasó ningún test si usan otra notación.

def filasParecidas(matriz: List[List[int]]) -> bool :

    primeraFila:List[int] = matriz[0]
    n_coordenadas = len(primeraFila)

    if len(matriz) == 1:
      return True

    segundaFila:List[int] = matriz[1]
    dif_inicial:int = segundaFila[0] - primeraFila[0]

    filaAnterior = primeraFila
    for fila in matriz[1:]:
        # armo un conjunto con las diferencias en cada indice
        diferencias:List[int] = []
        
        for i in range(n_coordenadas):
            diferencias.append(fila[i] - filaAnterior[i])
        
        if len(set(diferencias)) > 1 or diferencias[0] != dif_inicial: 
            # todas las diferencias tienen que ser iguales filaActual[i] = filaAnterior[i] + dif_inicial
            return False     

        filaAnterior = fila   

    return True

if __name__ == '__main__':
  filas = int(input())
  columnas = int(input())
 
  matriz = []
 
  for i in range(filas):         
    fila = input()
    if len(fila.split()) != columnas:
      print("Fila " + str(i) + " no contiene la cantidad adecuada de columnas")
    matriz.append([int(j) for j in fila.split()])
  
  print(filasParecidas(matriz))