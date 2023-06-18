from typing import List

# Aclaración: Debido a la versión de Python del CMS, para el tipo Lista, la sintaxis de la definición de tipos que deben usar es la siguiente:
# l: List[int]  <--Este es un ejemplo para una lista de enteros.
# Respetar esta sintaxis, ya que el CMS dirá que no pasó ningún test si usan otra notación.

def mesetaMasLarga(l: List[int]) -> int :
  # caso base
  if len(l) <= 1:
    return len(l)

  # armo la primer meseta (lista de alturas iguales contiguas)
  altura_inicial:int = l[0]
  meseta0:List[int] = [altura_inicial]

  for h in l[1:]:
    if altura_inicial == h:
      meseta0.append(h)
    else:
      break
  
  # agarro la meseta mas larga de la parte restante de la lista
  mesetaMasLargaTail:int = mesetaMasLarga(l[len(meseta0):]) 

  # elijo la meseta de mayor tamaño
  if len(meseta0) > mesetaMasLargaTail:
    return len(meseta0)
  else:
    return mesetaMasLargaTail
  
if __name__ == '__main__':
  x = input()
  print(mesetaMasLarga([int(j) for j in x.split()]))