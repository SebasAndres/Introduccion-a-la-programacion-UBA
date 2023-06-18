from typing import List
from typing import Tuple

# Aclaración: Debido a la versión de Python del CMS, para el tipo Lista y Tupla, la sintaxis de la definición de tipos que deben usar es la siguiente:
# l: List[int]  <--Este es un ejemplo para una lista de enteros.
# t: Tuple[str,str]  <--Este es un ejemplo para una tupla de strings.
# Respetar esta sintaxis, ya que el CMS dirá que no pasó ningún test si usan otra notación.

def sePuedeLlegar(origen: str, destino: str, vuelos: List[Tuple[str, str]]) -> int :
  
  # si hay un vuelo directo
  if (origen, destino) in vuelos:
    return 1
  
  # no hay vuelos desde esas ciudades
  if noTieneVuelos(origen, vuelos) or noLleganVuelos(destino, vuelos):
    return -1  

  # recorro el camino de vuelos desde el origen
  # aprovecho que solo hay un vuelo por ciudad 
  
  vueloInicial:Tuple[str,str] = vueloDesde(origen, vuelos)  
  recorrido:List[Tuple[str,str]] = [vueloInicial]
  
  noLlego:bool = True
  noRepite:bool = True
  
  ciudadActual:str = destinoDe(vueloInicial)

  while (noLlego and noRepite):
    vuelo:Tuple[str,str] = vueloDesde(ciudadActual, vuelos)    
    ciudadActual = destinoDe(vuelo)
    
    if ciudadActual == destino:
      noLlego = False
      recorrido.append(vuelo)
    else:
      if vuelo in recorrido:
        noRepite = False
      else:
        recorrido.append(vuelo)

  if not noRepite:
    return -1

  return len(recorrido)

def noTieneVuelos(origen:str, vuelos:List[Tuple[str,str]]) -> bool:
  for vuelo in vuelos:
    if origenDe(vuelo) == origen:
      return False
  return True

def noLleganVuelos(destino:str, vuelos:List[Tuple[str,str]]) -> bool:
  for vuelo in vuelos:
    if destinoDe(vuelo) == destino:
      return False
  return True

def origenDe(vuelo:Tuple[str,str]) -> str:
  return vuelo[0]

def destinoDe(vuelo:Tuple[str,str]) -> str:
  return vuelo[1]

def vueloDesde(origen:str,vuelos:List[Tuple[str,str]]) -> Tuple[str,str]:
  # valido para origen en vuelos
  for vuelo in vuelos:
    if origenDe(vuelo) == origen:
      return vuelo

if __name__ == '__main__':
  origen = input()
  destino = input()
  vuelos = input()
  
  print(sePuedeLlegar(origen, destino, [tuple(vuelo.split(',')) for vuelo in vuelos.split()]))