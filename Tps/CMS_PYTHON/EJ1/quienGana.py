import sys

def quienGana(j1: str, j2: str) -> str : 
  
  tuplasGanaEl1ro:list = [("Piedra", "Tijera"),("Tijera", "Papel"),("Papel", "Piedra")]

  if j1 == j2:
    return "Empate"  
  elif (j1,j2) in tuplasGanaEl1ro:
    return "Jugador1"  
  else:
    return "Jugador2"

if __name__ == '__main__':
  x = input()
  jug = str.split(x)
  print(quienGana(jug[0], jug[1]))