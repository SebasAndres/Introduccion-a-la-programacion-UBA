from typing import List
from typing import Dict
import json

def unir_diccionarios(a_unir: List[Dict[str,str]]) -> Dict[str,List[str]]:
  
  out:dict = dict()

  for d in a_unir:
    for key, value in d.items():
        if key not in out.keys():
            out[key] = [value]
        else:
            out[key].append(value)

  return out

if __name__ == '__main__':
  x = json.loads(input()) # Ejemplo de input: [{"a":2},{"b":3,"a":1}]
  print(unir_diccionarios(x))