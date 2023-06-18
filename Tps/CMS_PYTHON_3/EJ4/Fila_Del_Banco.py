from queue import Queue

# El tipo de fila debería ser Queue[int], pero la versión de python del CMS no lo soporta. 
# Usaremos en su lugar simplemente "Queue"

def avanzarFila(fila: Queue, min: int):
    
    # La idea es tener una diccionario de estados de las cajas,
    # donde las claves son los nombres de las cajas y cada estado es un diccionario con:
    # estadoCajas["Caja i"]: 
    # -> minutos = minutos restantes para que la i-esima caja se libere
    # -> tiempo = minutos entre clientes de la i-esima caja - 1 

    # Obs: en tiempo se resta 1 para contar el minuto actual
    
    estadoCajas:dict = {
        "Caja1": {"minutos": 1, "tiempo": 10-1},
        "Caja2": {"minutos": 3, "tiempo": 4-1},
        "Caja3": {"minutos": 2, "tiempo": 4-1, "cliente": ""}
    }

    # id del proximo cliente
    nuevo_cliente:int = fila.qsize() + 1

    # evoluciono los estados minuto a minuto
    for minuto in range(min+1):

        # actualizo estadoCajas, los minutos restantes para liberarse
        # ademas, busco cajas disponibles
        cajasDisponibles:list = []

        for ckey in estadoCajas.keys():
            tiempo_restante:int = estadoCajas[ckey]["minutos"]

            if tiempo_restante > 0:
                estadoCajas[ckey]["minutos"] -= 1            

            elif tiempo_restante == 0: 
                # una caja con tiempo == 0 está disponible
                # ademas este mecanismo respeta el orden de prioridad
                # c1 >> c2 >> c3, para asignar a un cliente
                cajasDisponibles.append(ckey)
                
            # el cliente en Caja3 tarda 3 minutos en volver a la fila,
            # pero Caja3 se libera recien en 4 minutos
            if ckey == "Caja3" and tiempo_restante == 1:
                cliente:int = estadoCajas[ckey]["cliente"]
                if cliente != "":  
                    fila.put(cliente)
            pass

        hayCajasDisponible:bool = (len(cajasDisponibles) > 0)

        # si hay cajas libres, desencolo de la fila en caso que esta no este vacia
        if hayCajasDisponible: 

            for cajaAsignada in cajasDisponibles:              

                if fila.empty():
                    break
                  
                cliente:int = fila.get()
                estadoCajas[cajaAsignada]["minutos"] += estadoCajas[cajaAsignada]["tiempo"]   

                if cajaAsignada == "Caja3":
                    estadoCajas[cajaAsignada]["cliente"] = cliente 

            pass

        # agrego una persona a la fila
        # por los tests de la consigna entiendo que esto va al final,
        # es decir acá
        if minuto % 4 == 0:
            fila.put(nuevo_cliente)
            nuevo_cliente += 1

    pass


if __name__ == '__main__':
  fila: Queue = Queue()
  fila_inicial: int = int(input())
  for numero in range(1, fila_inicial+1):
    fila.put(numero)
  min: int = int(input())
  avanzarFila(fila, min)
  res = []
  for i in range(0, fila.qsize()):
    res.append(fila.get())
  print(res)


# Caja1: Empieza a atender 10:01, y atiende a una persona cada 10 minutos
# Caja2: Empieza a atender 10:03, atiende a una persona cada 4 minutos
# Caja3: Empieza a atender 10:02, y atiende una persona cada 4 minutos, pero no le resuelve el problema y la persona debe volver a la fila (se va al final y tarda 3 min en llegar. Es decir, la persona que fue atendida 10:02 vuelve a entrar a la fila a las 10:05)
# La fila empieza con las n personas que llegaron antes de que abra el banco. Cuando abre (a las 10), cada 4 minutos llega una nueva persona a la fila (la primera entra a las 10:00)

