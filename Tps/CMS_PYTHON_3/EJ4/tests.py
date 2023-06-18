from queue import Queue

# El tipo de fila debería ser Queue[int], pero la versión de python del CMS no lo soporta. Usaremos en su lugar simplemente "Queue"
def avanzarFila(fila: Queue, min: int):
    
    # la idea es que estadoCajas[i] = # minutos hasta que se desocupa la iesima caja
    # tiempo[i] = tiempo_entre_clientes - 1 
    # se resta -1 para contar el minuto actual
    
    estadoCajas:dict = {
        "Caja1": {"minutos": 1, "tiempo": 10-1},
        "Caja2": {"minutos": 3, "tiempo": 4-1},
        "Caja3": {"minutos": 2, "tiempo": 4-1, "cliente": ""}
    }

    nuevo_cliente:int = fila.qsize() + 1

    for minuto in range(min+1):

        # actualizo estadoCajas y busco cajaDisponible
        cajasDisponibles:list = []

        for ckey in estadoCajas.keys():
            tiempo_restante:int = estadoCajas[ckey]["minutos"]

            if tiempo_restante > 0:
                estadoCajas[ckey]["minutos"] -= 1            

            # una caja que se libera al minuto n puede atender desde el minuto n+1
            elif tiempo_restante == 0: 
                # la primer caja en desocuparse en orden ascendente es ocupada
                cajasDisponibles.append(ckey)
                
            # el cliente en Caja3 tarda 3 minutos en volver a la fila,
            # pero Caja3 se libera recien en 4 minutos
            if ckey == "Caja3" and tiempo_restante == 1:
                cliente:int = estadoCajas[ckey]["cliente"]
                if cliente != "":  
                    fila.put(cliente)
            pass

        hayCajasDisponible:bool = (len(cajasDisponibles) > 0)

        # si hay una caja libre, desencolo de la fila
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

def test1():
    
    inicio:list = [1,2,3]
    expected:list = [
                     [1,2,3,4], # 0
                     [2,3,4], # 1
                     [3,4], # 2
                     [4], # 3
                     [4,5], # 4
                     [4,5,2] # 5
                    ]

    for min in range(len(expected)):

        fila:Queue = Queue()
        for n in inicio:
            fila.put(n)
        
        avanzarFila(fila, min)
        
        res:list = []
        for i in range(0, fila.qsize()):
            res.append(fila.get())
        
        if expected[min] != res:
            print(f"Test 1: min: {min} res: {res} expected: {expected[min]}")
            print()
            return False

    return True

def test2():
    
    inicio = []
    expected:list = [
                     [1], # 0 
                     [], # 1
                     [], # 2 
                     [], # 3
                     [2], # 4
                     [], # 5
                     [], # 6
                     [], # 7
                     [3], # 8
                     [], # 9
                     [], # 10
                     [], # 11
                     [4] # 12
                    ] 

    for min in range(len(expected)):

        fila:Queue = Queue()
        for n in inicio:
            fila.put(n)
        
        avanzarFila(fila, min)
        
        res:list = []
        for i in range(0, fila.qsize()):
            res.append(fila.get())
        
        if expected[min] != res:
            print(f"Test 2: min: {min} res: {res} expected: {expected[min]}")
            print()
            return False

    return True

def testSuitConsigna():
    print("Test1: ", test1())
    print("Test2:", test2())

testSuitConsigna()