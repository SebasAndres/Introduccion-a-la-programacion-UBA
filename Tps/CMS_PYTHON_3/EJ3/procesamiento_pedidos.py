from queue import Queue
from typing import List
from typing import Dict
from typing import Union
import json

# ACLARACIÓN: El tipo de "pedidos" debería ser: pedidos: Queue[Dict[str, Union[int, str, Dict[str, int]]]]
# Por no ser soportado por la versión de CMS, usamos simplemente "pedidos: Queue"
def procesamiento_pedidos(pedidos: Queue,
                          stock_productos: Dict[str, int],
                          precios_productos: Dict[str, float]) -> List[Dict[str, Union[int, str, float, Dict[str, int]]]]:
  
    # lista con los pedidos procesados
    res = []

    # proceso los pedidos de a uno
    while (not pedidos.empty()):

        # leo el pedido      
        pedido:dict = pedidos.get()

        id = pedido['id']
        cliente = pedido['cliente']
        productos = pedido['productos'].copy()

        # veo si hay stock, 
        # en tal caso, sumo el precio por la cantidad de productos comprados
        # y descuento del stock

        precio_total:float = 0
        estado:str = "completo" # estado del pedido

        for producto, cantidad in productos.items():

            precio_producto:float = precios_productos[producto]
            cantidad_disponible:int = stock_productos[producto]

            if cantidad_disponible >= cantidad:
                stock_productos[producto] -= cantidad
                precio_total += cantidad * precio_producto
            else:
                estado = "incompleto"       
                stock_productos[producto] = 0
                precio_total += cantidad_disponible * precio_producto
                
                # solo voy a devolver los que habia en stock 
                productos[producto] = cantidad_disponible 

            pass
        
        # armo el pedido procesado
        pedido_procesado:dict() = {
            'id': id,
            'cliente': cliente, 
            'productos': productos,
            'precio_total': precio_total,
            'estado': estado
        }
        res.append(pedido_procesado)

        pass
    
    return res

if __name__ == '__main__':
  pedidos: Queue = Queue()
  list_pedidos = json.loads(input())
  [pedidos.put(p) for p in list_pedidos]
  stock_productos = json.loads(input())
  precios_productos = json.loads(input())
  print("{} {}".format(procesamiento_pedidos(pedidos, stock_productos, precios_productos), stock_productos))

# Ejemplo input  
# pedidos: [{"id":21,"cliente":"Gabriela", "productos":{"Manzana":2}}, {"id":1,"cliente":"Juan","productos":{"Manzana":2,"Pan":4,"Factura":6}}]
# stock_productos: {"Manzana":10, "Leche":5, "Pan":3, "Factura":0}
# precios_productos: {"Manzana":3.5, "Leche":5.5, "Pan":3.5, "Factura":5}