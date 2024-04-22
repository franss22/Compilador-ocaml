# Entrega 5
## Francisco Ortiz
## Gabriel Orrego


## Garbage collection:

Para hacer el garbage collection, usamos el algoritimo de cheney, con un par de cambios.
* Revisamos todo el stack, desde stack_bottom hasta RSP
* En vez de hacer 2 pasadas copiando objetos, una para las raíces, y otra para los objetos linkeados desde las raíces, hacemos una sola pasada por todos los objetos del stack, y copiamos recursivamente todos sus subelementos.

En un poco mas de detalle, el algoritmo de GC consiste en:
* Al inicio del programa guardamos el primer RBP en STACK_BOTTOM.
* Cada vez que vamos a guardar una tupla o una closure, llamamos try_gc para ver si hay espacio suficiente en el heap.
* Si no hay suficiente espacio, llamamos collect.
* collect revisa cada valor del stack, desde STACK_BOTTOM hasta RSP, y intenta copiar todos los valores.
* Si el valor no era un puntero a un objeto, simplemente se mantiene igual en el stack (se sobreescribe con el mismo valor).
* Si el valor era un puntero a un objeto, vemos si es una tupla o una closure, y la copiamos. 
* Al copiar un objeto, tomamos sus datos desde FROM_SPACE, y los copiamos recursivamente (es decir que si es un objeto se repite el proceso) en TO_SPACE. Retornamos el puntero a la nueva dirección en el heap.
* Al terminar de copiar un objeto, se deja una dirección de dorwarding con un tag especial, que apunta a la nueva dirección en el TO_SPACE.
* Si se intenta copiar un objeto, y al revisar el heap se encuentra una dirección de orward (con el tag), no se copia )(pues ya fué copiado) y se retorna el pontero forwardeado.
* Al terminar de copiar, se guarda el nuevo puntero (con el tag correspondiente) en el stack.
* Se intercambian el FROM y el TO-space
* si luego de todo esto, sigue sin haber suficiente memoria en el heap, significa que no había suficiente memoria para el programa y se la nza una excpeión de memoria insuficiente.

## Bugs en argumentos de funciones:

Se detectaron bugs en funciones con muchos argumentos en que estos terminaban pasándose desordenados, provocando que los últimos se fueran a registros mientras que los primeros se guardaran en el stack, y al leerlos se obtenían resultados erróneos. Para solucionar este bug fue necesario invertir el orden de la lista de argumentos en ciertas partes del código para asegurarse de que los argumentos se guardaran correctamente.

Además, se encontró otro bug a partir del anterior en que se sobrescribían los registros de los primeros argumentos de la función al llamar una función dentro de otra, por lo que se modificó ligeramente el parser para almacenar estos argumentos correctamente al momento de llamar una función.

Durante los tests se encontró un error en la división que no se alcanzó a arreglar, ya que era bastante raro y solo apareción en un test en particular.

## Recursión

No se alcanzó a implementar la recursión debido al tiempo que tomó resolver los bugs de los argumentos de funciones.