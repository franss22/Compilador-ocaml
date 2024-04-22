# Entrega 1
## Francisco Ortiz
## Gabriel Orrego

## ANF
Para las funciones binarias, se convierte la expresión parseada a forma normal. Las operaciones AND y OR se excluyen de esto para poder añadirles atajos de evaluación.
La función que se encarga de esto se encuentra en el archivo anf.ml

## Tagging
Se le hace tagging a cada expresión para que cada una tenga un tag (int) distinto, de forma que al hacer labels para saltos en ASM, nunca hayan 2 tags iguales.
La función encargada de esto es tag, en el archivo compile.ml

## Registers
Se utilizan los registros `RAX` para retornar, y `RBX` y `R11` para mantener datos adicionales.

## Representación en runtime
Para representar mas de un tipo de dato en runtime, se utiliza el bit menos significativo (LSB) de los 64 utilizados. Un 0 en el LSB significa que el valor es un int63, mientras que un 1 indica que el valor es un booleano, que representa true con un uno en el bit mas significativo (MSB), y false con un 0 en el MSB.
Para asegurarse que todos los ints estén bien representados, el valor se multiplica por 2 durante la compilación.
Por ahora no hay nada que evite que se usen tipos incorrectos en operaciones (ejemplo: `(not 42)`)

### Print en c
Se edita sys.c para que reconoczca las nuevas representaciones de valores en runtime.

## Expresiones implementadas
### if
Se implementa if utilizando los tags para evitar que hayan labels repetidos.

### Operaciones unarias
#### sub1 y add1
Estas operaciones se implementan usando `add` y `sub` en vez de `inc` y `dec`, puesto que estos últimos no funcionan bien con nuestra representación de los enteros, debido a que usamos el ultimo bit para diferenciar los tipos.
#### not
Se utiliza `xor` con una mascara especifica, de forma de no interferir con el bit de tipo


### Operaciones binarias
Se implementan las operaciones pedidas en el enunciado, junto con varias mas que son triviales de implementar siguiendo la misma estructura que las ya hechas.
#### let 
Se implementa let utilizando alocación estática en el stack, mediante una variable env durante la compilación que lleva cuenta de las posiciones de cada variable.
#### add (y sub) [int -> int -> int]
Se implementa de forma trivial usando `add`. Debido al uso de ANF, al momento de compilar podemos suponer que los argumentos entregados en el stack son valores numericos.
#### and (y or) [bool -> bool -> bool]
Se implementa and (y de paso or, que es muy similar), junto con sus atajos de evaluación: 
- Si el primer argumento de AND es false, no es necesario calcular el segundo para saber que la expresión completa es false
- Si el primer argumento de OR es true, no es necesario calcular el segundo para saber que la expresión completa es true
Para hacer esto fué necesario no poner las expresiones `and` y `or` en ANF, puesto que eso haría las evaluaciones de los argumentos eager.
#### Comparaciones (<, >, !=, ==, xor) [int -> int -> bool]*
Se implementa < (pedido en el enunciado) y el resto de comparraciones usando `cmp`. Se aprovecha el sistema de tagging para los saltos necesarios en una comparación. Por la forma de sus tablas de verdad, se puede utilizar `xor` tanto para != como xor.
#### mul, div y mod
Se implementa mul (asegurandose de hacer arithmetic shift right debido a la representación de ints). Sin embargo, div y mod no funcionan y se dejan pendientes.

## Integer overflow
Por ahora, el compilador detiene intentos de usar un numero mayor al maximo numero representable, sin embargo, no hay checks en runtima que vean si una suma se sale de este limite.

## Tests
Se testean los casos simples de cada una de las operacoines implementadas.
Tambien se testean algunas operaciones nesteadas para asegurarse de que funcione bien.
Se testea que hay error en caso de intentar acceder a una variable no decalarada en un let.
Se testea los integer overflow, pero como aun no hay checks en runtime, se deja para la entrega 2.

