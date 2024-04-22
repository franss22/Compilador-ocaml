# Entrega 2
## Francisco Ortiz
## Gabriel Orrego

# Operaciones aritméticas

Se agregaron las operaciones / y %, que no habían sido implementadas en la entrega anterior. Ambas operaciones usan la misma instrucción `idiv`, pero la división se queda con el resultado de `RAX` (el resultado de la división entera) mientras que el módulo toma el resultado de `RDX` (el cociente de la división entera).

# Funciones
## Calling convention
Se adapta la calling convention de x64 (los primeros 6 argumentos de una función en los registros RDI; RSI; RDX; RCX; R8; R9, y el resto se guardan en el stack).

## Handling del stack
Para los valores guardados en el stack, utilizamos RSP. Para llevar la cuenta de las variables locales (introducidas con let) utilizamos RBP. AL inicio de cada función debemos guardar en el stack la posición del RBP de la calling function, dejar un espacio reservado entre RBP y RSP para las variables locales, y al final de la función reponer el RBP y RSP original.

## Espacio local
Para calcular el espacio local que toma cada función o cuerpo, contamos la cantidad de lets del cuerpo normalizado (en ANF). Le añadimos un poco de espacio extra para evitar problemas de memoria.

## Fenv
Las funciones introducen un nuevo tipo de ambiente durante la compilación, el ambiente de las funciones definidas. Este ambiente lleva cuenta del nombre, aridad, y espacio local necesario para cada función definida en un programa. De esta manera, durante la compilación al encontrar uba aplicación de función podemos revisar:
* Que la función exista y esté definida
* Que la cantidad de argumentos entregados sea igual a los que necesita la función
* Durante la compilación del cuerpo de cada función, necesitamos saber el espacio local que se necesita para reservar espacio entre RBP y RSP

## Env
Ahora que las funciones pueden acceder a argumentos en los registros y en el stack, además de en la memoria local, es necesario distinguir entre estos 3 tipos de valores guardados a los que accede una Id. Para eso, introducimos el tipo Slot que representa un valor guardado en Env, y luego al compilar determinamos donde ir a buscar el valor, si en un offset del RBP hacia los negativos (memoria local), en un registro (primeros 6 argumentos de una función), o en un offset positivo del RBP (resto de argumentos de una función, guardados en el stack)

# Print

Se implementó la función print como una función foránea de C. Al llamar `print` se imprime el argumento precedido por un `> ` y se devuelve el mismo argumento con que se llamó. Este es un ejemplo del funcionamiento de `print`:

```
INPUT:
(add1 (print 1))

OUTPUT:
> 1
2
```

# Errores de Tipo

Se implementó _type checking_ dinámico para las operaciones binarias y unarias. Esto añade instrucciones extra que se aseguran de que los argumentos entregados para la operación tengan el tipo correspondiente, y en caso de que alguno no satisfaga las condiciones de tipo se imprime un error de tipo y se termina el programa.

Las inconsistencias de tipo se detectan usando el bit menos significativo del valor entregado y el error se imprime llamando a una función foránea de C.

# Errores aritméticos

Se añadió una opción de compilación _"safe"_ que genera instrucciones extra para detectar errores aritméticos en _runtime_.

No se logró integrar la _flag_ `-safe` como parámetro del programa, así que por el momento se implementó como una variable en `safetyCheck.ml`, que por defecto es `true`
## Overflow en runtime

Para las operaciones +, - y * se aborta el programa si se  produce un over/underflow tras realizar la operación.

Para detectar over/underflow en + y - bastó utilizar la instrucción `jo`, sin embargo para la multiplicación fue necesario detectar manualmente el overflow ya que la instrucción `imul` entrega el resultado como un número de mayor tamaño que los factores. El overflow de multiplicación se reconoce comprobando que el bit de signo del resultado coincida con el de los factores (debiera ser 0 si los factores tienen el mismo signo y 1 si son distintos); esto se detecta aplicando `xor` y aislando el bit más significativo.

## División por 0
Para / y % se revisa antes de realizar la operación si el divisor es 0, en cuyo caso se lanza un error de división/módulo por 0 y se aborta el programa.

# Bug malvado
Durante el desarrollo encontramos un bug que no fuimos capaces de resolver. Por alguna razón, especificamente en el computador de Francisco, ciertos errores de tipo (en las opperaciones unarias) que deberían fallar según los errores hechos por nosotros dan error de segmentation fault durante el printf de c. No se pudo reproducir el problema en el PC de Gabriel, el otro integrante del grupo, ni el los computadores de ambos auxiliares.