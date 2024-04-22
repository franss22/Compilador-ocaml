# Entrega 4
## Francisco Ortiz
## Gabriel Orrego


# Funciones de primera clase
Para hacer funcionar las funciones de primera clase, hacemos lo siguiente:
* Determinamos los valores libres: ids sin valor definido dentro del cuerpo de la lambda
* Para estos valores, creamos un nuevo tipo de acceso de Id: llamado ClosPos, indica que un valor estará guardado en la closure de una función.
* Compilamos el cuerpo de la función, dándole un env vacío excepto por estos valores ClosPos.
* Rodeamos el cuerpo de la función con tags y un salto, de forma que antes de entrar al cuerpo de la función en el asm, se salta al final, a menos que se haya llamado la funciń con call.
* Compilamos una closure, un nuevo tipo de dato similar a las tuplas que contiene, en orden: La aridad de la función, el label de la función, la cantidad de valores libres guardados, y luego los valores libres guardados. Los valores libres se toman del env del contexto de la definición de la función.
* Guardamos la closure en el heap y el pointer a la closure en RAX, con tag 0x5
  
# Llamadas a lambdas
Para llamar una función lambda, se hicieron los siguientes cambios:
* Ahora cada función guarda (y luego recupera antes de retornar) R12 y R13, para utilizar estos registros para guardar el puntero a la closure de la función que se está ejecutando. R13 se terminó no utilizando, pero dado que hay que pushear 2 valores para mantener el alineamiento de RSP, se deja guardado enc aso de que sea necesario mas tarde.
  
Al llamar una función lambda, se le da el puntero a la closure como primer argumento, y luego se guarda en R12, de forma que si en algun momento es necesario buscar un valor libre guardado en al closure, se hace con respecto a R12.

Al llamar una función lambda, se revisa de que el primer valor de la aplicación sea una closure. Si lo es, se revisa de que la aridad sea correcta, y luego se llama la función utilizando el segundo valor del closure, la dirección del label de la función.

Para tener mas facil acceso a la closure de una aplicación, todas las aplicaciones de lambda se cambian (durante el paso a ANF) 
de `(@ lambda args)` a `(let (f lambda) (@ f args))` y la representación en AST de la aplicación queda con un nodo `Empty` en el lugar de la lambda. De esta forma, durante la compilación podemos extraer el puntero directmente de la memoria local en vez de tener que hacer malabares con los registros.

# Lambdas con recursión
No se logró terminar de implementar la recursión para lambdas, se dejará como una característica pendiente para la próxima entrega.