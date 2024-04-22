# Entrega 3
## Francisco Ortiz
## Gabriel Orrego


# Tuplas 
Para implementar tuplas al lenguaje fue necesario hacer varios cambios. 
Ahora se usa memoria del heap para almacenar los elementos de una tupla; esta memoria se pide al inicio desde C usando `calloc`, y se reservó el registro `R15` para tener un puntero al próximo lugar disponible del heap. 

Como las tuplas tienen una cantidad variable de elementos, estas son representadas por su ubicación en la memoria, y para poder diferenciarlas de un número, se usa la propiedad de que siempre la dirección en memoria será un múltiplo de 8 (esto se impone al principio del programa alineando el heap) para extender el sistema de tags de tipos usando los 3 bits menos significativos. En particular, una tupla tendrá sus últimos 3 bits en `011`, a diferencia de un booleando que tiene `001` y un número que tiene `xx0`. 

Para acceder a los elementos de una tupla, basta remover el tag de la tupla restándole 3 (`011`) y usar la dirección de memoria aplicando un offset, sin embargo hay que tener en cuenta que el primer elemento (con offset 0) contiene el largo de la tupla; esto sirve para asegurarse de que al acceder a un elemento de la tupla no se intente acceder a otras áreas de la memoria.

Las tuplas se definen en el lenguaje de la forma `(tup e1 e2 ... en)`, y pueden tener una cantidad arbitraria de elementos.

## Acceso a elementos

Para acceder al k-ésimo elemento de una tupla `t` se usa la operación `(get t k)`. Esta operación se encarga de hacer el chequeo de tipos de ambos operandos y también revisa que `k` sea un número entre 0 y el largo de la tupla.

## Mutación

Se puede modificar el k-ésimo elemento de la tupla `t` con un nuevo valor `v` usando `(set t k v)`. Esta operación funciona de forma similar a `get`, realizando el chequeo de tipos e índice correspondiente, y reemplaza en memoria el valor antiguo por el nuevo.

# Records
Para implementar los records, los hacemos de forma de azucar sintactica de aplicación de tuple y get. Es decir, al declarar el record `(point3d x y z)`, no creamos un nuevo tipo de dato, sino que añadimos 4 nuevas funciones al function environment: 
* `(point3d x y z)`: crea una tupla con 3 elementos
* `(point3d-x tupla)`, `(point3d-y tupla)`, `(point3d-z tupla)`: Acceden al primer, segundo y tercer elemento de cualquier tupla, respectivamente.
  
Al declarar records, nos aseguramos de que no hayan records con el mismo nombre, para evitar ambiguiedades. Ademas, nos aseguramos de que un record no pueda tener 2 o mas fields con el mismo nombre.

Al compilar estas funciones, se tratan casi igual que si fueran `(tup)` y `get`, con algunas diferencias debido a que son aplicaciones de función. Sin embargo, nos saltamos gran parte del proceso de llamar una función (no hay labels ni `call` de por medio).

# Bug de la entrega anterior
En la entrega anterior quedamos con un bug extraño que solo ocurría en el pc de uno de los integrantes, que ocurría a veces al llamar `print`. Investigando, descubrimos que era porque faltaba asegurar que rsp estuviera alineado a 16 bits al momento de llamar una función. Lo arreglamos asegurandonos que el espacio pedido para variables locales siempre fuera un multiplo de 16, y que al llamar una función, si tiene un numero impar de argumentos guardados en el stack, se rellena un espacio vacío extra.