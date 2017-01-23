# analizador-en-haskell
Este es un intento de crear un analizador sintáctico (generador de analizadores sintacticos ascendentes) en haskell
El fichero principal para el generador se llama: parser_asc.hs, recibe de la linea de comandos por un lado el nombre de un fichero con la gramatica y por otro el nombre del fichero en el cual se escribirá el analizador para la gramatica leida.
Ejemplo de un fichero que contiene una gramatica es grama_logica_1er_orden.txt, partiendo del cual se genera otro, con codigo haskell, en el ejemplo par_log_1er_orden.hs.
En este ultimo para la gramatica leida estan las tablas de accion e ir_a necesarias en el funcionamiento de un analizador ascendente.
A continuacion en el codigo de par_log_1er_orden.hs se ha añadido todo lo necesario para las acciones semanticas. 
Quizas seria mas correcto tener dos ficheros separados, uno con las tablas para el analizador, por si hubiera cambios en la gramatica y otro en donde irian las acciones semanticas, que de esta forma no se perderian en caso de tener que rehacer nuevamente las tablas (las tablas van embebidas en el fichero) por haberse cambiado la gramatica.
A continuacion voy a tratar de describir un poco como esta estructurado parser_asc.hs:
  a partir del axioma terminales variables y producciones leidas del fichero de la gramatica obtiene la coleccion canonica de conjuntos de elementos para una gramatica aumentada, con la funcion calc_colec_slr. Para ello se inicializa la coleccion de la manera: [Elemento {numero = 1,conjunto = cerradura t v p axioma ((PIzda ("P"++axioma),Pdcha [".",axioma]):[])}].
  esta va a usar una funcion ir_a donde para cada conjunto de elementos y simbolo de la gramatica va a ayudar a obtener un automata determinista que reconozca todos los prefijos viables validos.
El fichero que se genera con el analizador tiene las siguientes funciones:
Primero tabla_acc y tabla_ira en donde va a estar representado el automata determinista que hablabamos
busca_ira y busca_accion que buscan en las anteriores estructuras la operacion de accion que hay que hacer para el estado en la cima de la pila y el terminal leido
Si es la accion a ejecutar es "desplazar" se llama a la funcion desplazar_a que introduce en la pila el siguiente estado (sig_estado) que se indica en la operacion de accion y se avanza en la lectura de la entrada
Si la accion es "reducir" la cadena de entrada permanece igual, pero se eliminan de la pila tantos elementos como la longitud de la regla por la que se reduce, ademas de insertar en la pila en estado que indica busca_ira para el estado que queda en la cima de la pila y la variable izquierda de la regla por la que se reduce
Para eliminar elementos de la pila, se hace:
    (dropInt (length (reduce_por tabla_acc y (fst x))) (y:ys)) en donde reduce_por devuelve la parte derecha de la regla por la que se reduce, length devuelve la longitud de esa parte derecha y dropInt elimina de la pila (y:ys) tantos elementos como indica length
    despues se introduce en la pila es estado que indica la funcion busca_ira para el estado en la cabecera de la pila y la variable en la parte izquierda de la regla que devuelve (reduce_a tabla_acc y (fst x)), la secuencia de llamadas completa es:
    ((busca_ira tabla_ira (head (dropInt (if((reduce_por tabla_acc y (fst x))!!0=="lambda") then 0 else (length (reduce_por tabla_acc y (fst x)))) (y:ys))) (reduce_a tabla_acc y (fst x))):(if((reduce_por tabla_acc y (fst x))!!0=="lambda") then (y:ys) else (dropInt (length (reduce_por tabla_acc y (fst x))) (y:ys)))) 
