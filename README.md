# analizador-en-haskell
Este es un intento de crear un analizador sintáctico (generador de analizadores sintacticos ascendentes) en haskell
El fichero principal para el generador se llama: parser_asc.hs, recibe de la linea de comandos por un lado el nombre de un fichero con la gramatica y por otro el nombre del fichero en el cual se escribirá el analizador para la gramatica leida.
Ejemplo de un fichero que contiene una gramatica es grama_logica_1er_orden.txt, partiendo del cual se genera otro, con codigo haskell, en el ejemplo par_log_1er_orden.hs.
En este ultimo para la gramatica leida estan las tablas de accion e ir_a necesarias en el funcionamiento de un analizador ascendente.
A continuacion en el codigo de par_log_1er_orden.hs se ha añadido todo lo necesario para las acciones semanticas. 
Quizas seria mas correcto tener dos ficheros separados, uno con las tablas para el analizador, por si hubiera cambios en la gramatica y otro en donde irian las acciones semanticas, que de esta forma no se perderian en caso de tener que rehacer nuevamente las tablas (las tablas van embebidas en el fichero) por haberse cambiado la gramatica.
