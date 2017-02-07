import Parser_dc
import Data.Char(ord,chr)
import Utilidades
import Arbol
buscaSigToken::String->String->(String,String)
buscaSigToken [] "" =("$","")
buscaSigToken [] y = if(ord(head y)>=ord('0') && ord(head y)<=ord('9')) then ("n",y) else ("id",y)
buscaSigToken x y |(head x)=='\n' && (trim y)==""=("nl",(substr x 1 1))
				  |(head x)==':' && (trim y)==""=(":",(substr x 1 1))
                  |(head x)=='*' && (trim y)==""=("*",(substr x 1 1))
				  |(head x)==',' && (trim y)==""=(",",(substr x 1 1))
                  |(head x)=='(' && (trim y)==""=("(",(substr x 1 1))
                  |(head x)==')' && (trim y)==""=(")",(substr x 1 1))
                  |(head x)=='[' && (trim y)==""=("[",(substr x 1 1))
                  |(head x)==']' && (trim y)==""=("]",(substr x 1 1))
				  |(head x)=='\'' && (trim y)==""=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
				  |(head x)=='\'' && (trim y)/=""=("lit",y)
				  |(head x)=='|' && (trim y)==""=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
				  |(head x)=='|' && (trim y)=="|crear|"=("crear",y)
				  |(head x)=='|' && (trim y)=="|destruir|"=("destruir",y)
				  |(head x)=='|' && (trim y)=="|activacion|"=("activacion",y)
				  |(head x)=='|' && (trim y)=="|evento|"=("evento",y)
                  |((ord(head x)>=ord('a') && ord(head x)<=ord('z'))||(ord(head x)>=ord('A') && ord(head x)<=ord('Z'))||(head x)==' ')=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
				  |(ord(head x)>=ord('0') && ord(head x)<=ord('9'))=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
                  |otherwise = if(ord(head y)>=ord('0') && ord(head y)<=ord('9')) then ("num",y) else ("id",y)
sigToken::String->[(String,String)]
sigToken [] = [("$","")]
sigToken x = (buscaSigToken (trim x) ""):(sigToken (substr (trim x) ((length (snd (buscaSigToken (trim x) "")))+1) (length x)))

evaluar::Arbolsintactico->String
evaluar x |((obtener_regla_aplicable x)=="DC -> + OBJETO MENSAJE OBJETO DC")=(obtener_regla_aplicable x)++[chr(10)]++concat [(++) [chr(10)] (evaluar y)|y <- (devolver_ramas x)] 
          |((obtener_regla_aplicable x)=="OBJETO->NOMBRE_OB CLASE")=(obtener_regla_aplicable x)++[chr(10)]++concat [(++) [chr(10)] (evaluar y)|y <- (devolver_ramas x)] 
          |((obtener_regla_aplicable x)=="MENSAJE -> num : ITER id PARAMETROS CONDICION OPERACION")=(obtener_regla_aplicable x)++[chr(10)]++concat [(++) [chr(10)] (evaluar y)|y <- (devolver_ramas x)] 
          |((obtener_regla_aplicable x)=="PARAMETROS->( LISTA )")=(obtener_regla_aplicable x)++[chr(10)]++(evaluar (head (tail(devolver_ramas x))))
          |((obtener_regla_aplicable x)=="NOMBRE_OB -> id")=(obtener_regla_aplicable x)++[chr(10)]++(evaluar  (head (devolver_ramas x)))
          |((obtener_regla_aplicable x)=="CLASE -> : id ")=(obtener_regla_aplicable x)++[chr(10)]++(evaluar (head (tail(devolver_ramas x))))
          |((obtener_regla_aplicable x)=="LISTA -> id RLISTA")=(obtener_regla_aplicable x)++[chr(10)]++concat [(++) [chr(10)] (evaluar y)|y <- (devolver_ramas x)] 
		  |((obtener_regla_aplicable x)=="RLISTA -> , id RLISTA")=(obtener_regla_aplicable x)++[chr(10)]++concat [(++) [chr(10)] (evaluar y)|y <- (tail (devolver_ramas x))] 
          |otherwise="otro="++(obtener_regla_aplicable x)++[chr(10)]


main:: IO ()
main = do
        [f1] <- getArgs 
		h1 <- openFile f1 ReadMode
		y <- hGetContents h1
		hClose h1
        if (instrb (parser_slr (sigToken y) [1]) "entrada correcta" 1 1)>0 then putStrLn (evaluar (parser_slr_arbol (sigToken y) [1] [])) 
                                                                           else putStrLn ((parser_slr (sigToken y) [1]))    
