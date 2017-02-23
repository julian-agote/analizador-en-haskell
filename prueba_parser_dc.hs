import Parser_dc
import Data.Char(ord,chr)
import Utilidades
import Arbol
import System.IO
import System.Environment
buscaSigToken::String->String->(String,String)
buscaSigToken [] "" =("$","")
buscaSigToken [] y = if(ord(head y)>=ord('0') && ord(head y)<=ord('9')) then ("num",y) else ("id",y)
buscaSigToken x y |((head x)=='\n'||(head x)==' ') && (trim y)==""=(buscaSigToken  (tail x) "")
                  |(head x)==':' && (trim y)==""=("dos_puntos",(substr x 1 1))
                  |(head x)=='*' && (trim y)==""=("asterisco",(substr x 1 1))
                  |(head x)==',' && (trim y)==""=("coma",(substr x 1 1))
                  |(head x)=='(' && (trim y)==""=("par_ab",(substr x 1 1))
                  |(head x)==')' && (trim y)==""=("par_ce",(substr x 1 1))
                  |(head x)==';' && (trim y)==""=("punto_coma",(substr x 1 1))
                  |(head x)=='[' && (trim y)==""=("[",(substr x 1 1))
                  |(head x)==']' && (trim y)==""=("]",(substr x 1 1))
                  |(head x)=='\'' && (trim y)==""=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
                  |(head x)=='\'' && (trim y)/=""=("lit",y)
                  |(head x)=='|' && (trim y)==""=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
                  |(head x)=='|' && (trim y)=="|crear|"=("crear",y)
                  |(head x)=='|' && (trim y)=="|destruir|"=("destruir",y)
                  |(head x)=='|' && (trim y)=="|activacion|"=("activacion",y)
                  |(head x)=='|' && (trim y)=="|evento|"=("evento",y)
                  |(null y ||(head y)`elem` (['a'..'z']++['A'..'Z']))&&((ord(head x)>=ord('a') && ord(head x)<=ord('z'))||(ord(head x)>=ord('A') && ord(head x)<=ord('Z'))||(head x)==' ')=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
                  |(null y ||(head y)`elem` ['0'..'9'])&&(ord(head x)>=ord('0') && ord(head x)<=ord('9'))=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
                  |otherwise = if(ord(head y)>=ord('0') && ord(head y)<=ord('9')) then ("num",y) else ("id",y)
sigToken::String->[(String,String)]
sigToken [] = [("$","")]
sigToken x = (buscaSigToken (trim x) ""):(sigToken (substr (trim x) ((posb (snd (buscaSigToken (trim x) "")) 1 1 x)+(length (snd (buscaSigToken (trim x) "")))) (length x)))

evaluar::Arbolsintactico->String
evaluar x |((obtener_regla_aplicable x)=="DC -> OBJETO MENSAJE OBJETO nl DC")=(obtener_regla_aplicable x)++[chr(10)]++concat [(++) [chr(10)] (evaluar y)|y <- (devolver_ramas x)] 
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
        h1 <- openFile "dc_prestamo.txt" ReadMode
        y <- hGetContents h1
        -- if (instrb (parser_slr (sigToken y) [1]) "entrada correcta" 1 1)>0 then putStrLn (evaluar (parser_slr_arbol (sigToken y) [1] [])) 
                                                                           -- else putStrLn ((parser_slr (sigToken y) [1]))  
        putStrLn ((parser_slr (sigToken y) [1]))
        hClose h1       
