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
                  |otherwise = if(ord(head y)>=ord('0') && ord(head y)<=ord('9')) then ("num",y) else ("id", (rtrim y))
sigToken::String->[(String,String)]
sigToken [] = [("$","")]
--sigToken x = (buscaSigToken (trim x) ""):(sigToken (substr (trim x) ((posb (snd (buscaSigToken (trim x) "")) 1 1 x)+(length (snd (buscaSigToken (trim x) "")))) (length x)))
sigToken x = (buscaSigToken x ""):(sigToken (substr x ((posb (snd (buscaSigToken x "")) 1 1 x)+(length (snd (buscaSigToken x "")))) (length x)))

evaluar::Arbolsintactico->String
evaluar x |((obtener_regla_aplicable x)=="DC -> OBJETO MENSAJE OBJETO RDC")=(obtener_regla_aplicable x)++[chr(10)]++concat [(++) [chr(10)] (evaluar y)|y <- (devolver_ramas x)] 
          |((obtener_regla_aplicable x)=="RDC -> punto_coma OBJETO MENSAJE OBJETO RDC")=(obtener_regla_aplicable x)++[chr(10)]++concat [(++) [chr(10)] (evaluar y)|y <- (devolver_ramas x)] 
          |((obtener_regla_aplicable x)=="OBJETO -> NOMBRE_OB CLASE")=(obtener_regla_aplicable x)++[chr(10)]++concat [(++) [chr(10)] (evaluar y)|y <- (devolver_ramas x)] 
          |((obtener_regla_aplicable x)=="MENSAJE -> num dos_puntos ITER id PARAMETROS")=(obtener_regla_aplicable x)++[chr(10)]++concat [(++) [chr(10)] (evaluar y)|y <- (devolver_ramas x)] 
          |((obtener_regla_aplicable x)=="PARAMETROS -> par_ab LISTA par_ce")=(obtener_regla_aplicable x)++[chr(10)]++(evaluar (head (tail(devolver_ramas x))))
          |((obtener_regla_aplicable x)=="NOMBRE_OB -> id")=(obtener_regla_aplicable x)++[chr(10)]++(evaluar  (head (devolver_ramas x)))
          |((obtener_regla_aplicable x)=="CLASE -> dos_puntos id")=(obtener_regla_aplicable x)++[chr(10)]++(evaluar (head (tail(devolver_ramas x))))
          |((obtener_regla_aplicable x)=="LISTA -> id RLISTA")=(obtener_regla_aplicable x)++[chr(10)]++concat [(++) [chr(10)] (evaluar y)|y <- (devolver_ramas x)] 
          |((obtener_regla_aplicable x)=="RLISTA -> coma id RLISTA")=(obtener_regla_aplicable x)++[chr(10)]++concat [(++) [chr(10)] (evaluar y)|y <- (tail (devolver_ramas x))] 
          |otherwise="otro="++(obtener_regla_aplicable x)++[chr(10)]

obtener_las_clases::Arbolsintactico->String
obtener_las_clases x |((obtener_regla_aplicable x)=="CLASE -> dos_puntos id")=devolver_id (head(tail (devolver_ramas x)))
                     |((obtener_regla_aplicable x)=="DC -> OBJETO MENSAJE OBJETO RDC")=concat [(++) "," (obtener_las_clases y)|y <- (devolver_ramas x)] 
                     |((obtener_regla_aplicable x)=="RDC -> punto_coma OBJETO MENSAJE OBJETO RDC")=concat [(++) "," (obtener_las_clases y)|y <- (tail(devolver_ramas x))]
                     |((obtener_regla_aplicable x)=="OBJETO -> NOMBRE_OB CLASE")=concat [(++) "," (obtener_las_clases y)|y <- (tail(devolver_ramas x))]            
                     |otherwise=""

main = do
        h1 <- openFile "dc_prestamo.txt" ReadMode
        y  <- hGetContents h1
        -- if (instrb (parser_slr (sigToken y) [1]) "entrada correcta" 1 1)>0 then putStrLn (evaluar (parser_slr_arbol (sigToken y) [1] [])) 
                                                                           -- else putStrLn ((parser_slr (sigToken y) [1]))  
        --putStrLn ((parser_slr (sigToken y) [1]))
        putStrLn (join ',' (elimina_rep (split ',' (obtener_las_clases(parser_slr_arbol (sigToken y) [1] [])))))
        hClose h1       
