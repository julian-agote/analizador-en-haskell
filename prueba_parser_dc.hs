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

obtener_las_clases::Arbolsintactico->String
obtener_las_clases x |((obtener_regla_aplicable x)=="CLASE -> dos_puntos id")=devolver_id (head(tail (devolver_ramas x)))
                     |((obtener_regla_aplicable x)=="DC -> OBJETO MENSAJE OBJETO RDC")=concat [(++) "," (obtener_las_clases y)|y <- (devolver_ramas x)] 
                     |((obtener_regla_aplicable x)=="RDC -> punto_coma OBJETO MENSAJE OBJETO RDC")=concat [(++) "," (obtener_las_clases y)|y <- (tail(devolver_ramas x))]
                     |((obtener_regla_aplicable x)=="OBJETO -> NOMBRE_OB CLASE")=concat [(++) "," (obtener_las_clases y)|y <- (tail(devolver_ramas x))]            
                     |otherwise=""

data Mensaje = Mensaje {objeto1::String,llamada::String,objeto2::String}
evaluar::Arbolsintactico->[Mensaje]
evaluar x |((obtener_regla_aplicable x)=="DC -> OBJETO MENSAJE OBJETO RDC")=(Mensaje{objeto1=(obtener_la_clase (head (devolver_ramas x))),llamada=(obtener_el_mensaje ((devolver_ramas x)!!1)),objeto2=(obtener_la_clase ((devolver_ramas x)!!2))}):(evaluar ((devolver_ramas x)!!3))
          |((obtener_regla_aplicable x)=="RDC -> punto_coma OBJETO MENSAJE OBJETO RDC")=(Mensaje{objeto1=(obtener_la_clase ((devolver_ramas x)!!1)),llamada=(obtener_el_mensaje ((devolver_ramas x)!!2)),objeto2=(obtener_la_clase ((devolver_ramas x)!!3))}):(evaluar ((devolver_ramas x)!!4))
          |otherwise=[]

obtener_la_clase::Arbolsintactico->String
obtener_la_clase x |((obtener_regla_aplicable x)=="CLASE -> dos_puntos id")=devolver_id (head(tail (devolver_ramas x)))
                   |((obtener_regla_aplicable x)=="OBJETO -> NOMBRE_OB CLASE")=(obtener_la_clase ((devolver_ramas x)!!1))       

obtener_el_mensaje::Arbolsintactico->String                
obtener_el_mensaje x |((obtener_regla_aplicable x)=="MENSAJE -> num dos_puntos ITER id PARAMETROS")=devolver_id ((devolver_ramas x)!!3)

visualizar::[Mensaje]->String
visualizar []=""
visualizar (Mensaje{objeto1=o1,llamada=l,objeto2=o2}:xs)=o1++"--"++l++"-->"++o2++[chr(10)]++(visualizar xs)

main:: IO ()
main = do
        h1 <- openFile "dc_prestamo.txt" ReadMode
        y  <- hGetContents h1
        -- if (instrb (parser_slr (sigToken y) [1]) "entrada correcta" 1 1)>0 then putStrLn (evaluar (parser_slr_arbol (sigToken y) [1] [])) 
                                                                           -- else putStrLn ((parser_slr (sigToken y) [1]))  
        --putStrLn ((parser_slr (sigToken y) [1]))
        putStrLn (visualizar (evaluar(parser_slr_arbol (sigToken y) [1] [])))
        hClose h1       
