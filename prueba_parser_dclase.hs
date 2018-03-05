import Parser_dclase
import Data.Char(ord,chr)
import Utilidades
import Arbol
import System.IO
import System.Environment
buscaSigToken::String->String->(String,String)
buscaSigToken [] "" =("$","")
buscaSigToken [] y = if(ord(head y)>=ord('0') && ord(head y)<=ord('9')) then ("num",y) else  if (rtrim y)`elem` ["asoc"] then ((rtrim y),(rtrim y)) else ("id",y)
buscaSigToken x y |((head x)=='\n'||(head x)==' ') && (trim y)==""=(buscaSigToken  (tail x) "")
                  |(head x)==',' && (trim y)==""=("coma",(substr x 1 1))
                  |(head x)=='(' && (trim y)==""=("par_ab",(substr x 1 1))
                  |(head x)==')' && (trim y)==""=("par_ce",(substr x 1 1))
                  |(head x)==';' && (trim y)==""=("punto_coma",(substr x 1 1))
                  |(head x)=='{' && (trim y)==""=("llave_ab",(substr x 1 1))
                  |(head x)=='}' && (trim y)==""=("llave_ce",(substr x 1 1))
                  |(head x)==':' && (trim y)==""=("dos_puntos",(substr x 1 1))
                  |(head x)=='*' && (trim y)==""=("asterisco",(substr x 1 1))
                  |(head x)=='-' && (trim y)==""=("privada",(substr x 1 1))
                  |(head x)=='+' && (trim y)==""=("publica",(substr x 1 1))
                  |(head x)=='#' && (trim y)==""=("protegida",(substr x 1 1))
                  |(substr x 1 2)==".." && (trim y)==""=("puntos_susp",(substr x 1 2))
                  |(head x)=='=' && (trim y)==""=("igual",(substr x 1 1))
                  |(head x)=='"' && (trim y)==""=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
                  |(head x)/='"' && (trim y)/="" && (head y)=='"'=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
                  |(head x)=='"' && (trim y)/=""=("cad",(y++(substr x 1 1)))
                  |(null y ||(head y)`elem` (['a'..'z']++['A'..'Z']))&&((ord(head x)>=ord('a') && ord(head x)<=ord('z'))||(ord(head x)>=ord('A') && ord(head x)<=ord('Z'))||(ord(head x)>=ord('0') && ord(head x)<=ord('9'))||(head x)=='-'||(head x)=='_')=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
                  |(null y ||(head y)`elem` ['0'..'9'])&&(ord(head x)>=ord('0') && ord(head x)<=ord('9'))=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
                  |otherwise = if(ord(head y)>=ord('0') && ord(head y)<=ord('9')) then ("num",y) else if (rtrim y)`elem` ["asoc"] then ((rtrim y),(rtrim y)) else  ("id", (rtrim y))
sigToken::String->[(String,String)]
sigToken [] = [("$","")]
--sigToken x = (buscaSigToken (trim x) ""):(sigToken (substr (trim x) ((posb (snd (buscaSigToken (trim x) "")) 1 1 x)+(length (snd (buscaSigToken (trim x) "")))) (length x)))
sigToken x = (buscaSigToken x ""):(sigToken (substr x ((posb (snd (buscaSigToken x "")) 1 1 x)+(length (snd (buscaSigToken x "")))) (length x)))

obtener_programa::Arbolsintactico->String
obtener_programa x |((obtener_regla_aplicable x)=="PDCLASE -> DCLASE")=(obtener_programa (head(devolver_ramas x)))
                   |((obtener_regla_aplicable x)=="DCLASE -> id ESTEREOTIPO ATRIBUTOS punto_coma OPERACIONES RDCLASE")=
                          "class "++(devolver_id (head(devolver_ramas x)))++"{"++[chr(10)]++(obtener_atributos ((devolver_ramas x)!!2))++[chr (10)]++
                          (obtener_oper ((devolver_ramas x)!!4))++"}\n"++(obtener_programa (last(devolver_ramas x)))
                   |((obtener_regla_aplicable x)=="RDCLASE -> RELACION id ESTEREOTIPO ATRIBUTOS punto_coma OPERACIONES RDCLASE")=
                          "class "++(devolver_id ((devolver_ramas x)!!1))++"{"++[chr(10)]++(obtener_atributos ((devolver_ramas x)!!3))++[chr (10)]++
                          (obtener_oper ((devolver_ramas x)!!5))++"}\n"++(obtener_programa (last(devolver_ramas x)))
                   |otherwise=""
obtener_atributos::Arbolsintactico->String
obtener_atributos x|((obtener_regla_aplicable x)=="ATRIBUTOS -> VISIBILIDAD id TIPO VALOR_INICIAL PROPIEDADES RATRIBUTOS")=
                          (obtener_visibilidad (head (devolver_ramas x)))++(obtener_tipo ((devolver_ramas x)!!2))++(devolver_id ((devolver_ramas x)!!1))++";\n"++(obtener_atributos (last(devolver_ramas x)))
                   |((obtener_regla_aplicable x)=="RATRIBUTOS -> VISIBILIDAD id TIPO VALOR_INICIAL PROPIEDADES RATRIBUTOS")=
                          (obtener_visibilidad (head (devolver_ramas x)))++(obtener_tipo ((devolver_ramas x)!!2))++(devolver_id ((devolver_ramas x)!!1))++";\n"++(obtener_atributos (last(devolver_ramas x)))                         
                   |otherwise=""
obtener_visibilidad::Arbolsintactico->String
obtener_visibilidad x|((obtener_regla_aplicable x)=="VISIBILIDAD -> publica")="\tpublic "
                     |((obtener_regla_aplicable x)=="VISIBILIDAD -> privada")="\tprivate "
                     |((obtener_regla_aplicable x)=="VISIBILIDAD -> protegida")="\tprotected "
                     |otherwise=""
obtener_tipo::Arbolsintactico->String
obtener_tipo x|((obtener_regla_aplicable x)=="TIPO -> dos_puntos id")=(devolver_id ((devolver_ramas x)!!1))++" "
              |otherwise="" 
obtener_oper::Arbolsintactico->String
obtener_oper x|((obtener_regla_aplicable x)=="OPERACIONES -> VISIBILIDAD id PARAMETROS TIPO PROPIEDADES ROPERACIONES")=
                       (obtener_visibilidad (head (devolver_ramas x)))++(obtener_tipo ((devolver_ramas x)!!3))++(devolver_id ((devolver_ramas x)!!1))++"(){ }\n"++(obtener_oper (last(devolver_ramas x))) 
              |((obtener_regla_aplicable x)=="ROPERACIONES -> VISIBILIDAD id PARAMETROS TIPO PROPIEDADES ROPERACIONES")=
                       (obtener_visibilidad (head (devolver_ramas x)))++(obtener_tipo ((devolver_ramas x)!!3))++(devolver_id ((devolver_ramas x)!!1))++"(){ }\n"++(obtener_oper (last(devolver_ramas x))) 
              |otherwise=""        
main = do
        h1 <- openFile "dclase_prestamo.txt" ReadMode
        y  <- hGetContents h1
        --putStrLn (visualizar_resto_entrada  (sigToken y))   
        --putStrLn (parser_slr (sigToken y) [1])
        putStrLn (obtener_programa(parser_slr_arbol (sigToken y) [1] []))
        hClose h1       
