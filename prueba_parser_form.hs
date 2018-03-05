import Parser_form
import Data.Char(ord,chr)
import Utilidades
import Arbol
import System.IO
import System.Environment
buscaSigToken::String->String->(String,String)
buscaSigToken [] "" =("$","")
buscaSigToken [] y = if(ord(head y)>=ord('0') && ord(head y)<=ord('9')) then ("num",y) else if (rtrim y)=="v-r" then ("v-r","v-r") else if (rtrim y)=="v-b" then ("v-b","v-b") else  ("id",y)
buscaSigToken x y |((head x)=='\n'||(head x)==' ') && (trim y)==""=(buscaSigToken  (tail x) "")
                  |(head x)==',' && (trim y)==""=("coma",(substr x 1 1))
                  |(head x)=='(' && (trim y)==""=("par_ab",(substr x 1 1))
                  |(head x)==')' && (trim y)==""=("par_ce",(substr x 1 1))
                  |(head x)=='x' && ((substr x 2 1)==" "||(substr x 2 1)=="," ||(substr x 2 1)==")") && (null y)= ("x","x")
                  |(head x)=='e' && ((substr x 2 1)==" "||(substr x 2 1)=="," ||(substr x 2 1)==")") && (null y)= ("e","e")
                  |(head x)=='s' && ((substr x 2 1)==" "||(substr x 2 1)=="," ||(substr x 2 1)==")") && (null y)= ("s","s")
                  |(head x)=='m' && ((substr x 2 1)==" "||(substr x 2 1)=="," ||(substr x 2 1)==")") && (null y)= ("m","m")
                  |(head x)=='c' && ((substr x 2 1)==" "||(substr x 2 1)=="," ||(substr x 2 1)==")") && (null y)= ("c","c")
                  |(head x)=='f' && ((substr x 2 1)==" "||(substr x 2 1)=="," ||(substr x 2 1)==")") && (null y)= ("f","f")
                  |(head x)=='t' && ((substr x 2 1)==" "||(substr x 2 1)=="," ||(substr x 2 1)==")") && (null y)= ("t","t")
                  |(head x)=='"' && (trim y)==""=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
                  |(head x)/='"' && (trim y)/="" && (head y)=='"'=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
                  |(head x)=='"' && (trim y)/=""=("lit",(y++(substr x 1 1)))
                  |(null y ||(head y)`elem` (['a'..'z']++['A'..'Z']))&&((ord(head x)>=ord('a') && ord(head x)<=ord('z'))||(ord(head x)>=ord('A') && ord(head x)<=ord('Z'))||(ord(head x)>=ord('0') && ord(head x)<=ord('9'))||(head x)=='-'||(head x)=='_')=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
                  |(null y ||(head y)`elem` ['0'..'9'])&&(ord(head x)>=ord('0') && ord(head x)<=ord('9'))=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
                  |otherwise = if(ord(head y)>=ord('0') && ord(head y)<=ord('9')) then ("num",y) else  if (rtrim y)=="v-r" then ("v-r","v-r") else if (rtrim y)=="v-b" then ("v-b","v-b") else ("id", (rtrim y))
sigToken::String->[(String,String)]
sigToken [] = [("$","")]
--sigToken x = (buscaSigToken (trim x) ""):(sigToken (substr (trim x) ((posb (snd (buscaSigToken (trim x) "")) 1 1 x)+(length (snd (buscaSigToken (trim x) "")))) (length x)))
sigToken x = (buscaSigToken x ""):(sigToken (substr x ((posb (snd (buscaSigToken x "")) 1 1 x)+(length (snd (buscaSigToken x "")))) (length x)))

obtener_los_bloques::Arbolsintactico->String
obtener_los_bloques x |((obtener_regla_aplicable x)=="BLOQUES -> id CAMPOS VALREG PROCESO RBLOQUES")=(devolver_id (head(devolver_ramas x)))++","++(obtener_los_bloques (last(devolver_ramas x)))
                     |((obtener_regla_aplicable x)=="FORM -> id DESCP BLOQUES")=(obtener_los_bloques (last(devolver_ramas x)))
                     |((obtener_regla_aplicable x)=="RBLOQUES -> coma id CAMPOS VALREG PROCESO RBLOQUES")=(devolver_id (head(tail(devolver_ramas x))))++","++(obtener_los_bloques (last(devolver_ramas x)))
                     |otherwise=""
obtener_los_campos::String->Arbolsintactico->String
obtener_los_campos x z|((obtener_regla_aplicable z)=="FORM -> id DESCP BLOQUES")=obtener_los_campos x (last(devolver_ramas z))
                      |((obtener_regla_aplicable z)=="BLOQUES -> id CAMPOS VALREG PROCESO RBLOQUES" && (devolver_id (head(devolver_ramas z)))==x)=
                                  (obtener_los_campos x (head(tail(devolver_ramas z))))++","++(obtener_los_campos x (last(devolver_ramas z)))
		      |((obtener_regla_aplicable z)=="RBLOQUES -> coma id CAMPOS VALREG PROCESO RBLOQUES" && (devolver_id (head(tail(devolver_ramas z))))==x)=
		                                        (obtener_los_campos x (head(tail(tail(devolver_ramas z)))))++","++(obtener_los_campos x (last(devolver_ramas z)))
                      |((obtener_regla_aplicable z)=="BLOQUES -> id CAMPOS VALREG PROCESO RBLOQUES")=obtener_los_campos x (last(devolver_ramas z))
		      |((obtener_regla_aplicable z)=="RBLOQUES -> coma id CAMPOS VALREG PROCESO RBLOQUES")=obtener_los_campos x (last(devolver_ramas z))
                      |((obtener_regla_aplicable z)=="CAMPOS -> par_ab id ATTRC RCAMPOS par_ce")=(devolver_id (head (tail(devolver_ramas z))))++","++(obtener_los_campos x ((devolver_ramas z)!!3))
		      |((obtener_regla_aplicable z)=="RCAMPOS -> coma id ATTRC RCAMPOS")=(devolver_id (head (tail(devolver_ramas z))))++","++(obtener_los_campos x ((devolver_ramas z)!!3))
		      |otherwise=""
visualizar_resto_entrada :: [(String, String)] -> String
visualizar_resto_entrada (x:xs)=(first x)++(visualizar_resto_entrada xs)
visualizar_resto_entrada []=""
main = do
        h1 <- openFile "form_expm0570.txt" ReadMode
        y  <- hGetContents h1
        --putStrLn (visualizar_resto_entrada  (sigToken y))                                                                         --putStrLn ((parser_slr (sigToken y) [1]))
        putStrLn (join ',' (elimina_rep (split ',' (obtener_los_bloques(parser_slr_arbol (sigToken y) [1] [])))))
        putStrLn (join '\n' [x++":"++(join ',' (elimina_rep (split ',' (obtener_los_campos x (parser_slr_arbol (sigToken y) [1] [])))))|x <-(elimina_rep (split ',' (obtener_los_bloques(parser_slr_arbol (sigToken y) [1] []))))])
        hClose h1       
