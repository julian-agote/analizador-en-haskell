import Parser_arit2
import Data.Char(ord,chr)
import Utilidades
import Arbol
buscaSigToken::String->String->(String,String)
buscaSigToken [] "" =("$","")
buscaSigToken [] y = if(ord(head y)>=ord('0') && ord(head y)<=ord('9')) then ("n",y) else ("id",y)
buscaSigToken x y |((head x)=='\n'||(head x)==' ') && (trim y)==""=(buscaSigToken  (tail x) "")
                  |(head x)=='+' && (trim y)==""=("+",(substr x 1 1))
                  |(head x)=='*' && (trim y)==""=("*",(substr x 1 1))
                  |(head x)=='(' && (trim y)==""=("(",(substr x 1 1))
                  |(head x)==')' && (trim y)==""=(")",(substr x 1 1))
                  |((ord(head x)>=ord('a') && ord(head x)<=ord('z'))||(ord(head x)>=ord('0') && ord(head x)<=ord('9')))=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
                  |otherwise = if(ord(head y)>=ord('0') && ord(head y)<=ord('9')) then ("n",y) else ("id",y)
sigToken::String->[(String,String)]
sigToken [] = [("$","")]
sigToken x = (buscaSigToken (trim x) ""):(sigToken (substr (trim x) ((length (snd (buscaSigToken (trim x) "")))+1) (length x)))

{-evaluar::Arbolsintactico->Int
evaluar x |((obtener_regla_aplicable x)=="EP -> + T EP")=head [(+) 0 (evaluar y)|y <- devolver_ramas x] 
          |((obtener_regla_aplicable x)=="E -> T EP")=head [(+) (evaluar (head (devolver_ramas x))) (evaluar y)|y <- tail (devolver_ramas x)] 
          |((obtener_regla_aplicable x)=="T -> F TP")=head [(*) (evaluar (head (devolver_ramas x))) (evaluar y)|y <- tail (devolver_ramas x)] 
          |((obtener_regla_aplicable x)=="TP -> * F TP")=head [(*) 1 (evaluar y)|y <- devolver_ramas x] 
          |((obtener_regla_aplicable x)=="F -> n")=(evaluar  (head (devolver_ramas x)))
          |((obtener_regla_aplicable x)=="F -> ( E )")=(evaluar (head (tail(devolver_ramas x))))
          |((obtener_regla_aplicable x)=="n")=(devolver_valor x)
          |otherwise=0-}

evaluar::Arbolsintactico->String
evaluar x |((obtener_regla_aplicable x)=="EP -> + T EP")=(obtener_regla_aplicable x)++[chr(10)]++concat [(++) [chr(10)] (evaluar y)|y <-  (devolver_ramas x)] 
          |((obtener_regla_aplicable x)=="E -> T EP")=(obtener_regla_aplicable x)++[chr(10)]++concat [(++) [chr(10)] (evaluar y)|y <-  (devolver_ramas x)] 
          |((obtener_regla_aplicable x)=="T -> F TP")=(obtener_regla_aplicable x)++[chr(10)]++concat [(++) [chr(10)] (evaluar y)|y <-  (devolver_ramas x)] 
          |((obtener_regla_aplicable x)=="TP -> * F TP")=(obtener_regla_aplicable x)++[chr(10)]++concat [(++) [chr(10)] (evaluar y)|y <-  (devolver_ramas x)] 
          |((obtener_regla_aplicable x)=="F -> n")=(obtener_regla_aplicable x)++[chr(10)]++(evaluar  (head (devolver_ramas x)))
          |((obtener_regla_aplicable x)=="F -> ( E )")=(obtener_regla_aplicable x)++[chr(10)]++(evaluar (head (tail(devolver_ramas x))))
          |((obtener_regla_aplicable x)=="n")=(show (devolver_valor x))++[chr(10)]
          |otherwise="otro="++(obtener_regla_aplicable x)++[chr(10)]


main:: IO ()
main = do
        putStrLn "Introduce una cadena:"
        y <- getLine
        putStrLn (parser_slr (sigToken y) [1])  
        if (instrb (parser_slr (sigToken y) [1]) "entrada correcta" 1 1)>0 then putStrLn (evaluar (parser_slr_arbol (sigToken y) [1] [])) 
                                                                           else putStrLn ((parser_slr (sigToken y) [1]))    
