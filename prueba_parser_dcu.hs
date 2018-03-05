import Parser_dcu
import Data.Char(ord,chr)
import Utilidades
import Arbol
import System.IO
import System.Environment
buscaSigToken::String->String->(String,String)
buscaSigToken [] "" =("$","")
buscaSigToken x y |((head x)=='\n'||(head x)==' ') && (trim y)==""=(buscaSigToken  (tail x) "")
                  |(head x)==',' && (trim y)==""=("coma",(substr x 1 1))
                  |(head x)=='{' && (trim y)==""=("llave_ab",(substr x 1 1))
                  |(head x)=='}' && (trim y)==""=("llave_ce",(substr x 1 1))
                  |(head x)=='[' && (trim y)==""=(buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1)))
                  |(head x)==']' && (trim y)/=""=("actor",y++"]")
                  |(head x)=='(' && (trim y)==""=(buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1)))
                  |(head x)==')' && (trim y)/=""=("caso_uso",y++")")
                  |(head x)=='<' && (trim y)==""=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
                  |(head x)=='>' && (trim y)=="<usa"=("usa",y++">")
                  |(head x)=='>' && (trim y)=="<extiende"=("extiende",y++">")
                  |(null y ||(head y)`elem` ['[','(','<'])&&((ord(head x)>=ord('a') && ord(head x)<=ord('z'))||(ord(head x)>=ord('A') && ord(head x)<=ord('Z'))||(head x)==' ')=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
sigToken::String->[(String,String)]
sigToken [] = [("$","")]
sigToken x = (buscaSigToken x ""):(sigToken (substr x ((posb (snd (buscaSigToken x "")) 1 1 x)+(length (snd (buscaSigToken x "")))) (length x)))

main = do
        h1 <- openFile "dcu_prestamo.txt" ReadMode
        y  <- hGetContents h1
        putStrLn (visualizar_resto_entrada  (sigToken y))   
        putStrLn (parser_slr (sigToken y) [1])
        --putStrLn (obtener_programa(parser_slr_arbol (sigToken y) [1] []))
        hClose h1       
