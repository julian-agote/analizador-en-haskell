import Parser_jkl3
import Arbol
import System.IO
import System.Environment
main = do
        h1 <- openFile "fact.jkl" ReadMode
        y  <- hGetContents h1
        putStrLn (parser_slr y [1])
        --putStrLn (obtener_programa(parser_slr_arbol y [1] []))
        hClose h1       
