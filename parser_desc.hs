import Data.Char(ord)
import Data.List(delete,reverse)
import System.IO
import System.Environment
import Char
import Utilidades

data Partes = PIzda String
          |Pdcha [String]
type Regla=(Partes,Partes)
type Reglas = [Regla]
r1PIzda::Partes
r1Pdcha::Partes
r1PIzda=PIzda "E"
r1Pdcha=Pdcha (split ' ' "T EP")
r1::Regla
r1 = (r1PIzda,r1Pdcha)
producciones::String->Reglas
producciones []=[]
producciones x = (PIzda (substr x 1 ((posb "->" 1 1 x) - 1)),Pdcha (split ' ' (substr x ((posb "->" 1 1 x) +2) (if (pos (chr 10) 1 1 x)==0 then length x else ((pos (chr 10) 1 1 x)-((posb "->" 1 1 x) +2)))))):(producciones (if (pos (chr 10) 1 1 x)==0 then "" else (substr x ((pos (chr 10) 1 1 x)+1) (length x))))
--producciones=r1:((PIzda "EP"),(Pdcha (split ' ' "+ T EP"))):((PIzda "EP"),(Pdcha (split ' ' "lambda"))):((PIzda "T"),(Pdcha (split ' ' "F TP"))):((PIzda "TP"),(Pdcha (split ' ' "* F TP"))):((PIzda "TP"),(Pdcha (split ' ' "lambda"))):((PIzda "F"),(Pdcha (split ' ' "( E )"))):((PIzda "F"),(Pdcha (split ' ' "n"))):[]
--terminales=["n","+","*","(",")"]
axioma="E"
axioma::String
terminales::String->[String]
terminales x = split ' ' x
variables::String->[String]
variables x = split ' ' x
esIgualPizda::String->Regla->Bool
esIgualPizda x ((PIzda y),_)|x==y=True
                                     |otherwise = False
reglasde::String->String->Reglas				
reglasde a p = filter (esIgualPizda a) (producciones p)
contienePdcha::String->Regla->Bool
contienePdcha x ((PIzda a),(Pdcha y))|(pos x 1 1 y)>0 = True
                                                    |otherwise = False
reglascontienen::String->String->Reglas
reglascontienen a p = filter (contienePdcha a) (producciones p)
showPartes::Partes->String
showPartes (PIzda x) = x++" -> "
showPartes (Pdcha x) = rtrim (join ' ' x)
showRegla::Regla->String
showRegla x = showPartes (fst x) ++showPartes (snd x) 
primerodcha::String->String->Partes->[String]
primerodcha _ _ (Pdcha []) = []
primerodcha term prod (Pdcha x) = primero term prod (head x)++(if (member "lambda" (primero term prod (head x))) then (primerodcha term prod (Pdcha (tail x))) else [])
primero::String->String->String->[String]
primero term prod x |(member x ((terminales term)++["lambda"])) = [x]
		           |otherwise = concat ([primerodcha term prod (snd n)|n<- (reglasde x prod)])
primero_cad::String->String->[String]->[String]
primero_cad _ _ [] = []
primero_cad term prod x|(member "lambda" (primero_cad2 term prod x)) = eliminar_rep (if not(member "lambda" (primero term prod (head (reverse x)))) then (delete "lambda" (primero_cad2 term prod x)) else primero_cad2 term prod x)
                                |otherwise = (primero_cad2 term prod x)
primero_cad2 term prod (x:xs) = primero term prod x ++ (if (member "lambda" (primero term prod x)) then primero_cad term prod xs else [])
eliminar_rep::[String]->[String]
eliminar_rep []=[]
eliminar_rep (x:xs) = x:(eliminar_rep (delete x xs))
--se define Siguiente(A) para el no terminal A como el cjto. de terminales que pueden aparecer a la dcha de A
--en alguna forma sentencial derivada del axioma de la gramatica
siguiente::String->String->String->[String]
siguiente term prod x |x==axioma = "$":(eliminar_rep (concat [if (pos x 1 1 n)<(length n) then primero_cad term prod (dropInt (pos x 1 1 n) n)++(if (member "lambda" (primero_cad term prod (dropInt (pos x 1 1 n) n))) then if not(x==y) then (siguiente term prod y) else [] else []) else if not(x==y) then (siguiente term prod y) else []|(PIzda y, Pdcha n)<-reglascontienen x prod]))
			     |otherwise = eliminar_rep (concat [if (pos x 1 1 n)<(length n) then primero_cad term prod (dropInt (pos x 1 1 n) n)++(if (member "lambda" (primero_cad term prod (dropInt (pos x 1 1 n) n))) then if not(x==y) then (siguiente term prod y) else [] else []) else if not(x==y) then (siguiente term prod y) else []|(PIzda y, Pdcha n)<-reglascontienen x prod])
-- si existe una regla A->alfa el analizador sintactico debe expandir por esta regla si en la cadena de entrada el
-- simbolo en curso pertenece a Primero(alfa). Si alfa=Lambda o deriva en Lambda (Lambda pertenece a Primero(alfa))
-- entonces se expandira por esta regla si el simbolo en curso de la entrada pertenece a siguiente(A)
tabla_analisis_desc::String->String->String->String->[String]
tabla_analisis_desc term prod x y = concat [m|(PIzda n,Pdcha m)<-(reglasde x prod),(member y (primero_cad term prod m))||((member "lambda" (primero_cad term prod m)) && (member y (siguiente term prod x)))]
generar_tabla::String->[String]->String->String
generar_tabla _ [] _ = "[]"
generar_tabla t (v:vs) p = concat(["(Pvar \""++v++"\",Pterm \""++x++"\",Pregla (split ' ' \""++rtrim (join ' ' (tabla_analisis_desc t p v x))++"\")):"|x<-(terminales t)++["$"],not (null (tabla_analisis_desc t p v x))])++(generar_tabla t vs p)
generar_parser::String->String->String->String
generar_parser t v p =  "import Utilidades\ndata Casilla = Pvar String|Pterm String|Pregla [String]\ntype Ttabla = [(Casilla,Casilla,Casilla)]\ntabla::Ttabla\ntabla="++(generar_tabla t (variables v) p)++
				"\naxioma::String\naxioma=\""++axioma++"\"\nterminales::[String]\nterminales = ["++(substr (concat["\""++x++"\","|x<-split ' ' t]) 1 (length (concat["\""++x++"\","|x<-split ' ' t])-1))++"]\nbuscaSigToken::String->[String]->Int->String\n"++
				"buscaSigToken [] _ _=\"$\"\nbuscaSigToken x y c|(member (substr x 1 c) y)=(substr x 1 c)\n        |otherwise = buscaSigToken x y (c+1)\nsigToken::String->[[Char]]\n"++
				"sigToken [] = [\"$\"]\nsigToken x = (buscaSigToken (trim x) (terminales++[\"$\"]) 1):(sigToken (substr (trim x) ((length (buscaSigToken (trim x) (terminales ++[\"$\"]) 1))+1) (length x)))\n"++
                                "\nesIgualCasilla x y (Pvar v,Pterm t,_)|x==v && y==t = True \n      |otherwise = False\ntabla_en x y = concat [m|(Pvar _,Pterm _, Pregla m)<-(filter (esIgualCasilla x y) tabla)]\n"++
			        "parser_ll1::[String]->[String]->String\nparser_ll1 (\"$\":[]) (\"$\":[]) = \"entrada correcta\"\nparser_ll1 [] x = \"error: entrada terminada antes de tiempo, estado \"++head(x)\n"++
				"parser_ll1 (x:xs) (y:ys) | x == y = parser_ll1 xs ys\n      |null (tabla_en y x) = \"error: no hay entrada en la tabla para el simbolo de entrada \"++x++\" estado \"++y\n"++
				"      |otherwise = parser_ll1 (x:xs) ((if not ((head(tabla_en y x))==\"lambda\") then (tabla_en y x) else [])++ys)\nmain:: IO ()\nmain = do\n        putStrLn \"Introduce una cadena:\"\n"++
				"        y <- getLine\n        putStrLn (parser_ll1 (sigToken y) (axioma:[\"$\"]))"

-- comprueba la cadena de entrada es del lenguaje generado
parser_desc::String->String->[String]->[String]->String
parser_desc _ _ ("$":[]) ("$":[]) = "entrada correcta"
parser_desc _ _ [] x = "error: entrada terminada antes de tiempo, estado "++head(x)
parser_desc term prod (x:xs) (y:ys) | x == y = parser_desc term prod xs ys
				   |null (tabla_analisis_desc term prod y x) = "error: no hay entrada en la tabla para el simbolo de entrada "++x++" estado "++y
				   |otherwise = parser_desc term prod (x:xs) ((if not ((head(tabla_analisis_desc term prod y x))=="lambda") then (tabla_analisis_desc term prod y x) else [])++ys)
buscaSigToken::String->[String]->Int->String
buscaSigToken [] _ _="$"
buscaSigToken x y c|(member (substr x 1 c) y)=(substr x 1 c)
                            |otherwise = buscaSigToken x y (c+1)
sigToken::String->String->[[Char]]
sigToken _ [] = ["$"]
sigToken term x = (buscaSigToken (trim x) ((terminales term)++["$"]) 1):(sigToken term (substr (trim x) ((length (buscaSigToken (trim x) ((terminales term)++["$"]) 1))+1) (length x)))

main:: IO ()
main = do
		[f1,f2] <- getArgs 
		h1 <- openFile f1 ReadMode
		--h2 <- openFile f2 ReadMode
		axioma  <- hGetLine h1
		term  <- hGetLine h1
		var  <- hGetLine h1
		prod <- hGetContents h1
		--y  <- hGetContents h2
		--putStrLn ("terminales="++concat(terminales term))
		--putStrLn ("reglas="++concat(map showRegla (producciones prod)))
		writeFile f2 (generar_parser term var prod)
		--putStrLn (parser_desc term prod (sigToken term y) (axioma:["$"]))
		hClose h1
		--hClose h2

