import Utilidades
data Casilla = Pvar String|Pterm String|Pregla [String]
type Ttabla = [(Casilla,Casilla,Casilla)]
tabla::Ttabla
tabla=(Pvar "E",Pterm "n",Pregla (split ' ' "T EP")):(Pvar "E",Pterm "(",Pregla (split ' ' "T EP")):(Pvar "T",Pterm "n",Pregla (split ' ' "F TP")):(Pvar "T",Pterm "(",Pregla (split ' ' "F TP")):(Pvar "EP",Pterm "+",Pregla (split ' ' "+ T EP")):(Pvar "EP",Pterm ")",Pregla (split ' ' "lambda")):(Pvar "EP",Pterm "$",Pregla (split ' ' "lambda")):(Pvar "TP",Pterm "+",Pregla (split ' ' "lambda")):(Pvar "TP",Pterm "*",Pregla (split ' ' "* F TP")):(Pvar "TP",Pterm ")",Pregla (split ' ' "lambda")):(Pvar "TP",Pterm "$",Pregla (split ' ' "lambda")):(Pvar "F",Pterm "n",Pregla (split ' ' "n")):(Pvar "F",Pterm "(",Pregla (split ' ' "( E )")):[]
axioma::String
axioma="E"
terminales::[String]
terminales = ["n","+","*","(",")"]
buscaSigToken::String->[String]->Int->String
buscaSigToken [] _ _="$"
buscaSigToken x y c|(member (substr x 1 c) y)=(substr x 1 c)
        |otherwise = buscaSigToken x y (c+1)
sigToken::String->[[Char]]
sigToken [] = ["$"]
sigToken x = (buscaSigToken (trim x) (terminales++["$"]) 1):(sigToken (substr (trim x) ((length (buscaSigToken (trim x) (terminales ++["$"]) 1))+1) (length x)))

esIgualCasilla x y (Pvar v,Pterm t,_)|x==v && y==t = True 
      |otherwise = False
tabla_en x y = concat [m|(Pvar _,Pterm _, Pregla m)<-(filter (esIgualCasilla x y) tabla)]
parser_ll1::[String]->[String]->String
parser_ll1 ("$":[]) ("$":[]) = "entrada correcta"
parser_ll1 [] x = "error: entrada terminada antes de tiempo, estado "++head(x)
parser_ll1 (x:xs) (y:ys) | x == y = parser_ll1 xs ys
      |null (tabla_en y x) = "error: no hay entrada en la tabla para el simbolo de entrada "++x++" estado "++y
      |otherwise = parser_ll1 (x:xs) ((if not ((head(tabla_en y x))=="lambda") then (tabla_en y x) else [])++ys)
main:: IO ()
main = do
        putStrLn "Introduce una cadena:"
        y <- getLine
        putStrLn (parser_ll1 (sigToken y) (axioma:["$"]))