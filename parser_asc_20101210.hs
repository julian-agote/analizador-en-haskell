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
producciones::String->Reglas
producciones []=[]
producciones x = (PIzda (rtrim (substr x 1 ((posb "->" 1 1 x) - 1))),Pdcha (split ' ' (trim (substr x ((posb "->" 1 1 x) +2) (if (pos (chr 10) 1 1 x)==0 then length x else ((pos (chr 10) 1 1 x)-((posb "->" 1 1 x) +2))))))):(producciones (if (pos (chr 10) 1 1 x)==0 then "" else (substr x ((pos (chr 10) 1 1 x)+1) (length x))))
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
showRegla x = showPartes (fst x) ++showPartes (snd x) ++"\n" 
primerodcha::String->String->Partes->[String]
primerodcha _ _ (Pdcha []) = []
primerodcha term prod (Pdcha x) = primero term prod (head x)++(if (member "lambda" (primero term prod (head x))) then (primerodcha term prod (Pdcha (tail x))) else [])
primero::String->String->String->[String]
primero term prod x |(member x ((terminales term)++["lambda"])) = [x]
		           |otherwise = concat ([primerodcha term prod (Pdcha n)|(PIzda z,Pdcha n)<- (reglasde x prod),not(x==(head n))])
primero_cad::String->String->[String]->[String]
primero_cad term prod x = if(todos_tienen_lambda term prod x) then (delete "lambda" (elimina_rep (primero_cad2 term prod x))) else (elimina_rep (primero_cad2 term prod x))
primero_cad2 term prod (x:xs) = primero term prod x ++ (if (member "lambda" (primero term prod x)) then primero_cad2 term prod xs else [])
primero_cad2 _ _ [] = []
todos_tienen_lambda::String->String->[String]->Bool
todos_tienen_lambda term prod (x:xs) = if(member "lambda" (primero term prod x)) then todos_tienen_lambda term prod xs else False
todos_tienen_lambda term prod [] = True
--se define Siguiente(A) para el no terminal A como el cjto. de terminales que pueden aparecer a la dcha de A
--en alguna forma sentencial derivada del axioma de la gramatica
siguiente::String->String->String->String->[String]
siguiente axioma term prod x |x==axioma = "$":(elimina_rep (concat [if (pos x 1 1 n)<(length n) then primero_cad term prod (dropInt (pos x 1 1 n) n)++(if (member "lambda" (primero_cad term prod (dropInt (pos x 1 1 n) n))) then if not(x==y) then (siguiente axioma term prod y) else [] else []) else if not(x==y) then (siguiente axioma term prod y) else []|(PIzda y, Pdcha n)<-reglascontienen x prod]))
			     |otherwise = elimina_rep (concat [if (pos x 1 1 n)<(length n) then primero_cad term prod (dropInt (pos x 1 1 n) n)++(if (member "lambda" (primero_cad term prod (dropInt (pos x 1 1 n) n))) then if not(x==y) then (siguiente axioma term prod y) else [] else []) else if not(x==y) then (siguiente axioma term prod y) else []|(PIzda y, Pdcha n)<-reglascontienen x prod])
mostrar_primeros::[String]->String->String->String			     
mostrar_primeros (v:vs) term prod = "primero("++v++")={"++(join ',' (elimina_rep (primero term prod v)))++"}\n"++(mostrar_primeros vs term prod)
mostrar_primeros [] _ _ = ""
mostrar_siguientes::[String]->String->String->String->String	
mostrar_siguientes (v:vs) axioma term prod = "siguiente("++v++")={"++(join ',' (siguiente axioma term prod v))++"}\n"++(mostrar_siguientes vs axioma term prod)
mostrar_siguientes [] _ _ _ = ""
data Elemento =  Elemento {numero::Int, conjunto::[Regla]}
showElemento::[Elemento]->[String]
showElemento []=[]
showElemento ((Elemento {numero=n,conjunto=c}):xs) = (show n:(map showRegla c))++(showElemento xs)
ir_a::String->String->String->Elemento->String->Reglas
ir_a t v p (Elemento {numero=n,conjunto=c}) x= cerradura t v p [(PIzda r, Pdcha (avanzarpunto m))|(PIzda r, Pdcha m)<-(filter (contienePdcha x) c),m!!(if ((pos x 1 1 m)==1) then 0 else ((pos x 1 1 m)-2))=="."]
avanzarpunto::[String]->[String]
avanzarpunto x = (takeInt ((pos "." 1 1 x)-1) x)++[x!!(pos "." 1 1 x)]++["."]++(dropInt ((pos "." 1 1 x)+1) x)
cerradura::String->String->String->Reglas->Reglas	    
cerradura _ _ _ [] = []
cerradura t v p ((PIzda x,Pdcha y):xs) | (pos "." 1 1 y)<(length y) && (member (y!!(pos "." 1 1 y)) (variables v)) = [(PIzda x,Pdcha y)]++(cerradura t v p ([n|n<-(anadirpunto (reglasde (y!!(pos "." 1 1 y)) p)),noexiste n xs]++xs))
                                 | otherwise = [(PIzda x,Pdcha y)]++(cerradura t v p xs)
anadirpunto::Reglas->Reglas
anadirpunto [] = []
anadirpunto ((PIzda x,Pdcha y):xs) = [(PIzda x,Pdcha (".":y))]++(anadirpunto xs)
noexiste::Regla->Reglas->Bool
noexiste x y = not (member x y)
instance Eq Partes where
      (PIzda x)==(PIzda y) = x == y
      (Pdcha x)==(Pdcha y) = x == y
igualesReglas::Reglas->Reglas->Bool
igualesReglas [] _ = True
igualesReglas (x:xs) y |(member x y) = igualesReglas xs y
                       |otherwise = False
pertenece::Reglas->[Elemento]->Int->Int
pertenece _ [] n=n
pertenece c ((Elemento {numero=_,conjunto=x}):xs) n |(igualesReglas c x) && (igualesReglas x c) = n
                                                    |otherwise = pertenece c xs (n+1)
colec_slr::String->String->String->[Elemento]->[Elemento]->[Elemento]
colec_slr _ _ _ [] y = y      
colec_slr t v p (x:xs) y = colec_slr t v p ([Elemento {numero=(pertenece n y 1) , conjunto=n}|n<-(map (ir_a t v p x) ((terminales t)++(variables v))),(length n)>0 && (pertenece n y 1)>(length y)]++xs) ([Elemento {numero=(pertenece n y 1) , conjunto=n}|n<-(map (ir_a t v p x) ((terminales t)++(variables v))),(length n)>0&& (pertenece n y 1)>(length y)]++y)
cambiar_numero::[Elemento]->[Elemento]
cambiar_numero (x:[])=[x]
cambiar_numero ((Elemento {numero=n1,conjunto=c1}):(Elemento {numero=n2,conjunto=c2}):xs)|n2<=n1=(Elemento {numero=n1,conjunto=c1}):(cambiar_numero ((Elemento {numero=(n1+1),conjunto=c2}):xs))
                                                                                         |otherwise=(Elemento {numero=n1,conjunto=c1}):(cambiar_numero ((Elemento {numero=n2,conjunto=c2}):xs))
calc_colec_slr::String->String->String->[Elemento]->[Elemento] 
calc_colec_slr t v p x = cambiar_numero (reverse (colec_slr t v p x x))
data Elem_acc =  Elem_acc {estado::Int, term::String, accion::String, regla::Regla, sig_estado::Int}
tabla_acc::String->String->String->String->[Elemento]->[Elem_acc]
tabla_acc _ _ _ _ []=[]
tabla_acc axioma t v p ((Elemento {numero=n,conjunto=c}):xs)= (concat [[if(x=="P"++axioma && z=="$") then (Elem_acc {estado=n,term=z,accion="aceptar",regla=(PIzda x,Pdcha y),sig_estado=0}) else (Elem_acc {estado=n,term=z,accion="reducir",regla=(PIzda x,Pdcha y),sig_estado=0})|z<-(delete "lambda" (siguiente axioma t p x))]|(PIzda x,Pdcha y)<-reducir c])++
                                                                      [(Elem_acc {estado = n,term = y!!(pos "." 1 1 y),accion="desplazar",regla=(PIzda axioma,Pdcha []),sig_estado =(pertenece (ir_a t v p (Elemento {numero=n,conjunto=c}) (y!!(pos "." 1 1 y))) (calc_colec_slr t v p [Elemento {numero = 1,conjunto = cerradura t v p ((PIzda ("P"++axioma),Pdcha [".",axioma]):[])}]) 1)})|(PIzda x,Pdcha y)<-desplazar (terminales t) c]++
								      (tabla_acc axioma t v p xs)
reducir::[Regla]->[Regla]
reducir []=[]
reducir ((PIzda x,Pdcha y):xs)|(puntoAlFinal (Pdcha y))=(PIzda x,Pdcha y):reducir xs
                              |otherwise=reducir xs
puntoAlFinal::Partes->Bool
puntoAlFinal (Pdcha y)=if ((pos "." 1 1 y)==(length y) || (y!!1)=="lambda") then True else False
desplazar::[String]->[Regla]->[Regla]
desplazar _ []=[]
desplazar t ((PIzda x,Pdcha y):xs)|(punto_delante_terminal t (Pdcha y))=(PIzda x,Pdcha y):desplazar t xs
					      |otherwise=desplazar t xs
punto_delante_terminal::[String]->Partes->Bool
punto_delante_terminal t (Pdcha y)=if ((pos "." 1 1 y)<(length y) && (member (y!!(pos "." 1 1 y)) t)) then True else False
showElem_acc::[Elem_acc]->String
showElem_acc []=""
showElem_acc ( Elem_acc {estado=n, term=z, accion=acc, regla=r, sig_estado=m}:xs)|(acc=="reducir")=show n++": en " ++z++" reducir por "++(showRegla r)++" "++(showElem_acc xs)
                                                                                                                     |(acc=="desplazar")=show n++": en " ++z++" desplazar a "++show m++" "++(showElem_acc xs)
mostrar_tabla_accion::[[Elem_acc]]->String
mostrar_tabla_accion []=""
mostrar_tabla_accion (x:xs) = (showElem_acc x)++(mostrar_tabla_accion xs)
generar_tabla::[Elem_acc]->String
generar_tabla []="[]"
generar_tabla (Elem_acc {estado=n, term=z, accion=acc, regla=r, sig_estado=m}:xs)|(acc=="reducir")="(Elem_acc {estado="++show n++",term=\"" ++z++"\",accion=\"reducir\",regla=(PIzda \""++showPIzda (fst r)++"\",Pdcha [\""++showPdcha (snd r)++"\"]), sig_estado=0}):\n       "++(generar_tabla xs)
                                                                                                                    |(acc=="desplazar")="(Elem_acc {estado="++show n++",term=\"" ++z++"\",accion=\"desplazar\",regla=(PIzda \"A\",Pdcha [\"lambda\"]), sig_estado="++show m++"}):\n       "++(generar_tabla xs)
														    |(acc=="aceptar")="(Elem_acc {estado="++show n++",term=\"" ++z++"\",accion=\"aceptar\",regla=(PIzda \"A\",Pdcha [\"lambda\"]), sig_estado=0}):\n       "++(generar_tabla xs)
showPIzda::Partes->String
showPIzda (PIzda x) = x
showPdcha::Partes->String
showPdcha (Pdcha x)|(member "lambda"  x) = "lambda"
                             |otherwise = replace (rtrim (join ' ' (init x))) " " "\",\""
data Elem_ira =  Elem_ira {estado2::Int, var::String, sig_estado2::Int}
tabla_ira::String->String->String->String->[Elemento]->[Elem_ira]
tabla_ira _ _ _ _ []=[]
tabla_ira axioma t v p ((Elemento {numero=n,conjunto=c}):xs)= [(Elem_ira {estado2 = n,var = y!!(pos "." 1 1 y),sig_estado2 =(pertenece (ir_a t v p (Elemento {numero=n,conjunto=c}) (y!!(pos "." 1 1 y))) (calc_colec_slr t v p [Elemento {numero = 1,conjunto = cerradura t v p ((PIzda ("P"++axioma),Pdcha [".",axioma]):[])}]) 1)})|(PIzda x,Pdcha y)<-desplazar (variables v) c]++
								             (tabla_ira axioma t v p xs)
generar_tabla_ira::[Elem_ira]->String
generar_tabla_ira []="[]"
generar_tabla_ira (Elem_ira {estado2=n, var=z, sig_estado2=m}:xs)="(Elem_ira {estado2="++show n++",var=\"" ++z++"\",sig_estado2="++show m++"}):\n       "++(generar_tabla_ira xs)
generar_parser::String->String->String->String->String
generar_parser axioma t v p =  "import Utilidades\ndata Partes = PIzda String|Pdcha [String]\ntype Regla=(Partes,Partes)\ndata Elem_acc =  Elem_acc {estado::Int, term::String, accion::String, regla::Regla, sig_estado::Int}\ntabla_acc::[Elem_acc]\n"++
                                "tabla_acc="++(generar_tabla (tabla_acc axioma t v p (calc_colec_slr t v p [Elemento {numero = 1,conjunto = cerradura t v p ((PIzda ("P"++axioma),Pdcha [".",axioma]):[])}])))++
				"\ndata Elem_ira =  Elem_ira {estado2::Int, var::String, sig_estado2::Int}\ntabla_ira::[Elem_ira]\ntabla_ira="++(generar_tabla_ira (tabla_ira axioma t v p (calc_colec_slr t v p [Elemento {numero = 1,conjunto = cerradura t v p ((PIzda ("P"++axioma),Pdcha [".",axioma]):[])}])))++
				"\naxioma::String\naxioma=\""++axioma++"\"\nterminales::[String]\nterminales = ["++(substr (concat["\""++x++"\","|x<-split ' ' t]) 1 (length (concat["\""++x++"\","|x<-split ' ' t])-1))++"]\n"
			--   ++"buscaSigToken::String->[String]->Int->String\nbuscaSigToken [] _ _=\"$\"\nbuscaSigToken x y c|(member (substr x 1 c) y)=(substr x 1 c)\n        |otherwise = buscaSigToken x y (c+1)\nsigToken::String->[[Char]]\n"++
			--	"sigToken [] = [\"$\"]\nsigToken x = (buscaSigToken (trim x) (terminales++[\"$\"]) 1):(sigToken (substr (trim x) ((length (buscaSigToken (trim x) (terminales ++[\"$\"]) 1))+1) (length x)))\n"

			     
main:: IO ()
main = do
		[f1,f2] <- getArgs 
		--[f1] <- getArgs 
		h1 <- openFile f1 ReadMode
		axioma  <- hGetLine h1
		term  <- hGetLine h1
		var  <- hGetLine h1
		prod <- hGetContents h1
		putStrLn ("terminales="++(join ' ' (terminales term)))
		putStrLn ("variables="++(join ' ' (variables var)))
		putStrLn ("reglas="++concat(map showRegla (producciones prod)))
		--putStrLn ("tabla accion="++generar_tabla (tabla_acc axioma term var prod (calc_colec_slr term var prod [Elemento {numero = 1,conjunto = (cerradura term var prod ((PIzda ("P"++axioma),Pdcha [".",axioma]):[]))}])))
		--putStrLn (mostrar_primeros (variables var) term prod)
		--putStrLn (mostrar_siguientes (variables var) axioma term prod)
		writeFile f2 (generar_parser axioma term var prod)
		hClose h1
														    
