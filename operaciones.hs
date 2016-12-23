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
producciones x = (PIzda (substr x 1 ((posb "->" 1 1 x) - 1)),Pdcha (split ' ' (substr x ((posb "->" 1 1 x) +2) (if (pos (chr 10) 1 1 x)==0 then length x else ((pos (chr 10) 1 1 x)-((posb "->" 1 1 x) +2)))))):(producciones (if (pos (chr 10) 1 1 x)==0 then "" else (substr x ((pos (chr 10) 1 1 x)+1) (length x))))
axioma::String
axioma="E"
terminales::String->[String]
terminales x = split ' ' x
variables::String->[String]
variables x = split ' ' x
esIgualPizda::String->Regla->Bool
esIgualPizda x ((PIzda y),_)|x==y=True
                                     |otherwise = False
reglasde::String->Reglas->Reglas				
reglasde a p = filter (esIgualPizda a) p
contienePdcha::String->Regla->Bool
contienePdcha x ((PIzda a),(Pdcha y))|(pos x 1 1 y)>0 = True
                                                    |otherwise = False
reglascontienen::String->String->Reglas
reglascontienen a p = filter (contienePdcha a) (producciones p)
showPartes::Partes->String
showPartes (PIzda x) = x++" -> "
showPartes (Pdcha x) = rtrim (join ' ' x)
showRegla::Regla->String
showRegla x = showPartes (fst x) ++showPartes (snd x)++"\n" 
eliminar_rep::[String]->[String]
eliminar_rep []=[]
eliminar_rep (x:xs) = x:(eliminar_rep (delete x xs))

noexiste::Regla->Reglas->Bool
noexiste x y = not (member x y)
instance Eq Partes where
      (PIzda x)==(PIzda y) = x == y
      (Pdcha x)==(Pdcha y) = x == y
igualesReglas::Reglas->Reglas->Bool
igualesReglas [] _ = True
igualesReglas (x:xs) y |(member x y) = igualesReglas xs y
                       |otherwise = False
showPIzda::Partes->String
showPIzda (PIzda x) = x
showPdcha::Partes->String
showPdcha (Pdcha x)|(member "lambda"  x) = "lambda"
                             |otherwise = replace (rtrim (join ' ' (init x))) " " "\",\""
			     
-- encuentran las variables con reglas vacias y las que tienen reglas cuyas partes derechas estan formadas integramente por vbles anulables
var_anulables::[String]->[Regla]->[String]
var_anulables [] p =[]
var_anulables (x:xs) p |(es_anulable x p p) = x:(var_anulables xs p)
                              |otherwise = var_anulables xs p
es_anulable v [] p = False
es_anulable v ((PIzda x, Pdcha y):xs) p| (v==x && (head y)=="lambda") = True
						     | (v==x && not ((head y)=="lambda"))=if (length y)==length([n|n<-y, es_anulable n p p]) then True else es_anulable v xs p
						     | otherwise = es_anulable v xs p
-- elimina las reglas vacias, a partir de las variables anulables, quitando las reglas vacias, y las vbles. anulables de las partes derechas donde aparecen, quitando una cada vez						     
reglas_sin_lambda::[String]->[Regla]->[Regla]						     
reglas_sin_lambda var [] = []
reglas_sin_lambda var ((PIzda x, Pdcha y):xs)|(not ((head y)=="lambda"))=(PIzda x, Pdcha y):
                                                                                                       (case (cuenta_var_anulables y var) of
                                                                                                            0 -> (PIzda x,Pdcha y):reglas_sin_lambda var xs
													    1 -> if (length y)>1 then (PIzda x,Pdcha [n|n<-y,not (member n var)]):reglas_sin_lambda var xs else reglas_sin_lambda var xs
													    2 -> if (length y)>2 then (PIzda x,Pdcha [n|n<-y,not (member n var)]):(PIzda x,Pdcha (elimina_aparicion 1 var y 0)):(PIzda x,Pdcha (elimina_aparicion 2 var y 0)):reglas_sin_lambda var xs
																       else (PIzda x,Pdcha (elimina_aparicion 1 var y 0)):(PIzda x,Pdcha (elimina_aparicion 2 var y 0)):reglas_sin_lambda var xs 
													    3 -> if (length y)>3 then (PIzda x,Pdcha [n|n<-y,not (member n var)]):(PIzda x,Pdcha (elimina_aparicion 1 var y 0)):(PIzda x,Pdcha (elimina_aparicion 2 var y 0)):(PIzda x,Pdcha (elimina_aparicion 3 var y 0)):
													                                      (PIzda x,Pdcha (elimina_aparicion 1 var (elimina_aparicion 1 var y 0) 0)):(PIzda x,Pdcha (elimina_aparicion 2 var (elimina_aparicion 1 var y 0) 0)):(PIzda x,Pdcha (elimina_aparicion 2 var (elimina_aparicion 2 var y 0) 0)):reglas_sin_lambda var xs
														                       else  (PIzda x,Pdcha (elimina_aparicion 1 var y 0)):(PIzda x,Pdcha (elimina_aparicion 2 var y 0)):(PIzda x,Pdcha (elimina_aparicion 3 var y 0)):
													                                      (PIzda x,Pdcha (elimina_aparicion 1 var (elimina_aparicion 1 var y 0) 0)):(PIzda x,Pdcha (elimina_aparicion 2 var (elimina_aparicion 1 var y 0) 0)):(PIzda x,Pdcha (elimina_aparicion 2 var (elimina_aparicion 2 var y 0) 0)):reglas_sin_lambda var xs)
							     |otherwise = 	reglas_sin_lambda var xs
-- cuenta cuantos elementos de (x:xs) estan en var (2do param)							     
cuenta_var_anulables::[String]->[String]->Int							     
cuenta_var_anulables (x:xs) var = if (member x var) then 1 + (cuenta_var_anulables xs var) else cuenta_var_anulables xs var
cuenta_var_anulables [] var = 0
-- quita la variable n-ava de (y:ys)
elimina_aparicion::Int->[String]->[String]->Int->[String]
elimina_aparicion n var (y:ys) z|(member y var) && z==(n-1) = elimina_aparicion n var ys (z+1)
                                          |(member y var) && not (z==(n-1)) = y:(elimina_aparicion n var ys (z+1))
					  |otherwise=y:(elimina_aparicion n var ys z)
elimina_aparicion n var [] z = []
-- elimina las reglas que aparecieran repetidas en la lista
eliminar_reglas_repetidas::[Regla]->[Regla]
eliminar_reglas_repetidas ((PIzda x,Pdcha y):xs)=if ((length y)==1 && x==(head y)) then eliminar_reglas_repetidas xs else if (aparece_de_nuevo (PIzda x, Pdcha y) xs) then eliminar_reglas_repetidas xs else (PIzda x,Pdcha y):(eliminar_reglas_repetidas xs)
eliminar_reglas_repetidas [] = []
-- mira si la regla esta dos veces en la lista
aparece_de_nuevo::Regla->[Regla]->Bool
aparece_de_nuevo (PIzda x,Pdcha y) ((PIzda x1,Pdcha y1):xs)=if (x==x1 && y==y1) then True else aparece_de_nuevo (PIzda x,Pdcha y) xs
aparece_de_nuevo _ [] = False
--si encuentra una regla en cuya parte derecha solo hay una variable, construye nuevas reglas sustituyendo la parte derecha por las reglas de la vble
eliminar_reglas_unitarias::[String]->[Regla]->[Regla]->[Regla]
eliminar_reglas_unitarias var ((PIzda x, Pdcha y):xs) prod = if ((length y)==1 && (member (head y) var) && not (x==(head y))) then (devuelve_var_sustituida x (head y) prod)++(eliminar_reglas_unitarias var xs prod)
																				          else (PIzda x,Pdcha y):(eliminar_reglas_unitarias var xs prod)
eliminar_reglas_unitarias _ [] _ = []
-- devuelve las reglas formadas por la parte izda. de la regla unitaria (1er param) y las partes derechas de la variable que se esta sustituyendo (2do param.)
devuelve_var_sustituida::String->String->[Regla]->[Regla]																					
devuelve_var_sustituida x y ((PIzda x1,Pdcha y1):xs) = if (x1==y) then (PIzda x, Pdcha y1):(devuelve_var_sustituida x y xs)	else devuelve_var_sustituida x y xs
devuelve_var_sustituida _ _ [] = []
-- puede que tras eliminar unas reglas unitarias haya que eliminar las reglas de una variable sustituida, dado que ya no aparece en ninguna parte derecha
sustituir_reglas_unitarias::String->[String]->[Regla]->[Regla]
sustituir_reglas_unitarias axioma var p =(eliminar_reglas_no_accesibles (eliminar_reglas_unitarias var p p) (var_no_accesibles var (encuentra_var_accesibles [axioma] (eliminar_reglas_unitarias var p p) var))) 
-- elimina las reglas del primer parametro que tengan alguna vble en la parte izquierda o derecha en el conjunto de variables no accesibles
eliminar_reglas_no_accesibles::[Regla]->[String]->[Regla]
eliminar_reglas_no_accesibles ((PIzda x, Pdcha y):xs) var_no_acc = if ((member x var_no_acc)||length ([n|n<-y,(member n var_no_acc)])>0) then eliminar_reglas_no_accesibles xs var_no_acc
                                                                                                      else if (length y)==1 && (x==(head y)) then eliminar_reglas_no_accesibles xs var_no_acc else (PIzda x, Pdcha y):(eliminar_reglas_no_accesibles xs var_no_acc)
eliminar_reglas_no_accesibles [] _ = []	
aparece_en_pdcha::String->[Regla]->Bool
aparece_en_pdcha a ((PIzda x, Pdcha y):xs) = if(not (x==a) && (member a y)) then True else aparece_en_pdcha a xs
aparece_en_pdcha _ [] = False
-- a partir de las variables en el primer parametro, devuelve las variables que encuentra en la parte derecha de las reglas de esas variables
var_accesibles::[String]->[Regla]->[Regla]->[String]->[String]
var_accesibles (v:vs) ((PIzda x, Pdcha y):xs) p var  = if (v==x) then [n|n<-y,(member n var)]++(var_accesibles (v:vs) xs p var )
                                                                                      else var_accesibles (v:vs) xs p var 
var_accesibles (v:vs) [] p var = var_accesibles vs p p var 
var_accesibles [] _ _ _  = []
-- llama sucesivamente a var_accesibles hasta que se dejen de añadir variables nuevas al conjunto de variables accesibles
encuentra_var_accesibles::[String]->[Regla]->[String]->[String]
encuentra_var_accesibles a p var = if(incluido (var_accesibles a p p var) a) then (elimina_rep a) else encuentra_var_accesibles (elimina_rep ((var_accesibles a p p var)++a)) p var
-- devuelve True si los elementos de la primera lista estan todos en la segunda
incluido::[String]->[String]->Bool
incluido (x:xs) b = if (not (member x b)) then False else incluido xs b
incluido [] b = True
-- devuelve el conjunto formado por las variables que no estan en el conjunto de variables accesibles
var_no_accesibles (x:xs) var_acc = if(member x var_acc) then var_no_accesibles xs var_acc else x:(var_no_accesibles xs var_acc)
var_no_accesibles [] _ = []
nuevo_axioma::String->String
nuevo_axioma a = a++"P"
-- si el axioma esta en el cjto de vbles anulables, se construye un nuevo axioma y dos reglas para el nuevo axioma, una vacia y la otra que llama al antiguo axioma
obtener_reglas_sin_lambda::String->String->String->[Regla]
obtener_reglas_sin_lambda  axioma var prod   = if (member axioma (var_anulables (variables var) (producciones prod))) then (PIzda (nuevo_axioma axioma),Pdcha ["lambda"]):(PIzda (nuevo_axioma axioma),Pdcha [axioma]):(eliminar_reglas_repetidas (reglas_sin_lambda (var_anulables (variables var) (producciones prod)) (producciones prod)))
		                                                                                                                            else (eliminar_reglas_repetidas (reglas_sin_lambda (var_anulables (variables var) (producciones prod)) (producciones prod)))
-- ver si hay dos variables que tengan iguales las partes derechas de todas sus reglas, en tal caso se elimina una de las variables y sus reglas y se sustituyen sus apariciones en partes derechas por la primera variable
identificar_variables var p =if((length (encontrar_var_identificables var p))>0) then (sustituir_vars (encontrar_var_identificables var p) p p) else p
sustituir_vars::[String]->[Regla]->[Regla]->[Regla]
sustituir_vars (a:b:vs) ((PIzda x, Pdcha y):xs) p = if(not(x==b)) then (PIzda x,Pdcha (cambia_en y b a)):(sustituir_vars (a:b:vs) xs p) else sustituir_vars (a:b:vs) xs p
sustituir_vars (a:b:vs) [] p = sustituir_vars vs p p
sustituir_vars [] _ _ = []
encontrar_var_identificables::[String]->[Regla]->[String]
encontrar_var_identificables (v1:v2:vs) p | (iguales_pdcha (reglasde v1 p) (reglasde v2 p) (reglasde v2 p)) = [v1,v2]
                                                        | otherwise = (encontrar_var_identificables (v1:vs) p)++(encontrar_var_identificables (v2:vs) p)
encontrar_var_identificables (v:[]) _ = []	
iguales_pdcha::Reglas->Reglas->Reglas->Bool
iguales_pdcha ((PIzda x,Pdcha y):xs) ((PIzda x1,Pdcha y1):xs1) aux = if(y==y1) then (iguales_pdcha xs aux aux) else (iguales_pdcha ((PIzda x,Pdcha y):xs) xs1 aux)
iguales_pdcha _ [] _= False
iguales_pdcha [] _ _= True
cambia_en (x:xs) a b = if(x==a) then b:(cambia_en xs a b) else x:(cambia_en xs a b)
cambia_en []  _ _ = []
-- estan en CNF las reglas con un solo terminal y las formadas por 2 variables, en el resto de reglas se sustituyen los terminales por nueva variables y se añaden reglas nueva vble->terminal, terminando las
-- reglas originales de longitud>1 con todo variables
cnf_paso1::[Regla]->[String]->[Regla]																	    
cnf_paso1 ((PIzda x, Pdcha y):xs) var = if((length y)==1 ||((length y)==2 && (member (head y) var) && (member (y!!1) var))) then (PIzda x, Pdcha y):(cnf_paso1 xs var)
                                                                  else if(not ((length [n|n<-y,(member n var)])==(length y))) then [(PIzda x, Pdcha (sustituir_terminales y var))]++(nuevas_reglas y var)++(cnf_paso1 xs var)
								                                                                                   else (PIzda x, Pdcha y):(cnf_paso1 xs var)
cnf_paso1 [] _ = []																		   
sustituir_terminales (x:xs) var = if(member x var) then x:(sustituir_terminales xs var) else ("Z"++x):(sustituir_terminales xs var)
sustituir_terminales [] _ = []
nuevas_reglas (x:xs) var = if(member x var) then nuevas_reglas xs var else (PIzda ("Z"++x),Pdcha [x]):(nuevas_reglas xs var)
nuevas_reglas [] _ = []
-- se agrupan las variables de dos en dos: si la regla tiene 3 vbles. se sustituyen las dos ultimas por una nueva vble. y una nueva regla: nueva vble-> 2 vbles sustituidas,
-- si tiene mas de 3 voy sustituyendo las dos ultimas por una nueva  (junto con una nueva relga) sucesivamente, hasta obtener reglas con dos variables
cnf::[Regla]->[Regla]
cnf ((PIzda x, Pdcha y):xs) = if((length y)<=2) then (PIzda x,Pdcha y):(cnf xs)
                                                 else if ((length y)==3) then (PIzda x, Pdcha ((head y):["X"++(y!!1)++(y!!2)])):(PIzda ("X"++(y!!1)++(y!!2)),Pdcha (tail y)):(cnf xs)
								else (PIzda ("X"++(y!!((length y)-2))++(y!!((length y)-1))),Pdcha [y!!((length y)-2),y!!((length y)-1)]):(cnf ((PIzda x, Pdcha ((takeInt ((length y)-2) y)++["X"++(y!!((length y)-2))++(y!!((length y)-1))])):xs))
cnf []=[]	
-- devuelve todas las cadenas de longitud N, las cuales se obtienen, para las de longitud 1, directamente de las reglas y para las otras a partir de las reglas de dos vbles., concatenando las cadenas de ambas vbles
-- cuya suma de longitudes de el valor de longitud buscado
cyk::String->[Regla]->[Regla]->Int->Int->[String]
cyk s p ((PIzda x, Pdcha y):xs) n i|(s==x && n==1) = if((length y)==1) then y++(cyk s p xs n i) else cyk s p xs n i
                                              |(s==x) = [y1|y1<- (junta_cadenas	(filter (longitud_n (n-i)) (cyk (y!!0) p p (n-i) 1)) (filter (longitud_n i) (cyk (y!!1) p p i 1)))]++(cyk s p xs n i)
					      |otherwise = cyk s p xs n i
cyk s p [] n i = if ((i+1)<n) then cyk s p p n (i+1) else []
longitud_n::Int->String->Bool
longitud_n n x = if ((length x)==n) then True else False
junta_cadenas::[String]->[String]->[String]
junta_cadenas (x:xs) (y:ys) = junta_cadenas2 (x:xs) (y:ys) (x:xs)
junta_cadenas [] _ = []
junta_cadenas _ [] = []
junta_cadenas2 (x:xs) (y:ys) aux = (x++y):(junta_cadenas2 xs (y:ys) aux)
junta_cadenas2 [] (y:ys) aux = junta_cadenas2 aux ys aux
junta_cadenas2 _ [] _ = []

fng_paso1 (x:xs) var p = if (hay_rec_izda (ordenar_no_terminales var (reglasde x p) p)) then (eliminar_rec_izda (ordenar_no_terminales var (reglasde x p) p) var)++(fng_paso1 xs var ((eliminar_rec_izda (ordenar_no_terminales var (reglasde x p) p) var)++(resto_reglas x p))) 
                                                                                                                       else (ordenar_no_terminales var (reglasde x p) p)++(fng_paso1 xs var ((ordenar_no_terminales var (reglasde x p) p)++(resto_reglas x p)))
fng_paso1 [] var p = [] 
ordenar_no_terminales::[String]->[Regla]->[Regla]->[Regla]
ordenar_no_terminales var ((PIzda x, Pdcha y):xs) p = if((member (head y) var) && ((pos (head y) 1 1 var)<(pos x 1 1 var))) then (sustituye_por_regla x y (reglasde (head y) p))++(ordenar_no_terminales var xs p)
																					else (PIzda x, Pdcha y):(ordenar_no_terminales var xs p)
ordenar_no_terminales _ [] _ = []
sustituye_por_regla::String->[String]->[Regla]->[Regla]
sustituye_por_regla x y ((PIzda _, Pdcha y1):xs) = (PIzda x, Pdcha (y1++(tail y))):(sustituye_por_regla x y xs)
sustituye_por_regla _ _ [] = []
eliminar_rec_izda::[Regla]->[String]->[Regla]
eliminar_rec_izda ((PIzda x,Pdcha y):xs) var= if(x==(head y)) then (PIzda (x++(concat y)), Pdcha (tail y)):(PIzda (x++(concat y)),Pdcha ((tail y)++[(x++(concat y))])):(eliminar_rec_izda xs (var++[(x++(concat y))]))
                                                                                 else (PIzda x, Pdcha (y++[(last var)])):(eliminar_rec_izda xs var)
eliminar_rec_izda [] _ = []
hay_rec_izda::[Regla]->Bool
hay_rec_izda ((PIzda x, Pdcha y):xs) = if(x==(head y)) then True else hay_rec_izda xs
hay_rec_izda [] = False
resto_reglas::String->[Regla]->[Regla]
resto_reglas v ((PIzda x,Pdcha y):xs) = if(v==x) then resto_reglas v xs else (PIzda x,Pdcha y):(resto_reglas v xs)
resto_reglas _ [] = []
quedan_var_menores var ((PIzda x,Pdcha y):xs) = if((member (head y) var) && ((pos (head y) 1 1 var)<(pos x 1 1 var))) then True else quedan_var_menores var xs
quedan_var_menores var [] = False
nuevas_variables var ((PIzda x, Pdcha y):xs) = if(member x var) then nuevas_variables var xs else x:(nuevas_variables (var++[x]) xs)
nuevas_variables _ [] =[]
fng_paso2::[String]->[String]->[Regla]->[Regla]
fng_paso2 var (x:xs) p = (fusiona_prod var (reglasde x p) p)++(fng_paso2 var xs ((fusiona_prod var (reglasde x p) p)++(resto_reglas x p)))
fng_paso2 _ [] _ = []
fusiona_prod::[String]->[Regla]->[Regla]->[Regla]
fusiona_prod var ((PIzda x,Pdcha y):xs) p = if(member (head y) var) then (sustituye_por_regla x y (reglasde (head y) p))++(fusiona_prod var xs p)
											    else (PIzda x, Pdcha y):(fusiona_prod var xs p)
fusiona_prod _ [] _ = []	
fng_1b var p = if (quedan_var_menores var (fng_paso1 var var p)) then (fng_1b ((nuevas_variables var (fng_paso1 var var p))++var) (fng_paso1 var var p))
                                                                                     else (fng_paso1 var var p)
-- FNG, donde todas las reglas empiezan por un terminal y estan seguidas solo de No terminales y ademas puede existir S->lambda
fng::[String]->[Regla]->[Regla]
fng var p = (fng_paso2 (var++(nuevas_variables var (fng_1b var p))) ((reverse var)++(nuevas_variables var (fng_1b var p))) (fng_1b var p))										     
-- funcion de transicion de un AP que reconoce por vaciado de pila la cadena de entrada (3er param), la funcion de transicion del AP no determinista se construye a partir de la gramatica de tipo 2
-- en forma Normal de Greibach (FNG), p.ej. una regla en la gramatica como A->2 B C se convierte en f(q,2,A)=(q,BC) en el AP
f::[String]->[Regla]->[String]->[String]->Bool
f _ _ [] [] = True
f var p (a:as) (x:xs)= if(x==axioma && (member x (var_anulables var p))) then (f var p (a:as) xs) || (if (cuentaVerd(map (f var p as) (sig_elemento_pila a (reglasde x p) xs))>0) then True else False)
                                                                                                   else (if (cuentaVerd(map (f var p as) (sig_elemento_pila a (reglasde x p) xs))>0) then True else False)
f _ _ [] _ = False
f _ _ _ [] = False
sig_elemento_pila::String->[Regla]->[String]->[[String]]
sig_elemento_pila a ((PIzda _, Pdcha y):xs) pila |(a==(head y)) = ((tail y)++pila):(sig_elemento_pila a xs pila)
                                                                |otherwise = sig_elemento_pila a xs pila
sig_elemento_pila _ [] _ = []								
main:: IO ()
main2 = do
		[f1,f2] <- getArgs 
		h1 <- openFile f1 ReadMode
		axioma  <- hGetLine h1
		term  <- hGetLine h1
		var  <- hGetLine h1
		prod <- hGetContents h1
		putStrLn ("axioma="++axioma)
		putStrLn ("terminales="++concat(terminales term))
		putStrLn ("variables="++concat(variables var))
		putStrLn ("var. anulables="++concat((var_anulables (variables var) (producciones prod))))
		writeFile f2 ("reglas="++concat(map showRegla (producciones prod)))
		appendFile f2 "----------------sin lambda--------------------------\n"
		appendFile f2 (concat (map showRegla (obtener_reglas_sin_lambda axioma var prod)))
		appendFile f2 "----------------primer paso:eliminar reglas unitarias--------------------------\n"
		appendFile f2 (concat (map showRegla (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (obtener_reglas_sin_lambda axioma var prod))))
		appendFile f2 ("var. accesibles="++concat(encuentra_var_accesibles (if (member axioma (var_anulables (variables var) (producciones prod))) then [(nuevo_axioma axioma)] else [axioma])
		                                                                                       (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (obtener_reglas_sin_lambda axioma var prod))
		                                                                                      (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)))++"\n")
		appendFile f2 "---------------segundo paso:eliminar reglas unitarias---------------------------\n"
		appendFile f2 (concat (map showRegla (eliminar_reglas_repetidas (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (obtener_reglas_sin_lambda axioma var prod))))))
		appendFile f2 ("var. accesibles="++concat(encuentra_var_accesibles (if (member axioma (var_anulables (variables var) (producciones prod))) then [(nuevo_axioma axioma)] else [axioma])
		                                                                                       (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (obtener_reglas_sin_lambda axioma var prod)))
		                                                                                      (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)))++"\n")
		appendFile f2 "---------------tercer paso:eliminar reglas unitarias---------------------------\n"
		appendFile f2 (concat (map showRegla (eliminar_reglas_repetidas (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (obtener_reglas_sin_lambda axioma var prod)))))))
		appendFile f2 ("var. accesibles="++concat(encuentra_var_accesibles (if (member axioma (var_anulables (variables var) (producciones prod))) then [(nuevo_axioma axioma)] else [axioma])
		                                                                                       (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (obtener_reglas_sin_lambda axioma var prod))))
		                                                                                      (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)))++"\n")
		appendFile f2 "---------------CNF---------------------------\n"
		appendFile f2 (concat (map showRegla (eliminar_reglas_repetidas (cnf (cnf_paso1 (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (obtener_reglas_sin_lambda axioma var prod))))
		                                                                                                             (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)))))))
		appendFile f2 "---------------Identificar vbles---------------------------\n"
		appendFile f2 (concat (map showRegla (identificar_variables (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var))(eliminar_reglas_repetidas (cnf (cnf_paso1 (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (obtener_reglas_sin_lambda axioma var prod))))
		                                                                                                             (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var))))))))
		putStrLn "Introduce cadena:"
		cadena <- getLine
                appendFile f2 "---------------Cadenas longitud:n---------------------------\n"
		appendFile f2 (join ' '  (eliminar_rep (filter (longitud_n (length cadena)) (cyk (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (eliminar_reglas_repetidas (cnf (cnf_paso1 (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (obtener_reglas_sin_lambda axioma var prod))))
		                                                                                                             (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var))))) (eliminar_reglas_repetidas (cnf (cnf_paso1 (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (obtener_reglas_sin_lambda axioma var prod))))
		                                                                                                             (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var))))) (length cadena) 1))))
		putStrLn (if (member cadena (eliminar_rep (filter (longitud_n (length cadena)) (cyk (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (eliminar_reglas_repetidas (cnf (cnf_paso1 (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (obtener_reglas_sin_lambda axioma var prod))))
		                                                                                                             (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var))))) (eliminar_reglas_repetidas (cnf (cnf_paso1 (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (sustituir_reglas_unitarias (if (member axioma (var_anulables (variables var) (producciones prod))) then (nuevo_axioma axioma) else axioma) (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var)) (obtener_reglas_sin_lambda axioma var prod))))
		                                                                                                             (if (member axioma (var_anulables (variables var) (producciones prod))) then (variables var)++[(nuevo_axioma axioma)] else (variables var))))) (length cadena) 1)))) then cadena++" esta en el lenguaje" else cadena++" no esta en el lenguaje")
		hClose h1
														    
main3 = do
		[f1,f2] <- getArgs 
		h1 <- openFile f1 ReadMode
		axioma  <- hGetLine h1
		term  <- hGetLine h1
		var  <- hGetLine h1
		prod <- hGetContents h1
		putStrLn ("axioma="++axioma)
		putStrLn ("terminales="++concat(terminales term))
		putStrLn ("variables="++concat(variables var))
		writeFile f2 ("reglas="++concat(map showRegla (producciones prod)))
		appendFile f2 "----------------FNG-------------------\n"
		appendFile f2 (concat (map showRegla (fng (variables var) (producciones prod))))
		hClose h1

main = do
		[f1] <- getArgs 
		h1 <- openFile f1 ReadMode
		axioma  <- hGetLine h1
		term  <- hGetLine h1
		var  <- hGetLine h1
		prod <- hGetContents h1
		putStrLn ("axioma="++axioma)
		putStrLn ("terminales="++concat(terminales term))
		putStrLn ("variables="++concat(variables var))
		putStrLn ("reglas="++concat(map showRegla (producciones prod)))
		putStrLn "Introduce cadena:"
		cadena <- getLine
		putStrLn (if (f (variables var) (producciones prod) (split ' ' (rtrim cadena)) [axioma]) then cadena++" pertenece al lenguaje" else cadena++" no pertenece")
		hClose h1
