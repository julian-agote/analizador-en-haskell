import Data.Char(ord)
import Data.List(delete,reverse)
import System.IO
import System.Environment
import Data.Char
import Utilidades

data Partes = PIzda String
          |Pdcha [String]
type Regla=(Partes,Partes)
type Reglas = [Regla]
producciones::String->String->Reglas
producciones [] axioma=[(PIzda ("P"++axioma),Pdcha [axioma])]
producciones x axioma= (PIzda (rtrim (substr x 1 ((posb "->" 1 1 x) - 1))),Pdcha (split ' ' (trim (substr x ((posb "->" 1 1 x) +2) (if (pos (chr 10) 1 1 x)==0 then length x else ((pos (chr 10) 1 1 x)-((posb "->" 1 1 x) +2))))))):(producciones (if (pos (chr 10) 1 1 x)==0 then "" else (substr x ((pos (chr 10) 1 1 x)+1) (length x))) axioma)
terminales::String->[String]
terminales x = split ' ' x
variables::String->[String]
variables x = split ' ' x
esIgualPizda::String->Regla->Bool
esIgualPizda x ((PIzda y),_)|x==y=True
                                     |otherwise = False
reglasde::String->String->String->Reglas                
reglasde a p axioma= filter (esIgualPizda a) (producciones p axioma)
contienePdcha::String->Regla->Bool
contienePdcha x ((PIzda a),(Pdcha y))|(pos x 1 1 y)>0 = True
                                                    |otherwise = False
reglascontienen::String->String->String->Reglas
reglascontienen a p axioma= filter (contienePdcha a) (producciones p axioma)
showPartes::Partes->String
showPartes (PIzda x) = x++" -> "
showPartes (Pdcha x) = rtrim (join ' ' x)
showRegla::Regla->String
showRegla x = showPartes (fst x) ++showPartes (snd x) ++"\n" 
primerodcha::String->String->String->Partes->[String]
primerodcha _ _ _ (Pdcha []) = []
primerodcha term prod axioma (Pdcha x) = primero term prod axioma (head x)++(if (member "lambda" (primero term prod axioma (head x))) then (primerodcha term prod axioma (Pdcha (tail x))) else [])
primero::String->String->String->String->[String]
primero term prod axioma x |(member x ((terminales term)++["lambda"])) = [x]
                             |otherwise = concat ([primerodcha term prod axioma (Pdcha n)|(PIzda z,Pdcha n)<- (reglasde x prod axioma),not(x==(head n))])
primero_cad::String->String->String->[String]->[String]
primero_cad term prod axioma x = if(todos_tienen_lambda term prod axioma x) then (delete "lambda" (elimina_rep (primero_cad2 term prod axioma x))) else (elimina_rep (primero_cad2 term prod axioma x))
primero_cad2 term prod axioma (x:xs) = primero term prod axioma x ++ (if (member "lambda" (primero term prod axioma x)) then primero_cad2 term prod axioma xs else [])
primero_cad2 _ _ _ [] = []
todos_tienen_lambda::String->String->String->[String]->Bool
todos_tienen_lambda term prod axioma (x:xs) = if(member "lambda" (primero term prod axioma x)) then todos_tienen_lambda term prod axioma xs else False
todos_tienen_lambda term prod _ [] = True
--se define Siguiente(A) para el no terminal A como el cjto. de terminales que pueden aparecer a la dcha de A
--en alguna forma sentencial derivada del axioma de la gramatica
siguiente::String->String->String->String->[(String,[String])]->[String]
siguiente axioma term prod x sig_obt|x==("P"++axioma) = "$":(elimina_rep (concat [concat [if (pos x 1 t n)<(length n) then (if (member "lambda" (primero_cad2 term prod axioma (dropInt (pos x 1 t n) n))) && not(x==y) then (if(dev_sig sig_obt y)/=[] then (primero_cad term prod axioma (dropInt (pos x 1 t n) n))++(dev_sig sig_obt y) else []) else (primero_cad term prod axioma (dropInt (pos x 1 t n) n))) else if not(x==y) then (dev_sig sig_obt y) else []|t<-[1..2],(pos x 1 t n)>0]|(PIzda y, Pdcha n)<-reglascontienen x prod axioma]))
                                    |otherwise = (elimina_rep (concat [concat [if (pos x 1 t n)<(length n) then (if (member "lambda" (primero_cad2 term prod axioma (dropInt (pos x 1 t n) n))) && not(x==y) then (if(dev_sig sig_obt y)/=[] then (primero_cad term prod axioma (dropInt (pos x 1 t n) n))++(dev_sig sig_obt y) else []) else (primero_cad term prod axioma (dropInt (pos x 1 t n) n))) else if not(x==y) then (dev_sig sig_obt y) else []|t<-[1..2],(pos x 1 t n)>0]|(PIzda y, Pdcha n)<-reglascontienen x prod axioma]))
mostrar_primeros::[String]->String->String->String->String               
mostrar_primeros (v:vs) term prod axioma = "primero("++v++")={"++(join ',' (elimina_rep (primero term prod axioma v)))++"}\n"++(mostrar_primeros vs term prod axioma)
mostrar_primeros [] _ _ _ = ""
--mostrar_siguientes::[String]->String->String->String->String    
--mostrar_siguientes (v:vs) axioma term prod = "siguiente("++v++")={"++(join ',' (siguiente axioma term prod v))++"}\n"++(mostrar_siguientes vs axioma term prod)
--mostrar_siguientes [] _ _ _ = ""
mostrar_siguientes2::[(String,[String])]->String    
mostrar_siguientes2 ((v,vs):xs) = "siguiente("++v++")={"++(join ',' vs)++"}\n"++(mostrar_siguientes2 xs)
mostrar_siguientes2 [] = ""
inicializar_siguientes::[String]->[(String,[String])]
inicializar_siguientes (v:vs) = (v,[]):(inicializar_siguientes vs)
inicializar_siguientes [] = []
obtener_siguientes::String->String->String->[(String,[String])]->[(String,[String])]->Integer->[(String,[String])]     
obtener_siguientes axioma term prod ((x,y):xs) trat fin = (obtener_siguientes axioma term prod xs (trat++[(x,(if y/=[] then (elimina_rep (y++(siguiente axioma term prod x (trat++xs)))) else (siguiente axioma term prod x (trat++xs))))]) (if (length y)<(length (if y/=[] then (elimina_rep (y++(siguiente axioma term prod x (trat++xs)))) else (siguiente axioma term prod x (trat++xs)))) then (fin+1) else fin))
obtener_siguientes axioma term prod [] trat fin|(fin > 0) = (obtener_siguientes axioma term prod trat [] 0)
                                               |otherwise = trat
dev_sig::[(String,[String])]->String->[String]
dev_sig ((v,vs):xs) a|a==v=vs
                     |otherwise=dev_sig xs a
dev_sig [] _=[]                     
data Elemento =  Elemento {numero::Int, conjunto::[Regla]}
showElemento::[Elemento]->[String]
showElemento []=[]
showElemento ((Elemento {numero=n,conjunto=c}):xs) = (show n:(map showRegla c))++(showElemento xs)
ir_a::String->String->String->String->Elemento->String->Reglas
ir_a t v p axioma (Elemento {numero=n,conjunto=c}) x= cerradura t v p axioma [(PIzda r, Pdcha (avanzarpunto m))|(PIzda r, Pdcha m)<-(filter (contienePdcha x) c),m!!(if ((pos_punto x "." 1 m)==1) then 0 else ((pos_punto x "." 1 m)-2))=="."]
avanzarpunto::[String]->[String]
avanzarpunto x = (takeInt ((pos "." 1 1 x)-1) x)++[x!!(pos "." 1 1 x)]++["."]++(dropInt ((pos "." 1 1 x)+1) x)
cerradura::String->String->String->String->Reglas->Reglas       
cerradura _ _ _ _ [] = []
--cerradura t v p axioma ((PIzda x,Pdcha y):xs) | (pos "." 1 1 y)<(length y) && (member (y!!(pos "." 1 1 y)) (variables v)) = insertar_regla (PIzda x,Pdcha y) (cerradura t v p axioma ([n|n<-(anadirpunto (reglasde (y!!(pos "." 1 1 y)) p axioma)),noexiste n xs]++xs)) []
--                                 | otherwise = insertar_regla (PIzda x,Pdcha y) (cerradura t v p axioma xs) []
cerradura t v p axioma ((PIzda x,Pdcha y):xs) | (pos "." 1 1 y)<(length y) && (member (y!!(pos "." 1 1 y)) (variables v)) = [(PIzda x,Pdcha y)]++(cerradura t v p axioma ([n|n<-(anadirpunto (reglasde (y!!(pos "." 1 1 y)) p axioma)),noexiste n xs]++xs))
                                 | otherwise = [(PIzda x,Pdcha y)]++(cerradura t v p axioma xs)
                                 
anadirpunto::Reglas->Reglas
anadirpunto [] = []
anadirpunto ((PIzda x,Pdcha y):xs) = [(PIzda x,Pdcha (".":y))]++(anadirpunto xs)
insertar_regla::Regla->Reglas->Reglas->Reglas
insertar_regla (PIzda x,Pdcha y) [] z=z++[(PIzda x,Pdcha y)]
insertar_regla (PIzda x,Pdcha y) ((PIzda x1,Pdcha y1):xs) z|(x>x1)=z++[(PIzda x1,Pdcha y1),(PIzda x,Pdcha y)]++xs
                                                     |(x<x1)=insertar_regla (PIzda x,Pdcha y) xs z++[(PIzda x1,Pdcha y1)]
                                                     |(x==x1)&&(head y)<(head y1)=insertar_regla (PIzda x,Pdcha y) xs z++[(PIzda x1,Pdcha y1)]
                                                     |(x==x1)&&(head y)==(head y1)&&(length y)<(length y1)=insertar_regla (PIzda x,Pdcha y) xs z++[(PIzda x1,Pdcha y1)]
                                                     |otherwise=z++[(PIzda x1,Pdcha y1),(PIzda x,Pdcha y)]++xs
noexiste::Regla->Reglas->Bool
noexiste x y = not (member x y)
instance Eq Partes where
      (PIzda x)==(PIzda y) = x == y
      (Pdcha x)==(Pdcha y) = x == y
esta_en::Regla->Reglas->Bool
esta_en (PIzda x,Pdcha y) [] = False
esta_en (PIzda x,Pdcha y) ((PIzda x1,Pdcha y1):xs)|(x>x1)=False
                               |(x==x1)&&(head y)>(head y1)=False
                               |(x==x1)&&(y==y1)=True
                               |otherwise=esta_en (PIzda x,Pdcha y) xs
igualesReglas::Reglas->Reglas->Bool
igualesReglas [] _ = True
igualesReglas (x:xs) y |(member x y) = igualesReglas xs y
                       |otherwise = False
pertenece::Reglas->[Elemento]->Int->Int
pertenece _ [] n=n
pertenece c ((Elemento {numero=_,conjunto=x}):xs) n |(length c)==(length x)= if (igualesReglas c x) && (igualesReglas x c) then n else pertenece c xs (n+1)
                                                    |otherwise = pertenece c xs (n+1)
colec_slr::String->String->String->String->[Elemento]->[Elemento]->[Elemento]
colec_slr _ _ _ _ [] y = y      
colec_slr t v p axioma (x:xs) y = colec_slr t v p axioma ([Elemento {numero=(pertenece n y 1) , conjunto=n}|n<-(map (ir_a t v p axioma x) ((terminales t)++(variables v))),(length n)>0 && (pertenece n y 1)>(length y)]++xs) ([Elemento {numero=(pertenece n y 1) , conjunto=n}|n<-(map (ir_a t v p axioma x) ((terminales t)++(variables v))),(length n)>0&& (pertenece n y 1)>(length y)]++y)
cambiar_numero::[Elemento]->[Elemento]
cambiar_numero (x:[])=[x]
cambiar_numero ((Elemento {numero=n1,conjunto=c1}):(Elemento {numero=n2,conjunto=c2}):xs)|n2<=n1=(Elemento {numero=n1,conjunto=c1}):(cambiar_numero ((Elemento {numero=(n1+1),conjunto=c2}):xs))
                                                                                         |otherwise=(Elemento {numero=n1,conjunto=c1}):(cambiar_numero ((Elemento {numero=n2,conjunto=c2}):xs))
calc_colec_slr::String->String->String->String->[Elemento]->[Elemento] 
calc_colec_slr t v p axioma x = cambiar_numero (reverse (colec_slr t v p axioma x x))
--
--colec_slr::String->String->String->String->[Elemento]->[Elemento]->[Elemento]
--colec_slr _ _ _ _ [] y = y      
--colec_slr t v p axioma ((Elemento {numero=num,conjunto=ccurso}):xs) y  = colec_slr t v p axioma ((asignar_numero ([Elemento {numero=0 , conjunto=n}|n<-(map (ir_a t v p axioma (Elemento {numero=num,conjunto=ccurso})) ((terminales t)++(variables v))),(length n)>0 && (pertenece n y 1)>(length y)]) (length y))++xs) ((asignar_numero([Elemento {numero=0 , conjunto=n}|n<-(map (ir_a t v p axioma (Elemento {numero=num,conjunto=ccurso})) ((terminales t)++(variables v))),(length n)>0&& (pertenece n y 1)>(length y)]) (length y))++y) 
--asignar_numero::[Elemento]->Int->[Elemento]
--asignar_numero [] _ =[]
--asignar_numero ((Elemento {numero=_,conjunto=c1}):xs) n = (Elemento {numero=n+1,conjunto=c1}):(asignar_numero xs (n+1))
--calc_colec_slr::String->String->String->String->[Elemento]->[Elemento] 
--calc_colec_slr t v p axioma x = reverse (colec_slr t v p axioma x x)
--
data Elem_acc =  Elem_acc {estado::Int, term::String, accion::String, regla::Regla, sig_estado::Int}
--tabla_acc::String->String->String->String->[Elemento]->[Elemento]->[Elem_acc]
--tabla_acc _ _ _ _ [] _=[]
--tabla_acc axioma t v p ((Elemento {numero=n,conjunto=c}):xs) tac = (concat [[if(x=="P"++axioma && z=="$") then (Elem_acc {estado=n,term=z,accion="aceptar",regla=(PIzda x,Pdcha y),sig_estado=0}) else (Elem_acc {estado=n,term=z,accion="reducir",regla=(PIzda x,Pdcha y),sig_estado=0})|z<-(delete "lambda" (siguiente axioma t p x))]|(PIzda x,Pdcha y)<-reducir c])++
--                                                                      [(Elem_acc {estado = n,term = y!!(pos "." 1 1 y),accion="desplazar",regla=(PIzda axioma,Pdcha []),sig_estado =(pertenece (ir_a t v p axioma (Elemento {numero=n,conjunto=c}) (y!!(pos "." 1 1 y))) tac 1)})|(PIzda x,Pdcha y)<-desplazar (terminales t) c]++
--                                      (tabla_acc axioma t v p xs tac)
tabla_acc::String->String->String->String->[(String,[String])]->[Elemento]->[Elemento]->[Elem_acc]
tabla_acc _ _ _ _ _ [] _=[]
tabla_acc axioma t v p lsig ((Elemento {numero=n,conjunto=c}):xs) tac = (concat [[if(x=="P"++axioma && z=="$") then (Elem_acc {estado=n,term=z,accion="aceptar",regla=(PIzda x,Pdcha y),sig_estado=0}) else (Elem_acc {estado=n,term=z,accion="reducir",regla=(PIzda x,Pdcha y),sig_estado=0})|z<-(delete "lambda" (dev_sig lsig x))]|(PIzda x,Pdcha y)<-reducir c])++
                                                                      [(Elem_acc {estado = n,term = y!!(pos "." 1 1 y),accion="desplazar",regla=(PIzda axioma,Pdcha []),sig_estado =(pertenece (ir_a t v p axioma (Elemento {numero=n,conjunto=c}) (y!!(pos "." 1 1 y))) tac 1)})|(PIzda x,Pdcha y)<-desplazar (terminales t) c]++
                                      (tabla_acc axioma t v p lsig xs tac)
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
                                                             |(acc=="aceptar")=show n++": en " ++z++" aceptar "++(showElem_acc xs)
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
tabla_ira::String->String->String->String->[Elemento]->[Elemento]->[Elem_ira]
tabla_ira _ _ _ _ [] telem =[]
tabla_ira axioma t v p ((Elemento {numero=n,conjunto=c}):xs) telem = [(Elem_ira {estado2 = n,var = y!!(pos "." 1 1 y),sig_estado2 =(pertenece (ir_a t v p axioma (Elemento {numero=n,conjunto=c}) (y!!(pos "." 1 1 y))) telem 1)})|(PIzda x,Pdcha y)<-desplazar (variables v) c]++
                                             (tabla_ira axioma t v p xs telem)
generar_tabla_ira::[Elem_ira]->String
generar_tabla_ira []="[]"
generar_tabla_ira (Elem_ira {estado2=n, var=z, sig_estado2=m}:xs)="(Elem_ira {estado2="++show n++",var=\"" ++z++"\",sig_estado2="++show m++"}):\n       "++(generar_tabla_ira xs)
generar_parser::String->String->String->String->[Elemento]->String->String
generar_parser axioma t v p telem nom =  "module "++nom++"(parser_slr,parser_slr_arbol) where\nimport Utilidades\nimport Arbol\nimport Scaner"++(substr nom (posb "_" 1 1 nom) ((length nom)-(posb "_" 1 1 nom)+1))++"\n"++
                "--------------- Gramatica: -------------------------------------------------\n"++
                "-- "++axioma++"\n"++
                "-- "++(join ' ' (terminales t))++"\n"++
                "-- "++(join ' ' (variables v))++"\n"++
                concat[(++) "--" r|r<-(map showRegla (producciones p axioma))]++
                "----------------------------------------------------------------------------\n"++
                "data Partes = PIzda String|Pdcha [String]\ntype Regla=(Partes,Partes)\ndata Elem_acc =  Elem_acc {estado::Int, term::String, accion::String, regla::Regla, sig_estado::Int}\ntabla_acc::[Elem_acc]\n"++
                "tabla_acc="++(generar_tabla (tabla_acc axioma t v p (obtener_siguientes axioma t p ((("P"++axioma),["$"]):(inicializar_siguientes (variables v)))  [] 0) telem telem))++
                "\ndata Elem_ira =  Elem_ira {estado2::Int, var::String, sig_estado2::Int}\ntabla_ira::[Elem_ira]\ntabla_ira="++(generar_tabla_ira (tabla_ira axioma t v p telem telem))++
                "\naxioma::String\naxioma=\"P"++axioma++"\"\nterminales::[String]\nterminales = ["++(substr (concat["\""++x++"\","|x<-split ' ' t]) 1 (length (concat["\""++x++"\","|x<-split ' ' t])-1))++"]\n"++
            --   ++"buscaSigToken::String->[String]->Int->String\nbuscaSigToken [] _ _=\"$\"\nbuscaSigToken x y c|(member (substr x 1 c) y)=(substr x 1 c)\n        |otherwise = buscaSigToken x y (c+1)\nsigToken::String->[[Char]]\n"++
            --  "sigToken [] = [\"$\"]\nsigToken x = (buscaSigToken (trim x) (terminales++[\"$\"]) 1):(sigToken (substr (trim x) ((length (buscaSigToken (trim x) (terminales ++[\"$\"]) 1))+1) (length x)))\n"
                "busca_ira::[Elem_ira]->Int->String->Int\n"++
                "busca_ira (Elem_ira {estado2=n,var=z,sig_estado2=m}:xs) e v = if(n==e && v==z) then m else busca_ira xs e v\n"++
                "busca_ira [] _ _ = -1\n"++
                "busca_accion::[Elem_acc]->Int->String->String\n"++
                "busca_accion (Elem_acc {estado=n,term=z,accion=acc,regla=r,sig_estado=m}:xs) e t = if(n==e && t==z) then acc else busca_accion xs e t\n"++
                "busca_accion [] _ _ = \"error\"\n"++
                "desplazar_a::[Elem_acc]->Int->String->Int\n"++
                "desplazar_a (Elem_acc {estado=n,term=z,accion=acc,regla=r,sig_estado=m}:xs) e t = if(n==e && t==z) then m else desplazar_a xs e t\n"++
                "reduce_por::[Elem_acc]->Int->String->[String]\n"++
                "reduce_por (Elem_acc {estado=n,term=z,accion=acc,regla=(PIzda a,Pdcha beta),sig_estado=m}:xs) e t = if(n==e && t==z) then beta else reduce_por xs e t\n"++
                "reduce_a::[Elem_acc]->Int->String->String\n"++
                "reduce_a (Elem_acc {estado=n,term=z,accion=acc,regla=(PIzda a,Pdcha beta),sig_estado=m}:xs) e t = if(n==e && t==z) then a else reduce_a xs e t\n"++
                "visualizar_pila::[Int]->String\n"++
                "visualizar_pila (y:ys) = (visualizar_pila ys)++\" \"++(show y)\n"++
                "visualizar_pila []=\"\"\n"++
                "parser_slr::String->[Int]->String\n"++
                "parser_slr x (y:ys)|(busca_accion tabla_acc y (fst (sigToken x)))==\"desplazar\" = parser_slr (substr x ((posb (snd (buscaSigToken x \"\")) 1 1 x)+(length (snd (buscaSigToken x \"\")))) (length x)) ((desplazar_a tabla_acc y (fst (sigToken x))):y:ys)\n"++
                "                        |(busca_accion tabla_acc y (fst (sigToken x)))==\"reducir\"  && \n"++ 
                "                           (busca_ira tabla_ira (head (dropInt (if((reduce_por tabla_acc y (fst (sigToken x)))!!0==\"lambda\") then 0 else (length (reduce_por tabla_acc y (fst (sigToken x))))) (y:ys))) (reduce_a tabla_acc y (fst (sigToken x))))>0  = \n"++ 
                "                              (visualizar_pila (y:ys))++\"      \"++(reduce_a tabla_acc y (fst (sigToken x)))++\"->\"++(concat (reduce_por tabla_acc y (fst (sigToken x))))++\"      \"++x++\"   \"++\n"++ 
                "                             parser_slr x ((busca_ira tabla_ira (head (dropInt (if((reduce_por tabla_acc y (fst (sigToken x)))!!0==\"lambda\") then 0 else (length (reduce_por tabla_acc y (fst (sigToken x))))) (y:ys))) (reduce_a tabla_acc y (fst (sigToken x)))):(if((reduce_por tabla_acc y (fst (sigToken x)))!!0==\"lambda\") then (y:ys) else (dropInt (length (reduce_por tabla_acc y (fst (sigToken x)))) (y:ys)))) \n"++ 
                "                      |(busca_accion tabla_acc y (fst (sigToken x)))==\"reducir\"    = (visualizar_pila (y:ys))++\"      \"++(reduce_a tabla_acc y (fst (sigToken x)))++\"->\"++(concat (reduce_por tabla_acc y (fst (sigToken x))))++\"      \"++x++\n"++
                "                                                                            \"falta entrada en la tabla ira, estado=\"++show (head (dropInt (if((reduce_por tabla_acc y (fst (sigToken x)))!!0==\"lambda\") then 0 else (length (reduce_por tabla_acc y (fst (sigToken x))))) (y:ys)))++\",variable=\"++(reduce_a tabla_acc y (fst (sigToken x))) \n"++ 
                "                      |(busca_accion tabla_acc y (fst (sigToken x)))==\"aceptar\"   = \"entrada correcta\" \n"++ 
                "                      |(busca_accion tabla_acc y (fst (sigToken x)))==\"error\"       = (visualizar_pila (y:ys))++\"      \"++x++\n"++
                "                                                                            \"falta entrada en la tabla accion, estado=\"++(show y)++\", terminal=\"++(fst (sigToken x))\n"++
                "parser_slr_arbol::String->[Int]->[Arbolsintactico]->Arbolsintactico\n"++
                "parser_slr_arbol x (y:ys) pila_sem |(busca_accion tabla_acc y (fst (sigToken x)))==\"desplazar\" = parser_slr_arbol (substr x ((posb (snd (buscaSigToken x \"\")) 1 1 x)+(length (snd (buscaSigToken x \"\")))) (length x)) ((desplazar_a tabla_acc y (fst (sigToken x))):y:ys) ((devolver_hoja (sigToken x)):pila_sem)\n"++
                "                        |(busca_accion tabla_acc y (fst (sigToken x)))==\"reducir\"  && \n"++ 
                "                           (busca_ira tabla_ira (head (dropInt (if((reduce_por tabla_acc y (fst (sigToken x)))!!0==\"lambda\") then 0 else (length (reduce_por tabla_acc y (fst (sigToken x))))) (y:ys))) (reduce_a tabla_acc y (fst (sigToken x))))>0  = \n"++                 
                "                                parser_slr_arbol x ((busca_ira tabla_ira (head (dropInt (if((reduce_por tabla_acc y (fst (sigToken x)))!!0==\"lambda\") then 0 else (length (reduce_por tabla_acc y (fst (sigToken x))))) (y:ys))) (reduce_a tabla_acc y (fst (sigToken x)))):(if((reduce_por tabla_acc y (fst (sigToken x)))!!0==\"lambda\") then (y:ys) else (dropInt (length (reduce_por tabla_acc y (fst (sigToken x)))) (y:ys)))) \n"++ 
                "                                                        (if((reduce_por tabla_acc y (fst (sigToken x)))!!0==\"lambda\") then (devolver_rama_vacia (reduce_a tabla_acc y (fst (sigToken x)))):pila_sem else (devolver_rama (reduce_a tabla_acc y (fst (sigToken x))) (takeInt (length (reduce_por tabla_acc y (fst (sigToken x)))) pila_sem)):(dropInt (length (reduce_por tabla_acc y (fst (sigToken x)))) pila_sem))\n"++
                "                        |(busca_accion tabla_acc y (fst (sigToken x)))==\"aceptar\"   = (head pila_sem)\n"
devuelve_nombre x = (substr x 1 ((posb "." 1 1 x)-1))
capitalize x = if(ord(head x)>=ord('a') && ord(head x)<=ord('z')) then [chr(ord (head x)-32)]++(substr x 2 (length x)) else x                 
main:: IO ()
main = do
        [f1,f2] <- getArgs 
        h1 <- openFile f1 ReadMode
        axioma  <- hGetLine h1
        term  <- hGetLine h1
        var  <- hGetLine h1
        prod <- hGetContents h1
        --putStrLn ("terminales="++(join ' ' (terminales term)))
        --putStrLn ("variables="++(join ' ' (variables var)))
        --putStrLn ("reglas="++concat(map showRegla (producciones prod axioma)))
        --writeFile f2 ("tabla accion="++generar_tabla (tabla_acc axioma term var prod (obtener_siguientes (variables var) axioma term prod) (calc_colec_slr term var prod axioma [Elemento {numero = 1,conjunto = (cerradura term var prod axioma ((PIzda ("P"++axioma),Pdcha [".",axioma]):[]))}])))
        --putStrLn (mostrar_primeros (variables var) term prod axioma)
        --putStrLn (mostrar_siguientes2 (obtener_siguientes axioma term prod ((("P"++axioma),["$"]):(inicializar_siguientes (variables var)))  [] 0))
        --writeFile f2 (concat (showElemento (calc_colec_slr term var prod axioma [Elemento {numero = 1,conjunto = (cerradura term var prod axioma ((PIzda ("P"++axioma),Pdcha [".",axioma]):[]))}])))
        writeFile f2 (generar_parser axioma term var prod (calc_colec_slr term var prod axioma [Elemento {numero = 1,conjunto = cerradura term var prod axioma ((PIzda ("P"++axioma),Pdcha [".",axioma]):[])}]) (capitalize (devuelve_nombre f2)))
        hClose h1