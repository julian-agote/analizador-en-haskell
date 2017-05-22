import Parser_logica
import Data.Char(ord,chr)
import Utilidades
import Arbol
buscaSigToken::String->String->(String,String)
buscaSigToken [] "" =("$","")
buscaSigToken [] y =("prop",y)
buscaSigToken x y |((head x)=='\n'||(head x)==' ') && (trim y)==""=(buscaSigToken  (tail x) "")
                    |(head x)==',' && (trim y)==""=("coma",(substr x 1 1))
                    |(head x)=='~' && (trim y)==""=("neg",(substr x 1 1))
                    |(head x)=='^' && (trim y)==""=("conj_dis",(substr x 1 1))
                    |(head x)=='v' && (trim y)==""=("conj_dis",(substr x 1 1))
                    |(head x)=='(' && (trim y)==""=("(",(substr x 1 1))
                    |(head x)==')' && (trim y)==""=(")",(substr x 1 1))
                    |(substr x 1 2)=="->" && (trim y)==""=("impl","->")
                    |(substr x 1 3)=="<->" && (trim y)==""=("coimpl","<->")
                    |(substr x 1 2)=="=>" && (trim y)==""=("=>","=>")
                    |ord(head x)>=ord('a') && ord(head x)<=ord('z')=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
                    |otherwise = ("prop",y)
sigToken::String->[(String,String)]
sigToken [] = [("$","")]
sigToken x = (buscaSigToken x ""):(sigToken (substr x ((posb (snd (buscaSigToken x "")) 1 1 x)+(length (snd (buscaSigToken x "")))) (length x)))

data Arbolprop = Hojap String| Ramap  String (Arbolprop) (Arbolprop)| Ramaun String (Arbolprop)
visualizar_prop::[Arbolprop]->String
visualizar_prop []=""
visualizar_prop (x:xs)=(visualizar_arbol x)++","++(visualizar_prop xs)
visualizar_arbol::Arbolprop->String
visualizar_arbol (Hojap x) = x
visualizar_arbol (Ramap  x li ld)=(visualizar_arbol li)++x++(visualizar_arbol ld)
visualizar_arbol (Ramaun  x li)|x=="()"="("++(visualizar_arbol li)++")"
                               |otherwise = x++(visualizar_arbol li)
obtener_prop::Arbolsintactico->[Arbolprop]
obtener_prop x |((obtener_regla_aplicable x)=="G -> prop")= [(Hojap (devolver_id(head (devolver_ramas x))))]
               |((obtener_regla_aplicable x)=="G -> neg G")=[(Ramaun "~" (head(obtener_prop(head (tail (devolver_ramas x))))))] 
               |((obtener_regla_aplicable x)=="FIMPL -> G FIMPLP")= if(obtener_regla_aplicable (head (tail (devolver_ramas x))))=="FIMPLP -> impl G FIMPLP" then
                                                                               [(Ramap "->" (head(obtener_prop(head(devolver_ramas x)))) (head(obtener_prop((devolver_ramas x)!!1))))]
                                                                   else if(obtener_regla_aplicable (head (tail (devolver_ramas x))))=="FIMPLP -> coimpl G FIMPLP" then
                                                                               [(Ramap "<->" (head(obtener_prop(head(devolver_ramas x)))) (head(obtener_prop((devolver_ramas x)!!1))))]            
                                                                        else obtener_prop (head (devolver_ramas x))      
               |((obtener_regla_aplicable x)=="FIMPLP -> impl G FIMPLP")= if(obtener_regla_aplicable (head (tail (devolver_ramas x))))=="FIMPLP -> impl G FIMPLP" then
                                                                               [(Ramap "->" (head(obtener_prop(head(devolver_ramas x)))) (head(obtener_prop((devolver_ramas x)!!1))))]
                                                                   else if(obtener_regla_aplicable (head (tail (devolver_ramas x))))=="FIMPLP -> coimpl G FIMPLP" then
                                                                               [(Ramap "<->" (head(obtener_prop(head(devolver_ramas x)))) (head(obtener_prop((devolver_ramas x)!!1))))]            
                                                                        else obtener_prop (head (tail(devolver_ramas x)))      
               |((obtener_regla_aplicable x)=="FIMPLP -> coimpl G FIMPLP")= if(obtener_regla_aplicable (head (tail (devolver_ramas x))))=="FIMPLP -> impl G FIMPLP" then
                                                                               [(Ramap "->" (head(obtener_prop(head(devolver_ramas x)))) (head(obtener_prop((devolver_ramas x)!!1))))]
                                                                   else if(obtener_regla_aplicable (head (tail (devolver_ramas x))))=="FIMPLP -> coimpl G FIMPLP" then
                                                                               [(Ramap "<->" (head(obtener_prop(head(devolver_ramas x)))) (head(obtener_prop((devolver_ramas x)!!1))))]            
                                                                        else obtener_prop (head (tail(devolver_ramas x)))      
               |((obtener_regla_aplicable x)=="FCONJ -> FIMPL FCONJP")= if(obtener_regla_aplicable (head (tail (devolver_ramas x))))=="FCONJP -> conj_dis FIMPL FCONJP" then
                                                                               [(Ramap (devolver_id(head(devolver_ramas (head (tail (devolver_ramas x)))))) (head(obtener_prop(head(devolver_ramas x)))) (head(obtener_prop((devolver_ramas x)!!1))))]            
                                                                        else obtener_prop (head (devolver_ramas x))                                                              
               |((obtener_regla_aplicable x)=="FCONJP -> conj_dis FIMPL FCONJP")= if(obtener_regla_aplicable (head (tail (devolver_ramas x))))=="FCONJP -> conj_dis FIMPL FCONJP" then
                                                                               [(Ramap (devolver_id(head(devolver_ramas (head (tail (devolver_ramas x)))))) (head(obtener_prop(head(devolver_ramas x)))) (head(obtener_prop((devolver_ramas x)!!1))))]            
                                                                        else obtener_prop (head (tail(devolver_ramas x)))                                                              
               |((obtener_regla_aplicable x)=="P -> FCONJ LF")=(obtener_prop (head(devolver_ramas x)))++(obtener_prop (head(tail(devolver_ramas x))))
               |((obtener_regla_aplicable x)=="LF -> coma FCONJ LF")=(obtener_prop (head(tail(devolver_ramas x))))++(obtener_prop (head(tail(tail(devolver_ramas x)))))
               |((obtener_regla_aplicable x)=="A -> P => FCONJ")=(obtener_prop (head(devolver_ramas x)))++(obtener_prop (head(tail(tail(devolver_ramas x)))))
               |otherwise=[]

-- compara si son iguales dos formulas
igual::Arbolprop->Arbolprop->Bool
igual (Hojap x1) (Ramaun "()" (Hojap x2)) = x1==x2
igual (Ramaun "()" (Hojap x1)) (Hojap x2) = x1==x2
igual (Ramap et1 x1 y1) (Ramaun "()" (Ramap et2 x2 y2)) = et1==et2 && (igual x1 x2) && (igual y1 y2)
igual (Ramaun "()" (Ramap et1 x1 y1)) (Ramap et2 x2 y2) = et1==et2 && (igual x1 x2) && (igual y1 y2)
igual (Ramap et1 x1 y1) (Ramap et2 x2 y2) = et1==et2 && (igual x1 x2) && (igual y1 y2)
igual (Ramaun et1 x1) (Ramaun et2 x2) = et1==et2 && (igual x1 x2)
igual (Hojap x1) (Hojap x2)=x1==x2
igual _ _ = False
miembro::(Arbolprop,(String,[Arbolprop]))->[(Arbolprop,(String,[Arbolprop]))]->Bool
miembro (p1,(s1,sust1)) ((p2,(s2,sust2)):xs)|(igual p1 p2) && s1==s2 = True
                                                            |otherwise = miembro (p1,(s1,sust1)) xs
miembro (p1,(s1,sust1)) [] = False  
find::Arbolprop->[Arbolprop]->Bool
find x (y:ys)|(igual (quitar_parentesis x) (quitar_parentesis y)) = True
             |otherwise = find x ys
find _ [] = False
posicion::Arbolprop->[(Arbolprop,(String,[Arbolprop]))]->Int->Int
posicion p (x:xs) c|(igual p (fst x))=c
                   |otherwise = posicion p xs (c + 1)
posicion p [] _ = 0
quitar_parentesis::Arbolprop->Arbolprop
quitar_parentesis (Ramaun "()" p) = p
quitar_parentesis p = p
resto_formulas::Arbolprop->[(Arbolprop,(String,[Arbolprop]))]->[Arbolprop]
resto_formulas x (y:ys)|not (igual x (fst y)) = (fst y):(resto_formulas x ys)
                       |otherwise = resto_formulas x ys
resto_formulas _ [] = []      
-- de la demostracion sacar las formulas
sacar_formulas::[(Arbolprop,(String,[Arbolprop]))]->[Arbolprop]
sacar_formulas (x:xs) = (fst x):(sacar_formulas xs)
sacar_formulas []=[]    
-- ver si cada linea de una demostracion esta tambien en la otra
igual_demostracion::[(Arbolprop,(String,[Arbolprop]))]->[(Arbolprop,(String,[Arbolprop]))]->Bool
igual_demostracion (x:xs) y|(miembro x y) = igual_demostracion xs y
                           |otherwise = False
igual_demostracion [] _ = True                                       

-- ver si hay alguna entre las formulas del segundo parametro que es igual a la formula del primer parametro (que es el antecedente de una implicacion) y en caso de no encontrarse ninguno
-- ver si es igual a alguno de los axiomas o teoremas del sistema
obtener_antecedente::Arbolprop->[(Arbolprop,(String,[Arbolprop]))]->(String,[Arbolprop])
obtener_antecedente p (x:xs)|(igual (quitar_parentesis p) (quitar_parentesis (fst x))) = ("M",[p])
                                        |otherwise = obtener_antecedente p xs
obtener_antecedente (Ramap "->" p1 (Ramap "->" p2 p3)) []|igual p1 p3 = ("A1",[p1,p2])
obtener_antecedente (Ramap "->" p1 (Ramaun "()" (Ramap "->" p2 p3))) []|igual p1 p3 = ("A1",[p1,p2])
obtener_antecedente (Ramap "->" (Ramaun "()" (Ramap "->" p1 (Ramaun "()" (Ramap "->" p2 p3)))) (Ramaun "()" (Ramap "->" (Ramaun "()" (Ramap "->" p4 p5)) (Ramaun "()" (Ramap "->" p6 p7))))) []|(igual p1 p4)&&(igual p1 p6)&&(igual p2 p5)&&(igual p3 p7) = ("A2",[p1,p2,p3])
obtener_antecedente (Ramap "->" (Ramap "->" p1 (Ramaun "()" (Ramap "->" p2 p3))) (Ramaun "()" (Ramap "->" (Ramaun "()" (Ramap "->" p4 p5)) (Ramaun "()" (Ramap "->" p6 p7))))) []|(igual p1 p4)&&(igual p1 p6)&&(igual p2 p5)&&(igual p3 p7) = ("A2",[p1,p2,p3])
obtener_antecedente (Ramap "->" (Ramaun "()"(Ramap "->" (Ramaun "~" p1) (Ramaun "~" p2))) (Ramaun "()" (Ramap "->" p3 p4))) []|(igual p1 p4)&&(igual p2 p3)=("A3",[p1,p2])
obtener_antecedente (Ramap "->" (Ramaun "()"(Ramap "->" p1 p2)) (Ramaun "()" (Ramap "->" (Ramaun "()" (Ramap "->" p3 p4)) (Ramaun "()" (Ramap "->" p5 p6))))) []|(igual p2 p3)&&(igual p1 p5)&&(igual p4 p6)=("T2",[p1,p2,p4])
obtener_antecedente _ [] = ("",[])

-- buscar obtener la conclusion del tercer parametro c, aplicando el modus ponens entre las formulas existentes, e introduciendo los axiomas y teoremas del sistema L
buscar_implicaciones::[Arbolprop]->[(Arbolprop,(String,[Arbolprop]))]->[(Arbolprop,(String,[Arbolprop]))]->Arbolprop->[(Arbolprop,(String,[Arbolprop]))]
buscar_implicaciones ((Ramap "->" x y):xs) lc rlc c|(fst(obtener_antecedente x lc)/="") && not(miembro (y,("MP("++show (posicion (Ramap "->" x y) (reverse lc) 1)++")("++show (posicion x (reverse lc) 1)++")",snd(obtener_antecedente x lc))) lc)= buscar_implicaciones (y:(Ramap "->" x y):xs) ([(y,("MP("++show (posicion (Ramap "->" x y) (reverse lc) 1)++")("++show (posicion x (reverse lc) 1)++")",snd(obtener_antecedente x lc)))]++lc) rlc c
buscar_implicaciones ((Ramaun "()" (Ramap "->" x y)):xs) lc rlc c|(fst(obtener_antecedente x lc)/="") && not(miembro (y,("MP("++show (posicion (Ramaun "()" (Ramap "->" x y)) (reverse lc) 1)++")("++show (posicion x (reverse lc) 1)++")",snd(obtener_antecedente x lc))) lc)= buscar_implicaciones (y:(Ramap "->" x y):xs) ([(y,("MP("++show (posicion (Ramaun "()" (Ramap "->" x y)) (reverse lc) 1)++")("++show (posicion x (reverse lc) 1)++")",snd(obtener_antecedente x lc)))]++lc) rlc c
buscar_implicaciones (x:xs) lc rlc c|(igual (quitar_parentesis x) (quitar_parentesis c))= reverse lc -- ha encontrado la conclusion
buscar_implicaciones (x:xs) lc rlc c= buscar_implicaciones xs lc rlc c
-- volver a repetir hasta que no se den modificaciones (segundo y tercer parametros iguales)
buscar_implicaciones [] lc rlc c|((length lc)==(length rlc)) && (igual_demostracion lc rlc) = reverse lc
                                |otherwise = buscar_implicaciones (sacar_formulas lc) lc lc c
-- identificar las premisas de la demostracion
poner_como_premisa::Arbolprop->(Arbolprop,(String,[Arbolprop]))
poner_como_premisa x=(x,("P",[]))
ver_demostracion::[(Arbolprop,(String,[Arbolprop]))]->Int->String
ver_demostracion ((p,(op,sust)):xs) c= "("++show c++")"++(visualizar_arbol p)++" "++op++"/"++join ',' (map visualizar_arbol sust)++"\n"++(ver_demostracion xs (c+1))
ver_demostracion [] _= ""

main = do
      --putStrLn (visualizar_prop(obtener_prop(parser_slr_arbol (sigToken "p->q, p => q") [1] [])))
      putStrLn (ver_demostracion (buscar_implicaciones (init (obtener_prop(parser_slr_arbol (sigToken "p->q, p => q") [1] []))) (map poner_como_premisa (init (obtener_prop(parser_slr_arbol (sigToken "p->q, p => q") [1] [])))) (map poner_como_premisa (init (obtener_prop(parser_slr_arbol (sigToken "p->q, p => q") [1] [])))) (last (obtener_prop(parser_slr_arbol (sigToken "p->q, p => q") [1] [])))) 1 ) 