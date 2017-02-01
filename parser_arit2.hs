module Parser_arit2(parser_slr,parser_slr_arbol) where
import Utilidades
import Arbol
data Partes = PIzda String|Pdcha [String]
type Regla=(Partes,Partes)
data Elem_acc =  Elem_acc {estado::Int, term::String, accion::String, regla::Regla, sig_estado::Int}
tabla_acc::[Elem_acc]
tabla_acc=(Elem_acc {estado=1,term="(",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=5}):
       (Elem_acc {estado=1,term="n",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=6}):
       (Elem_acc {estado=2,term="+",accion="reducir",regla=(PIzda "TP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=2,term=")",accion="reducir",regla=(PIzda "TP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=2,term="$",accion="reducir",regla=(PIzda "TP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=2,term="*",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=14}):
       (Elem_acc {estado=3,term=")",accion="reducir",regla=(PIzda "EP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=3,term="$",accion="reducir",regla=(PIzda "EP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=3,term="+",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=10}):
       (Elem_acc {estado=4,term="$",accion="aceptar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=5,term="(",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=5}):
       (Elem_acc {estado=5,term="n",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=6}):
       (Elem_acc {estado=6,term="*",accion="reducir",regla=(PIzda "F",Pdcha ["n"]), sig_estado=0}):
       (Elem_acc {estado=6,term="+",accion="reducir",regla=(PIzda "F",Pdcha ["n"]), sig_estado=0}):
       (Elem_acc {estado=6,term=")",accion="reducir",regla=(PIzda "F",Pdcha ["n"]), sig_estado=0}):
       (Elem_acc {estado=6,term="$",accion="reducir",regla=(PIzda "F",Pdcha ["n"]), sig_estado=0}):
       (Elem_acc {estado=7,term=")",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=8,term="*",accion="reducir",regla=(PIzda "F",Pdcha ["(","E",")"]), sig_estado=0}):
       (Elem_acc {estado=8,term="+",accion="reducir",regla=(PIzda "F",Pdcha ["(","E",")"]), sig_estado=0}):
       (Elem_acc {estado=8,term=")",accion="reducir",regla=(PIzda "F",Pdcha ["(","E",")"]), sig_estado=0}):
       (Elem_acc {estado=8,term="$",accion="reducir",regla=(PIzda "F",Pdcha ["(","E",")"]), sig_estado=0}):
       (Elem_acc {estado=9,term=")",accion="reducir",regla=(PIzda "E",Pdcha ["T","EP"]), sig_estado=0}):
       (Elem_acc {estado=9,term="$",accion="reducir",regla=(PIzda "E",Pdcha ["T","EP"]), sig_estado=0}):
       (Elem_acc {estado=10,term="(",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=5}):
       (Elem_acc {estado=10,term="n",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=6}):
       (Elem_acc {estado=11,term=")",accion="reducir",regla=(PIzda "EP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=11,term="$",accion="reducir",regla=(PIzda "EP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=11,term="+",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=10}):
       (Elem_acc {estado=12,term=")",accion="reducir",regla=(PIzda "EP",Pdcha ["+","T","EP"]), sig_estado=0}):
       (Elem_acc {estado=12,term="$",accion="reducir",regla=(PIzda "EP",Pdcha ["+","T","EP"]), sig_estado=0}):
       (Elem_acc {estado=13,term="+",accion="reducir",regla=(PIzda "T",Pdcha ["F","TP"]), sig_estado=0}):
       (Elem_acc {estado=13,term=")",accion="reducir",regla=(PIzda "T",Pdcha ["F","TP"]), sig_estado=0}):
       (Elem_acc {estado=13,term="$",accion="reducir",regla=(PIzda "T",Pdcha ["F","TP"]), sig_estado=0}):
       (Elem_acc {estado=14,term="(",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=5}):
       (Elem_acc {estado=14,term="n",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=6}):
       (Elem_acc {estado=15,term="+",accion="reducir",regla=(PIzda "TP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=15,term=")",accion="reducir",regla=(PIzda "TP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=15,term="$",accion="reducir",regla=(PIzda "TP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=15,term="*",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=14}):
       (Elem_acc {estado=16,term="+",accion="reducir",regla=(PIzda "TP",Pdcha ["*","F","TP"]), sig_estado=0}):
       (Elem_acc {estado=16,term=")",accion="reducir",regla=(PIzda "TP",Pdcha ["*","F","TP"]), sig_estado=0}):
       (Elem_acc {estado=16,term="$",accion="reducir",regla=(PIzda "TP",Pdcha ["*","F","TP"]), sig_estado=0}):
       []
data Elem_ira =  Elem_ira {estado2::Int, var::String, sig_estado2::Int}
tabla_ira::[Elem_ira]
tabla_ira=(Elem_ira {estado2=1,var="E",sig_estado2=4}):
       (Elem_ira {estado2=1,var="T",sig_estado2=3}):
       (Elem_ira {estado2=1,var="F",sig_estado2=2}):
       (Elem_ira {estado2=2,var="TP",sig_estado2=13}):
       (Elem_ira {estado2=3,var="EP",sig_estado2=9}):
       (Elem_ira {estado2=5,var="E",sig_estado2=7}):
       (Elem_ira {estado2=5,var="T",sig_estado2=3}):
       (Elem_ira {estado2=5,var="F",sig_estado2=2}):
       (Elem_ira {estado2=10,var="T",sig_estado2=11}):
       (Elem_ira {estado2=10,var="F",sig_estado2=2}):
       (Elem_ira {estado2=11,var="EP",sig_estado2=12}):
       (Elem_ira {estado2=14,var="F",sig_estado2=15}):
       (Elem_ira {estado2=15,var="TP",sig_estado2=16}):
       []
axioma::String
axioma="PE"
terminales::[String]
terminales = ["n","+","*","(",")"]
busca_ira::[Elem_ira]->Int->String->Int
busca_ira (Elem_ira {estado2=n,var=z,sig_estado2=m}:xs) e v = if(n==e && v==z) then m else busca_ira xs e v
busca_ira [] _ _ = -1
busca_accion::[Elem_acc]->Int->String->String
busca_accion (Elem_acc {estado=n,term=z,accion=acc,regla=r,sig_estado=m}:xs) e t = if(n==e && t==z) then acc else busca_accion xs e t
busca_accion [] _ _ = "error"
desplazar_a::[Elem_acc]->Int->String->Int
desplazar_a (Elem_acc {estado=n,term=z,accion=acc,regla=r,sig_estado=m}:xs) e t = if(n==e && t==z) then m else desplazar_a xs e t
reduce_por::[Elem_acc]->Int->String->[String]
reduce_por (Elem_acc {estado=n,term=z,accion=acc,regla=(PIzda a,Pdcha beta),sig_estado=m}:xs) e t = if(n==e && t==z) then beta else reduce_por xs e t
reduce_a::[Elem_acc]->Int->String->String
reduce_a (Elem_acc {estado=n,term=z,accion=acc,regla=(PIzda a,Pdcha beta),sig_estado=m}:xs) e t = if(n==e && t==z) then a else reduce_a xs e t
visualizar_pila::[Int]->String
visualizar_pila (y:ys) = (visualizar_pila ys)++" "++(show y)
visualizar_pila []=""
visualizar_resto_entrada::[(String,String)]->String
visualizar_resto_entrada (x:xs)|(fst x)=="$"="$"
                               |otherwise=(fst x)++" "++visualizar_resto_entrada xs
parser_slr::[(String,String)]->[Int]->String
parser_slr (x:xs) (y:ys)|(busca_accion tabla_acc y (fst x))=="desplazar" = parser_slr xs ((desplazar_a tabla_acc y (fst x)):y:ys)
                        |(busca_accion tabla_acc y (fst x))=="reducir"  && 
                           (busca_ira tabla_ira (head (dropInt (if((reduce_por tabla_acc y (fst x))!!0=="lambda") then 0 else (length (reduce_por tabla_acc y (fst x)))) (y:ys))) (reduce_a tabla_acc y (fst x)))>0  = 
                              (visualizar_pila (y:ys))++"      "++(reduce_a tabla_acc y (fst x))++"->"++(concat (reduce_por tabla_acc y (fst x)))++"      "++(visualizar_resto_entrada (x:xs))++"   "++
                             parser_slr (x:xs) ((busca_ira tabla_ira (head (dropInt (if((reduce_por tabla_acc y (fst x))!!0=="lambda") then 0 else (length (reduce_por tabla_acc y (fst x)))) (y:ys))) (reduce_a tabla_acc y (fst x))):(if((reduce_por tabla_acc y (fst x))!!0=="lambda") then (y:ys) else (dropInt (length (reduce_por tabla_acc y (fst x))) (y:ys)))) 
                      |(busca_accion tabla_acc y (fst x))=="reducir"    = (visualizar_pila (y:ys))++"      "++(reduce_a tabla_acc y (fst x))++"->"++(concat (reduce_por tabla_acc y (fst x)))++"      "++(visualizar_resto_entrada (x:xs))++
                                                                            "falta entrada en la tabla ira, estado="++show (head (dropInt (if((reduce_por tabla_acc y (fst x))!!0=="lambda") then 0 else (length (reduce_por tabla_acc y (fst x)))) (y:ys)))++",variable="++(reduce_a tabla_acc y (fst x)) 
                      |(busca_accion tabla_acc y (fst x))=="aceptar"   = "entrada correcta" 
                      |(busca_accion tabla_acc y (fst x))=="error"       = (visualizar_pila (y:ys))++"      "++(visualizar_resto_entrada (x:xs))++
                                                                            "falta entrada en la tabla accion, estado="++(show y)++", terminal="++(fst x)
parser_slr_arbol::[(String,String)]->[Int]->[Arbolsintactico]->Arbolsintactico
parser_slr_arbol (x:xs) (y:ys) pila_sem |(busca_accion tabla_acc y (fst x))=="desplazar" = parser_slr_arbol xs ((desplazar_a tabla_acc y (fst x)):y:ys) ((devolver_hoja x):pila_sem)
                        |(busca_accion tabla_acc y (fst x))=="reducir"  && 
                           (busca_ira tabla_ira (head (dropInt (if((reduce_por tabla_acc y (fst x))!!0=="lambda") then 0 else (length (reduce_por tabla_acc y (fst x)))) (y:ys))) (reduce_a tabla_acc y (fst x)))>0  = 
                                parser_slr_arbol (x:xs) ((busca_ira tabla_ira (head (dropInt (if((reduce_por tabla_acc y (fst x))!!0=="lambda") then 0 else (length (reduce_por tabla_acc y (fst x)))) (y:ys))) (reduce_a tabla_acc y (fst x))):(if((reduce_por tabla_acc y (fst x))!!0=="lambda") then (y:ys) else (dropInt (length (reduce_por tabla_acc y (fst x))) (y:ys)))) 
                                                        (if((reduce_por tabla_acc y (fst x))!!0=="lambda") then (devolver_rama_vacia (reduce_a tabla_acc y (fst x))):pila_sem else (devolver_rama (reduce_a tabla_acc y (fst x)) (takeInt (length (reduce_por tabla_acc y (fst x))) pila_sem)):(dropInt (length (reduce_por tabla_acc y (fst x))) pila_sem))
                        |(busca_accion tabla_acc y (fst x))=="aceptar"   = (head pila_sem)
