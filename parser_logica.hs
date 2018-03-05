module Parser_logica(parser_slr,parser_slr_arbol) where
import Utilidades
import Arbol
--------------- Gramatica: -------------------------------------------------
-- A
-- prop ( ) coma => conj_dis impl coimpl neg 
-- A P LF FCONJ FCONJP FIMPL FIMPLP G 
--A -> P => FCONJ
--P -> FCONJ LF
--LF -> coma FCONJ LF
--LF -> lambda
--FCONJ -> FIMPL FCONJP
--FCONJP -> conj_dis FIMPL FCONJP
--FCONJP -> lambda
--FIMPL -> G FIMPLP
--FIMPLP -> impl G FIMPLP
--FIMPLP -> coimpl G FIMPLP
--FIMPLP -> lambda
--G -> neg G
--G -> ( FCONJ )
--G -> prop
--PA -> A
----------------------------------------------------------------------------
data Partes = PIzda String|Pdcha [String]
type Regla=(Partes,Partes)
data Elem_acc =  Elem_acc {estado::Int, term::String, accion::String, regla::Regla, sig_estado::Int}
tabla_acc::[Elem_acc]
tabla_acc=(Elem_acc {estado=1,term="neg",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=7}):
       (Elem_acc {estado=1,term="(",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=1,term="prop",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=9}):
       (Elem_acc {estado=2,term="conj_dis",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=2,term="$",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=2,term="coma",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=2,term="=>",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=2,term=")",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=2,term="impl",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=25}):
       (Elem_acc {estado=2,term="coimpl",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=24}):
       (Elem_acc {estado=3,term="$",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=3,term="coma",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=3,term="=>",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=3,term=")",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=3,term="conj_dis",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
       (Elem_acc {estado=4,term="=>",accion="reducir",regla=(PIzda "LF",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=4,term="coma",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=16}):
       (Elem_acc {estado=5,term="=>",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=13}):
       (Elem_acc {estado=6,term="$",accion="aceptar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=7,term="neg",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=7}):
       (Elem_acc {estado=7,term="(",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=7,term="prop",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=9}):
       (Elem_acc {estado=8,term="neg",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=7}):
       (Elem_acc {estado=8,term="(",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=8,term="prop",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=9}):
       (Elem_acc {estado=9,term="impl",accion="reducir",regla=(PIzda "G",Pdcha ["prop"]), sig_estado=0}):
       (Elem_acc {estado=9,term="coimpl",accion="reducir",regla=(PIzda "G",Pdcha ["prop"]), sig_estado=0}):
       (Elem_acc {estado=9,term="conj_dis",accion="reducir",regla=(PIzda "G",Pdcha ["prop"]), sig_estado=0}):
       (Elem_acc {estado=9,term="$",accion="reducir",regla=(PIzda "G",Pdcha ["prop"]), sig_estado=0}):
       (Elem_acc {estado=9,term="coma",accion="reducir",regla=(PIzda "G",Pdcha ["prop"]), sig_estado=0}):
       (Elem_acc {estado=9,term="=>",accion="reducir",regla=(PIzda "G",Pdcha ["prop"]), sig_estado=0}):
       (Elem_acc {estado=9,term=")",accion="reducir",regla=(PIzda "G",Pdcha ["prop"]), sig_estado=0}):
       (Elem_acc {estado=10,term=")",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=11}):
       (Elem_acc {estado=11,term="impl",accion="reducir",regla=(PIzda "G",Pdcha ["(","FCONJ",")"]), sig_estado=0}):
       (Elem_acc {estado=11,term="coimpl",accion="reducir",regla=(PIzda "G",Pdcha ["(","FCONJ",")"]), sig_estado=0}):
       (Elem_acc {estado=11,term="conj_dis",accion="reducir",regla=(PIzda "G",Pdcha ["(","FCONJ",")"]), sig_estado=0}):
       (Elem_acc {estado=11,term="$",accion="reducir",regla=(PIzda "G",Pdcha ["(","FCONJ",")"]), sig_estado=0}):
       (Elem_acc {estado=11,term="coma",accion="reducir",regla=(PIzda "G",Pdcha ["(","FCONJ",")"]), sig_estado=0}):
       (Elem_acc {estado=11,term="=>",accion="reducir",regla=(PIzda "G",Pdcha ["(","FCONJ",")"]), sig_estado=0}):
       (Elem_acc {estado=11,term=")",accion="reducir",regla=(PIzda "G",Pdcha ["(","FCONJ",")"]), sig_estado=0}):
       (Elem_acc {estado=12,term="impl",accion="reducir",regla=(PIzda "G",Pdcha ["neg","G"]), sig_estado=0}):
       (Elem_acc {estado=12,term="coimpl",accion="reducir",regla=(PIzda "G",Pdcha ["neg","G"]), sig_estado=0}):
       (Elem_acc {estado=12,term="conj_dis",accion="reducir",regla=(PIzda "G",Pdcha ["neg","G"]), sig_estado=0}):
       (Elem_acc {estado=12,term="$",accion="reducir",regla=(PIzda "G",Pdcha ["neg","G"]), sig_estado=0}):
       (Elem_acc {estado=12,term="coma",accion="reducir",regla=(PIzda "G",Pdcha ["neg","G"]), sig_estado=0}):
       (Elem_acc {estado=12,term="=>",accion="reducir",regla=(PIzda "G",Pdcha ["neg","G"]), sig_estado=0}):
       (Elem_acc {estado=12,term=")",accion="reducir",regla=(PIzda "G",Pdcha ["neg","G"]), sig_estado=0}):
       (Elem_acc {estado=13,term="neg",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=7}):
       (Elem_acc {estado=13,term="(",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=13,term="prop",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=9}):
       (Elem_acc {estado=14,term="$",accion="reducir",regla=(PIzda "A",Pdcha ["P","=>","FCONJ"]), sig_estado=0}):
       (Elem_acc {estado=15,term="=>",accion="reducir",regla=(PIzda "P",Pdcha ["FCONJ","LF"]), sig_estado=0}):
       (Elem_acc {estado=16,term="neg",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=7}):
       (Elem_acc {estado=16,term="(",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=16,term="prop",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=9}):
       (Elem_acc {estado=17,term="=>",accion="reducir",regla=(PIzda "LF",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=17,term="coma",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=16}):
       (Elem_acc {estado=18,term="=>",accion="reducir",regla=(PIzda "LF",Pdcha ["coma","FCONJ","LF"]), sig_estado=0}):
       (Elem_acc {estado=19,term="$",accion="reducir",regla=(PIzda "FCONJ",Pdcha ["FIMPL","FCONJP"]), sig_estado=0}):
       (Elem_acc {estado=19,term="coma",accion="reducir",regla=(PIzda "FCONJ",Pdcha ["FIMPL","FCONJP"]), sig_estado=0}):
       (Elem_acc {estado=19,term="=>",accion="reducir",regla=(PIzda "FCONJ",Pdcha ["FIMPL","FCONJP"]), sig_estado=0}):
       (Elem_acc {estado=19,term=")",accion="reducir",regla=(PIzda "FCONJ",Pdcha ["FIMPL","FCONJP"]), sig_estado=0}):
       (Elem_acc {estado=20,term="neg",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=7}):
       (Elem_acc {estado=20,term="(",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=20,term="prop",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=9}):
       (Elem_acc {estado=21,term="$",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=21,term="coma",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=21,term="=>",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=21,term=")",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=21,term="conj_dis",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
       (Elem_acc {estado=22,term="$",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["conj_dis","FIMPL","FCONJP"]), sig_estado=0}):
       (Elem_acc {estado=22,term="coma",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["conj_dis","FIMPL","FCONJP"]), sig_estado=0}):
       (Elem_acc {estado=22,term="=>",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["conj_dis","FIMPL","FCONJP"]), sig_estado=0}):
       (Elem_acc {estado=22,term=")",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["conj_dis","FIMPL","FCONJP"]), sig_estado=0}):
       (Elem_acc {estado=23,term="conj_dis",accion="reducir",regla=(PIzda "FIMPL",Pdcha ["G","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=23,term="$",accion="reducir",regla=(PIzda "FIMPL",Pdcha ["G","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=23,term="coma",accion="reducir",regla=(PIzda "FIMPL",Pdcha ["G","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=23,term="=>",accion="reducir",regla=(PIzda "FIMPL",Pdcha ["G","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=23,term=")",accion="reducir",regla=(PIzda "FIMPL",Pdcha ["G","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=24,term="neg",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=7}):
       (Elem_acc {estado=24,term="(",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=24,term="prop",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=9}):
       (Elem_acc {estado=25,term="neg",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=7}):
       (Elem_acc {estado=25,term="(",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=25,term="prop",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=9}):
       (Elem_acc {estado=26,term="conj_dis",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=26,term="$",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=26,term="coma",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=26,term="=>",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=26,term=")",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=26,term="impl",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=25}):
       (Elem_acc {estado=26,term="coimpl",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=24}):
       (Elem_acc {estado=27,term="conj_dis",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["impl","G","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=27,term="$",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["impl","G","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=27,term="coma",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["impl","G","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=27,term="=>",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["impl","G","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=27,term=")",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["impl","G","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=28,term="conj_dis",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=28,term="$",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=28,term="coma",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=28,term="=>",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=28,term=")",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=28,term="impl",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=25}):
       (Elem_acc {estado=28,term="coimpl",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=24}):
       (Elem_acc {estado=29,term="conj_dis",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["coimpl","G","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=29,term="$",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["coimpl","G","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=29,term="coma",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["coimpl","G","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=29,term="=>",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["coimpl","G","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=29,term=")",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["coimpl","G","FIMPLP"]), sig_estado=0}):
       []
data Elem_ira =  Elem_ira {estado2::Int, var::String, sig_estado2::Int}
tabla_ira::[Elem_ira]
tabla_ira=(Elem_ira {estado2=1,var="A",sig_estado2=6}):
       (Elem_ira {estado2=1,var="P",sig_estado2=5}):
       (Elem_ira {estado2=1,var="FCONJ",sig_estado2=4}):
       (Elem_ira {estado2=1,var="FIMPL",sig_estado2=3}):
       (Elem_ira {estado2=1,var="G",sig_estado2=2}):
       (Elem_ira {estado2=2,var="FIMPLP",sig_estado2=23}):
       (Elem_ira {estado2=3,var="FCONJP",sig_estado2=19}):
       (Elem_ira {estado2=4,var="LF",sig_estado2=15}):
       (Elem_ira {estado2=7,var="G",sig_estado2=12}):
       (Elem_ira {estado2=8,var="FCONJ",sig_estado2=10}):
       (Elem_ira {estado2=8,var="FIMPL",sig_estado2=3}):
       (Elem_ira {estado2=8,var="G",sig_estado2=2}):
       (Elem_ira {estado2=13,var="FCONJ",sig_estado2=14}):
       (Elem_ira {estado2=13,var="FIMPL",sig_estado2=3}):
       (Elem_ira {estado2=13,var="G",sig_estado2=2}):
       (Elem_ira {estado2=16,var="FCONJ",sig_estado2=17}):
       (Elem_ira {estado2=16,var="FIMPL",sig_estado2=3}):
       (Elem_ira {estado2=16,var="G",sig_estado2=2}):
       (Elem_ira {estado2=17,var="LF",sig_estado2=18}):
       (Elem_ira {estado2=20,var="FIMPL",sig_estado2=21}):
       (Elem_ira {estado2=20,var="G",sig_estado2=2}):
       (Elem_ira {estado2=21,var="FCONJP",sig_estado2=22}):
       (Elem_ira {estado2=24,var="G",sig_estado2=28}):
       (Elem_ira {estado2=25,var="G",sig_estado2=26}):
       (Elem_ira {estado2=26,var="FIMPLP",sig_estado2=27}):
       (Elem_ira {estado2=28,var="FIMPLP",sig_estado2=29}):
       []
axioma::String
axioma="PA"
terminales::[String]
terminales = ["prop","(",")","coma","=>","conj_dis","impl","coimpl","neg"]
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
