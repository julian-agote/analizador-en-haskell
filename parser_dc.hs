module Parser_dc(parser_slr,parser_slr_arbol,visualizar_resto_entrada) where
import Utilidades
import Arbol
--------------- Gramatica: -------------------------------------------------
-- DC
-- id asterisco dos_puntos par_ab par_ce num coma punto_coma 
-- DC OBJETO NOMBRE_OB CLASE MENSAJE ITER PARAMETROS LISTA RLISTA RDC 
--DC -> OBJETO MENSAJE OBJETO RDC
--RDC -> punto_coma OBJETO MENSAJE OBJETO RDC
--RDC -> lambda
--OBJETO -> NOMBRE_OB CLASE
--NOMBRE_OB -> id
--NOMBRE_OB -> lambda
--CLASE -> dos_puntos id
--MENSAJE -> num dos_puntos ITER id PARAMETROS
--ITER -> asterisco
--ITER -> lambda
--PARAMETROS -> par_ab LISTA par_ce
--LISTA -> id RLISTA
--LISTA -> lambda
--RLISTA -> coma id RLISTA
--RLISTA -> lambda
--PDC -> DC
----------------------------------------------------------------------------
data Partes = PIzda String|Pdcha [String]
type Regla=(Partes,Partes)
data Elem_acc =  Elem_acc {estado::Int, term::String, accion::String, regla::Regla, sig_estado::Int}
tabla_acc::[Elem_acc]
tabla_acc=(Elem_acc {estado=1,term="dos_puntos",accion="reducir",regla=(PIzda "NOMBRE_OB",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=1,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=5}):
       (Elem_acc {estado=2,term="dos_puntos",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=29}):
       (Elem_acc {estado=3,term="num",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=7}):
       (Elem_acc {estado=4,term="$",accion="aceptar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=5,term="dos_puntos",accion="reducir",regla=(PIzda "NOMBRE_OB",Pdcha ["id"]), sig_estado=0}):
       (Elem_acc {estado=6,term="dos_puntos",accion="reducir",regla=(PIzda "NOMBRE_OB",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=6,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=5}):
       (Elem_acc {estado=7,term="dos_puntos",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=8,term="id",accion="reducir",regla=(PIzda "ITER",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=8,term="asterisco",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=10}):
       (Elem_acc {estado=9,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=11}):
       (Elem_acc {estado=10,term="id",accion="reducir",regla=(PIzda "ITER",Pdcha ["asterisco"]), sig_estado=0}):
       (Elem_acc {estado=11,term="par_ab",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=13}):
       (Elem_acc {estado=12,term="id",accion="reducir",regla=(PIzda "MENSAJE",Pdcha ["num","dos_puntos","ITER","id","PARAMETROS"]), sig_estado=0}):
       (Elem_acc {estado=12,term="dos_puntos",accion="reducir",regla=(PIzda "MENSAJE",Pdcha ["num","dos_puntos","ITER","id","PARAMETROS"]), sig_estado=0}):
       (Elem_acc {estado=12,term="punto_coma",accion="reducir",regla=(PIzda "MENSAJE",Pdcha ["num","dos_puntos","ITER","id","PARAMETROS"]), sig_estado=0}):
       (Elem_acc {estado=12,term="$",accion="reducir",regla=(PIzda "MENSAJE",Pdcha ["num","dos_puntos","ITER","id","PARAMETROS"]), sig_estado=0}):
       (Elem_acc {estado=13,term="par_ce",accion="reducir",regla=(PIzda "LISTA",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=13,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=15}):
       (Elem_acc {estado=14,term="par_ce",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
       (Elem_acc {estado=15,term="par_ce",accion="reducir",regla=(PIzda "RLISTA",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=15,term="coma",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=17}):
       (Elem_acc {estado=16,term="par_ce",accion="reducir",regla=(PIzda "LISTA",Pdcha ["id","RLISTA"]), sig_estado=0}):
       (Elem_acc {estado=17,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=18}):
       (Elem_acc {estado=18,term="par_ce",accion="reducir",regla=(PIzda "RLISTA",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=18,term="coma",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=17}):
       (Elem_acc {estado=19,term="par_ce",accion="reducir",regla=(PIzda "RLISTA",Pdcha ["coma","id","RLISTA"]), sig_estado=0}):
       (Elem_acc {estado=20,term="id",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["par_ab","LISTA","par_ce"]), sig_estado=0}):
       (Elem_acc {estado=20,term="dos_puntos",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["par_ab","LISTA","par_ce"]), sig_estado=0}):
       (Elem_acc {estado=20,term="punto_coma",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["par_ab","LISTA","par_ce"]), sig_estado=0}):
       (Elem_acc {estado=20,term="$",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["par_ab","LISTA","par_ce"]), sig_estado=0}):
       (Elem_acc {estado=21,term="$",accion="reducir",regla=(PIzda "RDC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=21,term="punto_coma",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=23}):
       (Elem_acc {estado=22,term="$",accion="reducir",regla=(PIzda "DC",Pdcha ["OBJETO","MENSAJE","OBJETO","RDC"]), sig_estado=0}):
       (Elem_acc {estado=23,term="dos_puntos",accion="reducir",regla=(PIzda "NOMBRE_OB",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=23,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=5}):
       (Elem_acc {estado=24,term="num",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=7}):
       (Elem_acc {estado=25,term="dos_puntos",accion="reducir",regla=(PIzda "NOMBRE_OB",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=25,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=5}):
       (Elem_acc {estado=26,term="$",accion="reducir",regla=(PIzda "RDC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=26,term="punto_coma",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=23}):
       (Elem_acc {estado=27,term="$",accion="reducir",regla=(PIzda "RDC",Pdcha ["punto_coma","OBJETO","MENSAJE","OBJETO","RDC"]), sig_estado=0}):
       (Elem_acc {estado=28,term="num",accion="reducir",regla=(PIzda "OBJETO",Pdcha ["NOMBRE_OB","CLASE"]), sig_estado=0}):
       (Elem_acc {estado=28,term="punto_coma",accion="reducir",regla=(PIzda "OBJETO",Pdcha ["NOMBRE_OB","CLASE"]), sig_estado=0}):
       (Elem_acc {estado=28,term="$",accion="reducir",regla=(PIzda "OBJETO",Pdcha ["NOMBRE_OB","CLASE"]), sig_estado=0}):
       (Elem_acc {estado=29,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=30}):
       (Elem_acc {estado=30,term="num",accion="reducir",regla=(PIzda "CLASE",Pdcha ["dos_puntos","id"]), sig_estado=0}):
       (Elem_acc {estado=30,term="punto_coma",accion="reducir",regla=(PIzda "CLASE",Pdcha ["dos_puntos","id"]), sig_estado=0}):
       (Elem_acc {estado=30,term="$",accion="reducir",regla=(PIzda "CLASE",Pdcha ["dos_puntos","id"]), sig_estado=0}):
       []
data Elem_ira =  Elem_ira {estado2::Int, var::String, sig_estado2::Int}
tabla_ira::[Elem_ira]
tabla_ira=(Elem_ira {estado2=1,var="DC",sig_estado2=4}):
       (Elem_ira {estado2=1,var="OBJETO",sig_estado2=3}):
       (Elem_ira {estado2=1,var="NOMBRE_OB",sig_estado2=2}):
       (Elem_ira {estado2=2,var="CLASE",sig_estado2=28}):
       (Elem_ira {estado2=3,var="MENSAJE",sig_estado2=6}):
       (Elem_ira {estado2=6,var="OBJETO",sig_estado2=21}):
       (Elem_ira {estado2=6,var="NOMBRE_OB",sig_estado2=2}):
       (Elem_ira {estado2=8,var="ITER",sig_estado2=9}):
       (Elem_ira {estado2=11,var="PARAMETROS",sig_estado2=12}):
       (Elem_ira {estado2=13,var="LISTA",sig_estado2=14}):
       (Elem_ira {estado2=15,var="RLISTA",sig_estado2=16}):
       (Elem_ira {estado2=18,var="RLISTA",sig_estado2=19}):
       (Elem_ira {estado2=21,var="RDC",sig_estado2=22}):
       (Elem_ira {estado2=23,var="OBJETO",sig_estado2=24}):
       (Elem_ira {estado2=23,var="NOMBRE_OB",sig_estado2=2}):
       (Elem_ira {estado2=24,var="MENSAJE",sig_estado2=25}):
       (Elem_ira {estado2=25,var="OBJETO",sig_estado2=26}):
       (Elem_ira {estado2=25,var="NOMBRE_OB",sig_estado2=2}):
       (Elem_ira {estado2=26,var="RDC",sig_estado2=27}):
       []
axioma::String
axioma="PDC"
terminales::[String]
terminales = ["id","asterisco","dos_puntos","par_ab","par_ce","num","coma","punto_coma"]
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
