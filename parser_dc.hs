module Parser_dc(parser_slr,parser_slr_arbol) where
import Utilidades
import Arbol
data Partes = PIzda String|Pdcha [String]
type Regla=(Partes,Partes)
data Elem_acc =  Elem_acc {estado::Int, term::String, accion::String, regla::Regla, sig_estado::Int}
tabla_acc::[Elem_acc]
tabla_acc=(Elem_acc {estado=1,term=":",accion="reducir",regla=(PIzda "NOMBRE_OB",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=1,term="num",accion="reducir",regla=(PIzda "NOMBRE_OB",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=1,term="$",accion="reducir",regla=(PIzda "DC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=1,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=5}):
       (Elem_acc {estado=2,term="num",accion="reducir",regla=(PIzda "CLASE",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=2,term=":",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=34}):
       (Elem_acc {estado=3,term="num",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=7}):
       (Elem_acc {estado=4,term="$",accion="aceptar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=5,term=":",accion="reducir",regla=(PIzda "NOMBRE_OB",Pdcha ["id"]), sig_estado=0}):
       (Elem_acc {estado=5,term="num",accion="reducir",regla=(PIzda "NOMBRE_OB",Pdcha ["id"]), sig_estado=0}):
       (Elem_acc {estado=6,term=":",accion="reducir",regla=(PIzda "NOMBRE_OB",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=6,term="num",accion="reducir",regla=(PIzda "NOMBRE_OB",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=6,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=5}):
       (Elem_acc {estado=7,term=":",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=8,term="id",accion="reducir",regla=(PIzda "ITER",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=8,term="*",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=10}):
       (Elem_acc {estado=9,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=11}):
       (Elem_acc {estado=10,term="id",accion="reducir",regla=(PIzda "ITER",Pdcha ["*"]), sig_estado=0}):
       (Elem_acc {estado=11,term="(",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=13}):
       (Elem_acc {estado=12,term="crear",accion="reducir",regla=(PIzda "CONDICION",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=12,term="destruir",accion="reducir",regla=(PIzda "CONDICION",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=12,term="activacion",accion="reducir",regla=(PIzda "CONDICION",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=12,term="evento",accion="reducir",regla=(PIzda "CONDICION",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=12,term="id",accion="reducir",regla=(PIzda "CONDICION",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=12,term=":",accion="reducir",regla=(PIzda "CONDICION",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=12,term="nl",accion="reducir",regla=(PIzda "CONDICION",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=12,term="$",accion="reducir",regla=(PIzda "CONDICION",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=12,term="[",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=22}):
       (Elem_acc {estado=13,term=")",accion="reducir",regla=(PIzda "LISTA",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=13,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=15}):
       (Elem_acc {estado=14,term=")",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
       (Elem_acc {estado=15,term=")",accion="reducir",regla=(PIzda "RLISTA",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=15,term=",",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=17}):
       (Elem_acc {estado=16,term=")",accion="reducir",regla=(PIzda "LISTA",Pdcha ["id","RLISTA"]), sig_estado=0}):
       (Elem_acc {estado=17,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=18}):
       (Elem_acc {estado=18,term=")",accion="reducir",regla=(PIzda "RLISTA",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=18,term=",",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=17}):
       (Elem_acc {estado=19,term=")",accion="reducir",regla=(PIzda "RLISTA",Pdcha [",","id","RLISTA"]), sig_estado=0}):
       (Elem_acc {estado=20,term="[",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["(","LISTA",")"]), sig_estado=0}):
       (Elem_acc {estado=20,term="crear",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["(","LISTA",")"]), sig_estado=0}):
       (Elem_acc {estado=20,term="destruir",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["(","LISTA",")"]), sig_estado=0}):
       (Elem_acc {estado=20,term="activacion",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["(","LISTA",")"]), sig_estado=0}):
       (Elem_acc {estado=20,term="evento",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["(","LISTA",")"]), sig_estado=0}):
       (Elem_acc {estado=20,term="id",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["(","LISTA",")"]), sig_estado=0}):
       (Elem_acc {estado=20,term=":",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["(","LISTA",")"]), sig_estado=0}):
       (Elem_acc {estado=20,term="nl",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["(","LISTA",")"]), sig_estado=0}):
       (Elem_acc {estado=20,term="$",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["(","LISTA",")"]), sig_estado=0}):
       (Elem_acc {estado=21,term="id",accion="reducir",regla=(PIzda "OPERACION",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=21,term=":",accion="reducir",regla=(PIzda "OPERACION",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=21,term="nl",accion="reducir",regla=(PIzda "OPERACION",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=21,term="$",accion="reducir",regla=(PIzda "OPERACION",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=21,term="crear",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=32}):
       (Elem_acc {estado=21,term="destruir",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=31}):
       (Elem_acc {estado=21,term="activacion",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=30}):
       (Elem_acc {estado=21,term="evento",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=29}):
       (Elem_acc {estado=22,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=24}):
       (Elem_acc {estado=23,term="]",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=27}):
       (Elem_acc {estado=24,term="=",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=25}):
       (Elem_acc {estado=25,term="lit",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=26}):
       (Elem_acc {estado=26,term="]",accion="reducir",regla=(PIzda "EXPR_BOOL",Pdcha ["id","=","lit"]), sig_estado=0}):
       (Elem_acc {estado=27,term="crear",accion="reducir",regla=(PIzda "CONDICION",Pdcha ["[","EXPR_BOOL","]"]), sig_estado=0}):
       (Elem_acc {estado=27,term="destruir",accion="reducir",regla=(PIzda "CONDICION",Pdcha ["[","EXPR_BOOL","]"]), sig_estado=0}):
       (Elem_acc {estado=27,term="activacion",accion="reducir",regla=(PIzda "CONDICION",Pdcha ["[","EXPR_BOOL","]"]), sig_estado=0}):
       (Elem_acc {estado=27,term="evento",accion="reducir",regla=(PIzda "CONDICION",Pdcha ["[","EXPR_BOOL","]"]), sig_estado=0}):
       (Elem_acc {estado=27,term="id",accion="reducir",regla=(PIzda "CONDICION",Pdcha ["[","EXPR_BOOL","]"]), sig_estado=0}):
       (Elem_acc {estado=27,term=":",accion="reducir",regla=(PIzda "CONDICION",Pdcha ["[","EXPR_BOOL","]"]), sig_estado=0}):
       (Elem_acc {estado=27,term="nl",accion="reducir",regla=(PIzda "CONDICION",Pdcha ["[","EXPR_BOOL","]"]), sig_estado=0}):
       (Elem_acc {estado=27,term="$",accion="reducir",regla=(PIzda "CONDICION",Pdcha ["[","EXPR_BOOL","]"]), sig_estado=0}):
       (Elem_acc {estado=28,term="id",accion="reducir",regla=(PIzda "MENSAJE",Pdcha ["num",":","ITER","id","PARAMETROS","CONDICION","OPERACION"]), sig_estado=0}):
       (Elem_acc {estado=28,term=":",accion="reducir",regla=(PIzda "MENSAJE",Pdcha ["num",":","ITER","id","PARAMETROS","CONDICION","OPERACION"]), sig_estado=0}):
       (Elem_acc {estado=28,term="nl",accion="reducir",regla=(PIzda "MENSAJE",Pdcha ["num",":","ITER","id","PARAMETROS","CONDICION","OPERACION"]), sig_estado=0}):
       (Elem_acc {estado=28,term="$",accion="reducir",regla=(PIzda "MENSAJE",Pdcha ["num",":","ITER","id","PARAMETROS","CONDICION","OPERACION"]), sig_estado=0}):
       (Elem_acc {estado=29,term="id",accion="reducir",regla=(PIzda "OPERACION",Pdcha ["evento"]), sig_estado=0}):
       (Elem_acc {estado=29,term=":",accion="reducir",regla=(PIzda "OPERACION",Pdcha ["evento"]), sig_estado=0}):
       (Elem_acc {estado=29,term="nl",accion="reducir",regla=(PIzda "OPERACION",Pdcha ["evento"]), sig_estado=0}):
       (Elem_acc {estado=29,term="$",accion="reducir",regla=(PIzda "OPERACION",Pdcha ["evento"]), sig_estado=0}):
       (Elem_acc {estado=30,term="id",accion="reducir",regla=(PIzda "OPERACION",Pdcha ["activacion"]), sig_estado=0}):
       (Elem_acc {estado=30,term=":",accion="reducir",regla=(PIzda "OPERACION",Pdcha ["activacion"]), sig_estado=0}):
       (Elem_acc {estado=30,term="nl",accion="reducir",regla=(PIzda "OPERACION",Pdcha ["activacion"]), sig_estado=0}):
       (Elem_acc {estado=30,term="$",accion="reducir",regla=(PIzda "OPERACION",Pdcha ["activacion"]), sig_estado=0}):
       (Elem_acc {estado=31,term="id",accion="reducir",regla=(PIzda "OPERACION",Pdcha ["destruir"]), sig_estado=0}):
       (Elem_acc {estado=31,term=":",accion="reducir",regla=(PIzda "OPERACION",Pdcha ["destruir"]), sig_estado=0}):
       (Elem_acc {estado=31,term="nl",accion="reducir",regla=(PIzda "OPERACION",Pdcha ["destruir"]), sig_estado=0}):
       (Elem_acc {estado=31,term="$",accion="reducir",regla=(PIzda "OPERACION",Pdcha ["destruir"]), sig_estado=0}):
       (Elem_acc {estado=32,term="id",accion="reducir",regla=(PIzda "OPERACION",Pdcha ["crear"]), sig_estado=0}):
       (Elem_acc {estado=32,term=":",accion="reducir",regla=(PIzda "OPERACION",Pdcha ["crear"]), sig_estado=0}):
       (Elem_acc {estado=32,term="nl",accion="reducir",regla=(PIzda "OPERACION",Pdcha ["crear"]), sig_estado=0}):
       (Elem_acc {estado=32,term="$",accion="reducir",regla=(PIzda "OPERACION",Pdcha ["crear"]), sig_estado=0}):
       (Elem_acc {estado=33,term="num",accion="reducir",regla=(PIzda "OBJETO",Pdcha ["NOMBRE_OB","CLASE"]), sig_estado=0}):
       (Elem_acc {estado=34,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=35}):
       (Elem_acc {estado=35,term="num",accion="reducir",regla=(PIzda "CLASE",Pdcha [":","id"]), sig_estado=0}):
       []
data Elem_ira =  Elem_ira {estado2::Int, var::String, sig_estado2::Int}
tabla_ira::[Elem_ira]
tabla_ira=(Elem_ira {estado2=1,var="DC",sig_estado2=4}):
       (Elem_ira {estado2=1,var="OBJETO",sig_estado2=3}):
       (Elem_ira {estado2=1,var="NOMBRE_OB",sig_estado2=2}):
       (Elem_ira {estado2=2,var="CLASE",sig_estado2=33}):
       (Elem_ira {estado2=3,var="MENSAJE",sig_estado2=6}):
       (Elem_ira {estado2=6,var="OBJETO",sig_estado2=36}):
       (Elem_ira {estado2=6,var="NOMBRE_OB",sig_estado2=2}):
       (Elem_ira {estado2=8,var="ITER",sig_estado2=9}):
       (Elem_ira {estado2=11,var="PARAMETROS",sig_estado2=12}):
       (Elem_ira {estado2=12,var="CONDICION",sig_estado2=21}):
       (Elem_ira {estado2=13,var="LISTA",sig_estado2=14}):
       (Elem_ira {estado2=15,var="RLISTA",sig_estado2=16}):
       (Elem_ira {estado2=18,var="RLISTA",sig_estado2=19}):
       (Elem_ira {estado2=21,var="OPERACION",sig_estado2=28}):
       (Elem_ira {estado2=22,var="EXPR_BOOL",sig_estado2=23}):
       []
axioma::String
axioma="PDC"
terminales::[String]
terminales = ["id","*",":","[","]","(",")","=","lit","num","nl",",","crear","destruir","activacion","evento"]
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
