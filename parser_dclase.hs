module Parser_dclase(parser_slr,parser_slr_arbol,visualizar_resto_entrada) where
import Utilidades
import Arbol
--------------- Gramatica: -------------------------------------------------
-- DCLASE
-- id dos_puntos par_ab par_ce llave_ab llave_ce publica privada protegida igual num coma cad punto_coma puntos_susp asterisco asoc 
-- DCLASE RDCLASE ESTEREOTIPO ATRIBUTOS RATRIBUTOS OPERACIONES ROPERACIONES VISIBILIDAD VALOR_INICIAL TIPO CTE PARAMETROS LISTA RLISTA PROPIEDADES ASERTOS RELACION MULT RMULT CARD 
--DCLASE -> id ESTEREOTIPO ATRIBUTOS punto_coma OPERACIONES RDCLASE
--RDCLASE -> RELACION id ESTEREOTIPO ATRIBUTOS punto_coma OPERACIONES RDCLASE
--RDCLASE -> lambda
--ESTEREOTIPO -> cad
--ESTEREOTIPO -> lambda
--ATRIBUTOS -> VISIBILIDAD id TIPO VALOR_INICIAL PROPIEDADES RATRIBUTOS
--RATRIBUTOS -> VISIBILIDAD id TIPO VALOR_INICIAL PROPIEDADES RATRIBUTOS
--RATRIBUTOS -> lambda
--VISIBILIDAD -> publica
--VISIBILIDAD -> privada
--VISIBILIDAD -> protegida
--TIPO -> dos_puntos id
--TIPO -> lambda
--VALOR_INICIAL -> igual CTE
--VALOR_INICIAL -> lambda
--CTE -> num
--CTE -> cad
--OPERACIONES -> VISIBILIDAD id PARAMETROS TIPO PROPIEDADES ROPERACIONES
--ROPERACIONES -> VISIBILIDAD id PARAMETROS TIPO PROPIEDADES ROPERACIONES
--ROPERACIONES -> lambda
--PARAMETROS -> par_ab LISTA par_ce
--PARAMETROS -> lambda
--PROPIEDADES -> llave_ab ASERTOS llave_ce
--PROPIEDADES -> lambda
--ASERTOS -> cad
--LISTA -> id TIPO RLISTA
--RLISTA -> coma id TIPO RLISTA
--RLISTA -> lambda
--RELACION -> asoc MULT cad
--MULT -> num RMULT
--MULT -> lambda
--RMULT -> puntos_susp CARD
--RMULT -> lambda
--CARD -> num
--CARD -> asterisco
--PDCLASE -> DCLASE
----------------------------------------------------------------------------
data Partes = PIzda String|Pdcha [String]
type Regla=(Partes,Partes)
data Elem_acc =  Elem_acc {estado::Int, term::String, accion::String, regla::Regla, sig_estado::Int}
tabla_acc::[Elem_acc]
tabla_acc=(Elem_acc {estado=1,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=3}):
       (Elem_acc {estado=2,term="$",accion="aceptar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=3,term="publica",accion="reducir",regla=(PIzda "ESTEREOTIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=3,term="privada",accion="reducir",regla=(PIzda "ESTEREOTIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=3,term="protegida",accion="reducir",regla=(PIzda "ESTEREOTIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=3,term="cad",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=5}):
       (Elem_acc {estado=4,term="publica",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=10}):
       (Elem_acc {estado=4,term="privada",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=9}):
       (Elem_acc {estado=4,term="protegida",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=5,term="publica",accion="reducir",regla=(PIzda "ESTEREOTIPO",Pdcha ["cad"]), sig_estado=0}):
       (Elem_acc {estado=5,term="privada",accion="reducir",regla=(PIzda "ESTEREOTIPO",Pdcha ["cad"]), sig_estado=0}):
       (Elem_acc {estado=5,term="protegida",accion="reducir",regla=(PIzda "ESTEREOTIPO",Pdcha ["cad"]), sig_estado=0}):
       (Elem_acc {estado=6,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=58}):
       (Elem_acc {estado=7,term="punto_coma",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=11}):
       (Elem_acc {estado=8,term="id",accion="reducir",regla=(PIzda "VISIBILIDAD",Pdcha ["protegida"]), sig_estado=0}):
       (Elem_acc {estado=9,term="id",accion="reducir",regla=(PIzda "VISIBILIDAD",Pdcha ["privada"]), sig_estado=0}):
       (Elem_acc {estado=10,term="id",accion="reducir",regla=(PIzda "VISIBILIDAD",Pdcha ["publica"]), sig_estado=0}):
       (Elem_acc {estado=11,term="publica",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=10}):
       (Elem_acc {estado=11,term="privada",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=9}):
       (Elem_acc {estado=11,term="protegida",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=12,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=31}):
       (Elem_acc {estado=13,term="$",accion="reducir",regla=(PIzda "RDCLASE",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=13,term="asoc",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=16}):
       (Elem_acc {estado=14,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=25}):
       (Elem_acc {estado=15,term="$",accion="reducir",regla=(PIzda "DCLASE",Pdcha ["id","ESTEREOTIPO","ATRIBUTOS","punto_coma","OPERACIONES","RDCLASE"]), sig_estado=0}):
       (Elem_acc {estado=16,term="cad",accion="reducir",regla=(PIzda "MULT",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=16,term="num",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=18}):
       (Elem_acc {estado=17,term="cad",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=24}):
       (Elem_acc {estado=18,term="cad",accion="reducir",regla=(PIzda "RMULT",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=18,term="puntos_susp",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
       (Elem_acc {estado=19,term="cad",accion="reducir",regla=(PIzda "MULT",Pdcha ["num","RMULT"]), sig_estado=0}):
       (Elem_acc {estado=20,term="num",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=23}):
       (Elem_acc {estado=20,term="asterisco",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=22}):
       (Elem_acc {estado=21,term="cad",accion="reducir",regla=(PIzda "RMULT",Pdcha ["puntos_susp","CARD"]), sig_estado=0}):
       (Elem_acc {estado=22,term="cad",accion="reducir",regla=(PIzda "CARD",Pdcha ["asterisco"]), sig_estado=0}):
       (Elem_acc {estado=23,term="cad",accion="reducir",regla=(PIzda "CARD",Pdcha ["num"]), sig_estado=0}):
       (Elem_acc {estado=24,term="id",accion="reducir",regla=(PIzda "RELACION",Pdcha ["asoc","MULT","cad"]), sig_estado=0}):
       (Elem_acc {estado=25,term="publica",accion="reducir",regla=(PIzda "ESTEREOTIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=25,term="privada",accion="reducir",regla=(PIzda "ESTEREOTIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=25,term="protegida",accion="reducir",regla=(PIzda "ESTEREOTIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=25,term="cad",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=5}):
       (Elem_acc {estado=26,term="publica",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=10}):
       (Elem_acc {estado=26,term="privada",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=9}):
       (Elem_acc {estado=26,term="protegida",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=27,term="punto_coma",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=28}):
       (Elem_acc {estado=28,term="publica",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=10}):
       (Elem_acc {estado=28,term="privada",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=9}):
       (Elem_acc {estado=28,term="protegida",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=29,term="$",accion="reducir",regla=(PIzda "RDCLASE",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=29,term="asoc",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=16}):
       (Elem_acc {estado=30,term="$",accion="reducir",regla=(PIzda "RDCLASE",Pdcha ["RELACION","id","ESTEREOTIPO","ATRIBUTOS","punto_coma","OPERACIONES","RDCLASE"]), sig_estado=0}):
       (Elem_acc {estado=31,term="dos_puntos",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=31,term="llave_ab",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=31,term="publica",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=31,term="privada",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=31,term="protegida",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=31,term="asoc",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=31,term="$",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=31,term="par_ab",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=33}):
       (Elem_acc {estado=32,term="igual",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=32,term="punto_coma",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=32,term="llave_ab",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=32,term="publica",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=32,term="privada",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=32,term="protegida",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=32,term="asoc",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=32,term="$",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=32,term="coma",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=32,term="par_ce",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=32,term="dos_puntos",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=37}):
       (Elem_acc {estado=33,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=35}):
       (Elem_acc {estado=34,term="par_ce",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=44}):
       (Elem_acc {estado=35,term="igual",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=35,term="punto_coma",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=35,term="llave_ab",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=35,term="publica",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=35,term="privada",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=35,term="protegida",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=35,term="asoc",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=35,term="$",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=35,term="coma",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=35,term="par_ce",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=35,term="dos_puntos",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=37}):
       (Elem_acc {estado=36,term="par_ce",accion="reducir",regla=(PIzda "RLISTA",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=36,term="coma",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=40}):
       (Elem_acc {estado=37,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=38}):
       (Elem_acc {estado=38,term="igual",accion="reducir",regla=(PIzda "TIPO",Pdcha ["dos_puntos","id"]), sig_estado=0}):
       (Elem_acc {estado=38,term="punto_coma",accion="reducir",regla=(PIzda "TIPO",Pdcha ["dos_puntos","id"]), sig_estado=0}):
       (Elem_acc {estado=38,term="llave_ab",accion="reducir",regla=(PIzda "TIPO",Pdcha ["dos_puntos","id"]), sig_estado=0}):
       (Elem_acc {estado=38,term="publica",accion="reducir",regla=(PIzda "TIPO",Pdcha ["dos_puntos","id"]), sig_estado=0}):
       (Elem_acc {estado=38,term="privada",accion="reducir",regla=(PIzda "TIPO",Pdcha ["dos_puntos","id"]), sig_estado=0}):
       (Elem_acc {estado=38,term="protegida",accion="reducir",regla=(PIzda "TIPO",Pdcha ["dos_puntos","id"]), sig_estado=0}):
       (Elem_acc {estado=38,term="asoc",accion="reducir",regla=(PIzda "TIPO",Pdcha ["dos_puntos","id"]), sig_estado=0}):
       (Elem_acc {estado=38,term="$",accion="reducir",regla=(PIzda "TIPO",Pdcha ["dos_puntos","id"]), sig_estado=0}):
       (Elem_acc {estado=38,term="coma",accion="reducir",regla=(PIzda "TIPO",Pdcha ["dos_puntos","id"]), sig_estado=0}):
       (Elem_acc {estado=38,term="par_ce",accion="reducir",regla=(PIzda "TIPO",Pdcha ["dos_puntos","id"]), sig_estado=0}):
       (Elem_acc {estado=39,term="par_ce",accion="reducir",regla=(PIzda "LISTA",Pdcha ["id","TIPO","RLISTA"]), sig_estado=0}):
       (Elem_acc {estado=40,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=41}):
       (Elem_acc {estado=41,term="igual",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=41,term="punto_coma",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=41,term="llave_ab",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=41,term="publica",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=41,term="privada",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=41,term="protegida",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=41,term="asoc",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=41,term="$",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=41,term="coma",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=41,term="par_ce",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=41,term="dos_puntos",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=37}):
       (Elem_acc {estado=42,term="par_ce",accion="reducir",regla=(PIzda "RLISTA",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=42,term="coma",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=40}):
       (Elem_acc {estado=43,term="par_ce",accion="reducir",regla=(PIzda "RLISTA",Pdcha ["coma","id","TIPO","RLISTA"]), sig_estado=0}):
       (Elem_acc {estado=44,term="dos_puntos",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["par_ab","LISTA","par_ce"]), sig_estado=0}):
       (Elem_acc {estado=44,term="llave_ab",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["par_ab","LISTA","par_ce"]), sig_estado=0}):
       (Elem_acc {estado=44,term="publica",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["par_ab","LISTA","par_ce"]), sig_estado=0}):
       (Elem_acc {estado=44,term="privada",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["par_ab","LISTA","par_ce"]), sig_estado=0}):
       (Elem_acc {estado=44,term="protegida",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["par_ab","LISTA","par_ce"]), sig_estado=0}):
       (Elem_acc {estado=44,term="asoc",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["par_ab","LISTA","par_ce"]), sig_estado=0}):
       (Elem_acc {estado=44,term="$",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["par_ab","LISTA","par_ce"]), sig_estado=0}):
       (Elem_acc {estado=45,term="punto_coma",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=45,term="publica",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=45,term="privada",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=45,term="protegida",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=45,term="asoc",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=45,term="$",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=45,term="llave_ab",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=47}):
       (Elem_acc {estado=46,term="asoc",accion="reducir",regla=(PIzda "ROPERACIONES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=46,term="$",accion="reducir",regla=(PIzda "ROPERACIONES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=46,term="publica",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=10}):
       (Elem_acc {estado=46,term="privada",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=9}):
       (Elem_acc {estado=46,term="protegida",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=47,term="cad",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=49}):
       (Elem_acc {estado=48,term="llave_ce",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=50}):
       (Elem_acc {estado=49,term="llave_ce",accion="reducir",regla=(PIzda "ASERTOS",Pdcha ["cad"]), sig_estado=0}):
       (Elem_acc {estado=50,term="punto_coma",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["llave_ab","ASERTOS","llave_ce"]), sig_estado=0}):
       (Elem_acc {estado=50,term="publica",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["llave_ab","ASERTOS","llave_ce"]), sig_estado=0}):
       (Elem_acc {estado=50,term="privada",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["llave_ab","ASERTOS","llave_ce"]), sig_estado=0}):
       (Elem_acc {estado=50,term="protegida",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["llave_ab","ASERTOS","llave_ce"]), sig_estado=0}):
       (Elem_acc {estado=50,term="asoc",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["llave_ab","ASERTOS","llave_ce"]), sig_estado=0}):
       (Elem_acc {estado=50,term="$",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["llave_ab","ASERTOS","llave_ce"]), sig_estado=0}):
       (Elem_acc {estado=51,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=53}):
       (Elem_acc {estado=52,term="asoc",accion="reducir",regla=(PIzda "OPERACIONES",Pdcha ["VISIBILIDAD","id","PARAMETROS","TIPO","PROPIEDADES","ROPERACIONES"]), sig_estado=0}):
       (Elem_acc {estado=52,term="$",accion="reducir",regla=(PIzda "OPERACIONES",Pdcha ["VISIBILIDAD","id","PARAMETROS","TIPO","PROPIEDADES","ROPERACIONES"]), sig_estado=0}):
       (Elem_acc {estado=53,term="dos_puntos",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=53,term="llave_ab",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=53,term="publica",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=53,term="privada",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=53,term="protegida",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=53,term="asoc",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=53,term="$",accion="reducir",regla=(PIzda "PARAMETROS",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=53,term="par_ab",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=33}):
       (Elem_acc {estado=54,term="igual",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=54,term="punto_coma",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=54,term="llave_ab",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=54,term="publica",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=54,term="privada",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=54,term="protegida",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=54,term="asoc",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=54,term="$",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=54,term="coma",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=54,term="par_ce",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=54,term="dos_puntos",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=37}):
       (Elem_acc {estado=55,term="punto_coma",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=55,term="publica",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=55,term="privada",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=55,term="protegida",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=55,term="asoc",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=55,term="$",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=55,term="llave_ab",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=47}):
       (Elem_acc {estado=56,term="asoc",accion="reducir",regla=(PIzda "ROPERACIONES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=56,term="$",accion="reducir",regla=(PIzda "ROPERACIONES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=56,term="publica",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=10}):
       (Elem_acc {estado=56,term="privada",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=9}):
       (Elem_acc {estado=56,term="protegida",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=57,term="asoc",accion="reducir",regla=(PIzda "ROPERACIONES",Pdcha ["VISIBILIDAD","id","PARAMETROS","TIPO","PROPIEDADES","ROPERACIONES"]), sig_estado=0}):
       (Elem_acc {estado=57,term="$",accion="reducir",regla=(PIzda "ROPERACIONES",Pdcha ["VISIBILIDAD","id","PARAMETROS","TIPO","PROPIEDADES","ROPERACIONES"]), sig_estado=0}):
       (Elem_acc {estado=58,term="igual",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=58,term="punto_coma",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=58,term="llave_ab",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=58,term="publica",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=58,term="privada",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=58,term="protegida",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=58,term="asoc",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=58,term="$",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=58,term="coma",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=58,term="par_ce",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=58,term="dos_puntos",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=37}):
       (Elem_acc {estado=59,term="llave_ab",accion="reducir",regla=(PIzda "VALOR_INICIAL",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=59,term="publica",accion="reducir",regla=(PIzda "VALOR_INICIAL",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=59,term="privada",accion="reducir",regla=(PIzda "VALOR_INICIAL",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=59,term="protegida",accion="reducir",regla=(PIzda "VALOR_INICIAL",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=59,term="punto_coma",accion="reducir",regla=(PIzda "VALOR_INICIAL",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=59,term="igual",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=61}):
       (Elem_acc {estado=60,term="punto_coma",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=60,term="publica",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=60,term="privada",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=60,term="protegida",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=60,term="asoc",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=60,term="$",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=60,term="llave_ab",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=47}):
       (Elem_acc {estado=61,term="num",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=64}):
       (Elem_acc {estado=61,term="cad",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=63}):
       (Elem_acc {estado=62,term="llave_ab",accion="reducir",regla=(PIzda "VALOR_INICIAL",Pdcha ["igual","CTE"]), sig_estado=0}):
       (Elem_acc {estado=62,term="publica",accion="reducir",regla=(PIzda "VALOR_INICIAL",Pdcha ["igual","CTE"]), sig_estado=0}):
       (Elem_acc {estado=62,term="privada",accion="reducir",regla=(PIzda "VALOR_INICIAL",Pdcha ["igual","CTE"]), sig_estado=0}):
       (Elem_acc {estado=62,term="protegida",accion="reducir",regla=(PIzda "VALOR_INICIAL",Pdcha ["igual","CTE"]), sig_estado=0}):
       (Elem_acc {estado=62,term="punto_coma",accion="reducir",regla=(PIzda "VALOR_INICIAL",Pdcha ["igual","CTE"]), sig_estado=0}):
       (Elem_acc {estado=63,term="llave_ab",accion="reducir",regla=(PIzda "CTE",Pdcha ["cad"]), sig_estado=0}):
       (Elem_acc {estado=63,term="publica",accion="reducir",regla=(PIzda "CTE",Pdcha ["cad"]), sig_estado=0}):
       (Elem_acc {estado=63,term="privada",accion="reducir",regla=(PIzda "CTE",Pdcha ["cad"]), sig_estado=0}):
       (Elem_acc {estado=63,term="protegida",accion="reducir",regla=(PIzda "CTE",Pdcha ["cad"]), sig_estado=0}):
       (Elem_acc {estado=63,term="punto_coma",accion="reducir",regla=(PIzda "CTE",Pdcha ["cad"]), sig_estado=0}):
       (Elem_acc {estado=64,term="llave_ab",accion="reducir",regla=(PIzda "CTE",Pdcha ["num"]), sig_estado=0}):
       (Elem_acc {estado=64,term="publica",accion="reducir",regla=(PIzda "CTE",Pdcha ["num"]), sig_estado=0}):
       (Elem_acc {estado=64,term="privada",accion="reducir",regla=(PIzda "CTE",Pdcha ["num"]), sig_estado=0}):
       (Elem_acc {estado=64,term="protegida",accion="reducir",regla=(PIzda "CTE",Pdcha ["num"]), sig_estado=0}):
       (Elem_acc {estado=64,term="punto_coma",accion="reducir",regla=(PIzda "CTE",Pdcha ["num"]), sig_estado=0}):
       (Elem_acc {estado=65,term="punto_coma",accion="reducir",regla=(PIzda "RATRIBUTOS",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=65,term="publica",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=10}):
       (Elem_acc {estado=65,term="privada",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=9}):
       (Elem_acc {estado=65,term="protegida",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=66,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=68}):
       (Elem_acc {estado=67,term="punto_coma",accion="reducir",regla=(PIzda "ATRIBUTOS",Pdcha ["VISIBILIDAD","id","TIPO","VALOR_INICIAL","PROPIEDADES","RATRIBUTOS"]), sig_estado=0}):
       (Elem_acc {estado=68,term="igual",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=68,term="punto_coma",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=68,term="llave_ab",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=68,term="publica",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=68,term="privada",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=68,term="protegida",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=68,term="asoc",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=68,term="$",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=68,term="coma",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=68,term="par_ce",accion="reducir",regla=(PIzda "TIPO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=68,term="dos_puntos",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=37}):
       (Elem_acc {estado=69,term="llave_ab",accion="reducir",regla=(PIzda "VALOR_INICIAL",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=69,term="publica",accion="reducir",regla=(PIzda "VALOR_INICIAL",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=69,term="privada",accion="reducir",regla=(PIzda "VALOR_INICIAL",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=69,term="protegida",accion="reducir",regla=(PIzda "VALOR_INICIAL",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=69,term="punto_coma",accion="reducir",regla=(PIzda "VALOR_INICIAL",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=69,term="igual",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=61}):
       (Elem_acc {estado=70,term="punto_coma",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=70,term="publica",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=70,term="privada",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=70,term="protegida",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=70,term="asoc",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=70,term="$",accion="reducir",regla=(PIzda "PROPIEDADES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=70,term="llave_ab",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=47}):
       (Elem_acc {estado=71,term="punto_coma",accion="reducir",regla=(PIzda "RATRIBUTOS",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=71,term="publica",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=10}):
       (Elem_acc {estado=71,term="privada",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=9}):
       (Elem_acc {estado=71,term="protegida",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=72,term="punto_coma",accion="reducir",regla=(PIzda "RATRIBUTOS",Pdcha ["VISIBILIDAD","id","TIPO","VALOR_INICIAL","PROPIEDADES","RATRIBUTOS"]), sig_estado=0}):
       []
data Elem_ira =  Elem_ira {estado2::Int, var::String, sig_estado2::Int}
tabla_ira::[Elem_ira]
tabla_ira=(Elem_ira {estado2=1,var="DCLASE",sig_estado2=2}):
       (Elem_ira {estado2=3,var="ESTEREOTIPO",sig_estado2=4}):
       (Elem_ira {estado2=4,var="ATRIBUTOS",sig_estado2=7}):
       (Elem_ira {estado2=4,var="VISIBILIDAD",sig_estado2=6}):
       (Elem_ira {estado2=11,var="OPERACIONES",sig_estado2=13}):
       (Elem_ira {estado2=11,var="VISIBILIDAD",sig_estado2=12}):
       (Elem_ira {estado2=13,var="RDCLASE",sig_estado2=15}):
       (Elem_ira {estado2=13,var="RELACION",sig_estado2=14}):
       (Elem_ira {estado2=16,var="MULT",sig_estado2=17}):
       (Elem_ira {estado2=18,var="RMULT",sig_estado2=19}):
       (Elem_ira {estado2=20,var="CARD",sig_estado2=21}):
       (Elem_ira {estado2=25,var="ESTEREOTIPO",sig_estado2=26}):
       (Elem_ira {estado2=26,var="ATRIBUTOS",sig_estado2=27}):
       (Elem_ira {estado2=26,var="VISIBILIDAD",sig_estado2=6}):
       (Elem_ira {estado2=28,var="OPERACIONES",sig_estado2=29}):
       (Elem_ira {estado2=28,var="VISIBILIDAD",sig_estado2=12}):
       (Elem_ira {estado2=29,var="RDCLASE",sig_estado2=30}):
       (Elem_ira {estado2=29,var="RELACION",sig_estado2=14}):
       (Elem_ira {estado2=31,var="PARAMETROS",sig_estado2=32}):
       (Elem_ira {estado2=32,var="TIPO",sig_estado2=45}):
       (Elem_ira {estado2=33,var="LISTA",sig_estado2=34}):
       (Elem_ira {estado2=35,var="TIPO",sig_estado2=36}):
       (Elem_ira {estado2=36,var="RLISTA",sig_estado2=39}):
       (Elem_ira {estado2=41,var="TIPO",sig_estado2=42}):
       (Elem_ira {estado2=42,var="RLISTA",sig_estado2=43}):
       (Elem_ira {estado2=45,var="PROPIEDADES",sig_estado2=46}):
       (Elem_ira {estado2=46,var="ROPERACIONES",sig_estado2=52}):
       (Elem_ira {estado2=46,var="VISIBILIDAD",sig_estado2=51}):
       (Elem_ira {estado2=47,var="ASERTOS",sig_estado2=48}):
       (Elem_ira {estado2=53,var="PARAMETROS",sig_estado2=54}):
       (Elem_ira {estado2=54,var="TIPO",sig_estado2=55}):
       (Elem_ira {estado2=55,var="PROPIEDADES",sig_estado2=56}):
       (Elem_ira {estado2=56,var="ROPERACIONES",sig_estado2=57}):
       (Elem_ira {estado2=56,var="VISIBILIDAD",sig_estado2=51}):
       (Elem_ira {estado2=58,var="TIPO",sig_estado2=59}):
       (Elem_ira {estado2=59,var="VALOR_INICIAL",sig_estado2=60}):
       (Elem_ira {estado2=60,var="PROPIEDADES",sig_estado2=65}):
       (Elem_ira {estado2=61,var="CTE",sig_estado2=62}):
       (Elem_ira {estado2=65,var="RATRIBUTOS",sig_estado2=67}):
       (Elem_ira {estado2=65,var="VISIBILIDAD",sig_estado2=66}):
       (Elem_ira {estado2=68,var="TIPO",sig_estado2=69}):
       (Elem_ira {estado2=69,var="VALOR_INICIAL",sig_estado2=70}):
       (Elem_ira {estado2=70,var="PROPIEDADES",sig_estado2=71}):
       (Elem_ira {estado2=71,var="RATRIBUTOS",sig_estado2=72}):
       (Elem_ira {estado2=71,var="VISIBILIDAD",sig_estado2=66}):
       []
axioma::String
axioma="PDCLASE"
terminales::[String]
terminales = ["id","dos_puntos","par_ab","par_ce","llave_ab","llave_ce","publica","privada","protegida","igual","num","coma","cad","punto_coma","puntos_susp","asterisco","asoc"]
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
