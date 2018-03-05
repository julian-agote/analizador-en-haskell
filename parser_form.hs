module Parser_form(parser_slr,parser_slr_arbol,visualizar_resto_entrada) where
import Utilidades
import Arbol
--------------- Gramatica: -------------------------------------------------
-- FORM
-- id lit coma par_ab par_ce x e s m c f t v-r v-b 
-- FORM DESCP BLOQUES RBLOQUES CAMPOS RCAMPOS ATTRC OBLIG ESM LISTA VAL VALREG PROCESO 
--FORM -> id DESCP BLOQUES
--DESCP -> lit
--BLOQUES -> id CAMPOS VALREG PROCESO RBLOQUES
--RBLOQUES -> coma id CAMPOS VALREG PROCESO RBLOQUES
--RBLOQUES -> lambda
--CAMPOS -> par_ab id ATTRC RCAMPOS par_ce
--RCAMPOS -> coma id ATTRC RCAMPOS
--RCAMPOS -> lambda
--ATTRC -> OBLIG ATTRC
--ATTRC -> ESM ATTRC
--ATTRC -> LISTA ATTRC
--ATTRC -> VAL ATTRC
--ATTRC -> lambda
--OBLIG -> x
--ESM -> e
--ESM -> s
--ESM -> m
--ESM -> c
--LISTA -> f
--LISTA -> t
--VAL -> lit
--VALREG -> v-r lit
--VALREG -> lambda
--PROCESO -> v-b lit
--PROCESO -> lambda
--PFORM -> FORM
----------------------------------------------------------------------------
data Partes = PIzda String|Pdcha [String]
type Regla=(Partes,Partes)
data Elem_acc =  Elem_acc {estado::Int, term::String, accion::String, regla::Regla, sig_estado::Int}
tabla_acc::[Elem_acc]
tabla_acc=(Elem_acc {estado=1,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=3}):
       (Elem_acc {estado=2,term="$",accion="aceptar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=3,term="lit",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=5}):
       (Elem_acc {estado=4,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=7}):
       (Elem_acc {estado=5,term="id",accion="reducir",regla=(PIzda "DESCP",Pdcha ["lit"]), sig_estado=0}):
       (Elem_acc {estado=6,term="$",accion="reducir",regla=(PIzda "FORM",Pdcha ["id","DESCP","BLOQUES"]), sig_estado=0}):
       (Elem_acc {estado=7,term="par_ab",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=9}):
       (Elem_acc {estado=8,term="v-b",accion="reducir",regla=(PIzda "VALREG",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=8,term="coma",accion="reducir",regla=(PIzda "VALREG",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=8,term="$",accion="reducir",regla=(PIzda "VALREG",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=8,term="v-r",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=35}):
       (Elem_acc {estado=9,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=10}):
       (Elem_acc {estado=10,term="v-r",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=10,term="v-b",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=10,term="$",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=10,term="coma",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=10,term="par_ce",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=10,term="x",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=22}):
       (Elem_acc {estado=10,term="e",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=21}):
       (Elem_acc {estado=10,term="s",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
       (Elem_acc {estado=10,term="m",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=19}):
       (Elem_acc {estado=10,term="c",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=18}):
       (Elem_acc {estado=10,term="f",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=17}):
       (Elem_acc {estado=10,term="t",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=16}):
       (Elem_acc {estado=10,term="lit",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=23}):
       (Elem_acc {estado=11,term="v-r",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=11,term="v-b",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=11,term="$",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=11,term="coma",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=11,term="par_ce",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=11,term="x",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=22}):
       (Elem_acc {estado=11,term="e",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=21}):
       (Elem_acc {estado=11,term="s",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
       (Elem_acc {estado=11,term="m",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=19}):
       (Elem_acc {estado=11,term="c",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=18}):
       (Elem_acc {estado=11,term="f",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=17}):
       (Elem_acc {estado=11,term="t",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=16}):
       (Elem_acc {estado=11,term="lit",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=23}):
       (Elem_acc {estado=12,term="v-r",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=12,term="v-b",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=12,term="$",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=12,term="coma",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=12,term="par_ce",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=12,term="x",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=22}):
       (Elem_acc {estado=12,term="e",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=21}):
       (Elem_acc {estado=12,term="s",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
       (Elem_acc {estado=12,term="m",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=19}):
       (Elem_acc {estado=12,term="c",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=18}):
       (Elem_acc {estado=12,term="f",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=17}):
       (Elem_acc {estado=12,term="t",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=16}):
       (Elem_acc {estado=12,term="lit",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=23}):
       (Elem_acc {estado=13,term="v-r",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=13,term="v-b",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=13,term="$",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=13,term="coma",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=13,term="par_ce",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=13,term="x",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=22}):
       (Elem_acc {estado=13,term="e",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=21}):
       (Elem_acc {estado=13,term="s",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
       (Elem_acc {estado=13,term="m",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=19}):
       (Elem_acc {estado=13,term="c",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=18}):
       (Elem_acc {estado=13,term="f",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=17}):
       (Elem_acc {estado=13,term="t",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=16}):
       (Elem_acc {estado=13,term="lit",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=23}):
       (Elem_acc {estado=14,term="v-r",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=14,term="v-b",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=14,term="$",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=14,term="coma",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=14,term="par_ce",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=14,term="x",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=22}):
       (Elem_acc {estado=14,term="e",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=21}):
       (Elem_acc {estado=14,term="s",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
       (Elem_acc {estado=14,term="m",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=19}):
       (Elem_acc {estado=14,term="c",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=18}):
       (Elem_acc {estado=14,term="f",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=17}):
       (Elem_acc {estado=14,term="t",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=16}):
       (Elem_acc {estado=14,term="lit",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=23}):
       (Elem_acc {estado=15,term="par_ce",accion="reducir",regla=(PIzda "RCAMPOS",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=15,term="coma",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=25}):
       (Elem_acc {estado=16,term="x",accion="reducir",regla=(PIzda "LISTA",Pdcha ["t"]), sig_estado=0}):
       (Elem_acc {estado=16,term="e",accion="reducir",regla=(PIzda "LISTA",Pdcha ["t"]), sig_estado=0}):
       (Elem_acc {estado=16,term="s",accion="reducir",regla=(PIzda "LISTA",Pdcha ["t"]), sig_estado=0}):
       (Elem_acc {estado=16,term="m",accion="reducir",regla=(PIzda "LISTA",Pdcha ["t"]), sig_estado=0}):
       (Elem_acc {estado=16,term="c",accion="reducir",regla=(PIzda "LISTA",Pdcha ["t"]), sig_estado=0}):
       (Elem_acc {estado=16,term="f",accion="reducir",regla=(PIzda "LISTA",Pdcha ["t"]), sig_estado=0}):
       (Elem_acc {estado=16,term="t",accion="reducir",regla=(PIzda "LISTA",Pdcha ["t"]), sig_estado=0}):
       (Elem_acc {estado=16,term="lit",accion="reducir",regla=(PIzda "LISTA",Pdcha ["t"]), sig_estado=0}):
       (Elem_acc {estado=16,term="v-r",accion="reducir",regla=(PIzda "LISTA",Pdcha ["t"]), sig_estado=0}):
       (Elem_acc {estado=16,term="v-b",accion="reducir",regla=(PIzda "LISTA",Pdcha ["t"]), sig_estado=0}):
       (Elem_acc {estado=16,term="$",accion="reducir",regla=(PIzda "LISTA",Pdcha ["t"]), sig_estado=0}):
       (Elem_acc {estado=16,term="coma",accion="reducir",regla=(PIzda "LISTA",Pdcha ["t"]), sig_estado=0}):
       (Elem_acc {estado=16,term="par_ce",accion="reducir",regla=(PIzda "LISTA",Pdcha ["t"]), sig_estado=0}):
       (Elem_acc {estado=17,term="x",accion="reducir",regla=(PIzda "LISTA",Pdcha ["f"]), sig_estado=0}):
       (Elem_acc {estado=17,term="e",accion="reducir",regla=(PIzda "LISTA",Pdcha ["f"]), sig_estado=0}):
       (Elem_acc {estado=17,term="s",accion="reducir",regla=(PIzda "LISTA",Pdcha ["f"]), sig_estado=0}):
       (Elem_acc {estado=17,term="m",accion="reducir",regla=(PIzda "LISTA",Pdcha ["f"]), sig_estado=0}):
       (Elem_acc {estado=17,term="c",accion="reducir",regla=(PIzda "LISTA",Pdcha ["f"]), sig_estado=0}):
       (Elem_acc {estado=17,term="f",accion="reducir",regla=(PIzda "LISTA",Pdcha ["f"]), sig_estado=0}):
       (Elem_acc {estado=17,term="t",accion="reducir",regla=(PIzda "LISTA",Pdcha ["f"]), sig_estado=0}):
       (Elem_acc {estado=17,term="lit",accion="reducir",regla=(PIzda "LISTA",Pdcha ["f"]), sig_estado=0}):
       (Elem_acc {estado=17,term="v-r",accion="reducir",regla=(PIzda "LISTA",Pdcha ["f"]), sig_estado=0}):
       (Elem_acc {estado=17,term="v-b",accion="reducir",regla=(PIzda "LISTA",Pdcha ["f"]), sig_estado=0}):
       (Elem_acc {estado=17,term="$",accion="reducir",regla=(PIzda "LISTA",Pdcha ["f"]), sig_estado=0}):
       (Elem_acc {estado=17,term="coma",accion="reducir",regla=(PIzda "LISTA",Pdcha ["f"]), sig_estado=0}):
       (Elem_acc {estado=17,term="par_ce",accion="reducir",regla=(PIzda "LISTA",Pdcha ["f"]), sig_estado=0}):
       (Elem_acc {estado=18,term="x",accion="reducir",regla=(PIzda "ESM",Pdcha ["c"]), sig_estado=0}):
       (Elem_acc {estado=18,term="e",accion="reducir",regla=(PIzda "ESM",Pdcha ["c"]), sig_estado=0}):
       (Elem_acc {estado=18,term="s",accion="reducir",regla=(PIzda "ESM",Pdcha ["c"]), sig_estado=0}):
       (Elem_acc {estado=18,term="m",accion="reducir",regla=(PIzda "ESM",Pdcha ["c"]), sig_estado=0}):
       (Elem_acc {estado=18,term="c",accion="reducir",regla=(PIzda "ESM",Pdcha ["c"]), sig_estado=0}):
       (Elem_acc {estado=18,term="f",accion="reducir",regla=(PIzda "ESM",Pdcha ["c"]), sig_estado=0}):
       (Elem_acc {estado=18,term="t",accion="reducir",regla=(PIzda "ESM",Pdcha ["c"]), sig_estado=0}):
       (Elem_acc {estado=18,term="lit",accion="reducir",regla=(PIzda "ESM",Pdcha ["c"]), sig_estado=0}):
       (Elem_acc {estado=18,term="v-r",accion="reducir",regla=(PIzda "ESM",Pdcha ["c"]), sig_estado=0}):
       (Elem_acc {estado=18,term="v-b",accion="reducir",regla=(PIzda "ESM",Pdcha ["c"]), sig_estado=0}):
       (Elem_acc {estado=18,term="$",accion="reducir",regla=(PIzda "ESM",Pdcha ["c"]), sig_estado=0}):
       (Elem_acc {estado=18,term="coma",accion="reducir",regla=(PIzda "ESM",Pdcha ["c"]), sig_estado=0}):
       (Elem_acc {estado=18,term="par_ce",accion="reducir",regla=(PIzda "ESM",Pdcha ["c"]), sig_estado=0}):
       (Elem_acc {estado=19,term="x",accion="reducir",regla=(PIzda "ESM",Pdcha ["m"]), sig_estado=0}):
       (Elem_acc {estado=19,term="e",accion="reducir",regla=(PIzda "ESM",Pdcha ["m"]), sig_estado=0}):
       (Elem_acc {estado=19,term="s",accion="reducir",regla=(PIzda "ESM",Pdcha ["m"]), sig_estado=0}):
       (Elem_acc {estado=19,term="m",accion="reducir",regla=(PIzda "ESM",Pdcha ["m"]), sig_estado=0}):
       (Elem_acc {estado=19,term="c",accion="reducir",regla=(PIzda "ESM",Pdcha ["m"]), sig_estado=0}):
       (Elem_acc {estado=19,term="f",accion="reducir",regla=(PIzda "ESM",Pdcha ["m"]), sig_estado=0}):
       (Elem_acc {estado=19,term="t",accion="reducir",regla=(PIzda "ESM",Pdcha ["m"]), sig_estado=0}):
       (Elem_acc {estado=19,term="lit",accion="reducir",regla=(PIzda "ESM",Pdcha ["m"]), sig_estado=0}):
       (Elem_acc {estado=19,term="v-r",accion="reducir",regla=(PIzda "ESM",Pdcha ["m"]), sig_estado=0}):
       (Elem_acc {estado=19,term="v-b",accion="reducir",regla=(PIzda "ESM",Pdcha ["m"]), sig_estado=0}):
       (Elem_acc {estado=19,term="$",accion="reducir",regla=(PIzda "ESM",Pdcha ["m"]), sig_estado=0}):
       (Elem_acc {estado=19,term="coma",accion="reducir",regla=(PIzda "ESM",Pdcha ["m"]), sig_estado=0}):
       (Elem_acc {estado=19,term="par_ce",accion="reducir",regla=(PIzda "ESM",Pdcha ["m"]), sig_estado=0}):
       (Elem_acc {estado=20,term="x",accion="reducir",regla=(PIzda "ESM",Pdcha ["s"]), sig_estado=0}):
       (Elem_acc {estado=20,term="e",accion="reducir",regla=(PIzda "ESM",Pdcha ["s"]), sig_estado=0}):
       (Elem_acc {estado=20,term="s",accion="reducir",regla=(PIzda "ESM",Pdcha ["s"]), sig_estado=0}):
       (Elem_acc {estado=20,term="m",accion="reducir",regla=(PIzda "ESM",Pdcha ["s"]), sig_estado=0}):
       (Elem_acc {estado=20,term="c",accion="reducir",regla=(PIzda "ESM",Pdcha ["s"]), sig_estado=0}):
       (Elem_acc {estado=20,term="f",accion="reducir",regla=(PIzda "ESM",Pdcha ["s"]), sig_estado=0}):
       (Elem_acc {estado=20,term="t",accion="reducir",regla=(PIzda "ESM",Pdcha ["s"]), sig_estado=0}):
       (Elem_acc {estado=20,term="lit",accion="reducir",regla=(PIzda "ESM",Pdcha ["s"]), sig_estado=0}):
       (Elem_acc {estado=20,term="v-r",accion="reducir",regla=(PIzda "ESM",Pdcha ["s"]), sig_estado=0}):
       (Elem_acc {estado=20,term="v-b",accion="reducir",regla=(PIzda "ESM",Pdcha ["s"]), sig_estado=0}):
       (Elem_acc {estado=20,term="$",accion="reducir",regla=(PIzda "ESM",Pdcha ["s"]), sig_estado=0}):
       (Elem_acc {estado=20,term="coma",accion="reducir",regla=(PIzda "ESM",Pdcha ["s"]), sig_estado=0}):
       (Elem_acc {estado=20,term="par_ce",accion="reducir",regla=(PIzda "ESM",Pdcha ["s"]), sig_estado=0}):
       (Elem_acc {estado=21,term="x",accion="reducir",regla=(PIzda "ESM",Pdcha ["e"]), sig_estado=0}):
       (Elem_acc {estado=21,term="e",accion="reducir",regla=(PIzda "ESM",Pdcha ["e"]), sig_estado=0}):
       (Elem_acc {estado=21,term="s",accion="reducir",regla=(PIzda "ESM",Pdcha ["e"]), sig_estado=0}):
       (Elem_acc {estado=21,term="m",accion="reducir",regla=(PIzda "ESM",Pdcha ["e"]), sig_estado=0}):
       (Elem_acc {estado=21,term="c",accion="reducir",regla=(PIzda "ESM",Pdcha ["e"]), sig_estado=0}):
       (Elem_acc {estado=21,term="f",accion="reducir",regla=(PIzda "ESM",Pdcha ["e"]), sig_estado=0}):
       (Elem_acc {estado=21,term="t",accion="reducir",regla=(PIzda "ESM",Pdcha ["e"]), sig_estado=0}):
       (Elem_acc {estado=21,term="lit",accion="reducir",regla=(PIzda "ESM",Pdcha ["e"]), sig_estado=0}):
       (Elem_acc {estado=21,term="v-r",accion="reducir",regla=(PIzda "ESM",Pdcha ["e"]), sig_estado=0}):
       (Elem_acc {estado=21,term="v-b",accion="reducir",regla=(PIzda "ESM",Pdcha ["e"]), sig_estado=0}):
       (Elem_acc {estado=21,term="$",accion="reducir",regla=(PIzda "ESM",Pdcha ["e"]), sig_estado=0}):
       (Elem_acc {estado=21,term="coma",accion="reducir",regla=(PIzda "ESM",Pdcha ["e"]), sig_estado=0}):
       (Elem_acc {estado=21,term="par_ce",accion="reducir",regla=(PIzda "ESM",Pdcha ["e"]), sig_estado=0}):
       (Elem_acc {estado=22,term="x",accion="reducir",regla=(PIzda "OBLIG",Pdcha ["x"]), sig_estado=0}):
       (Elem_acc {estado=22,term="e",accion="reducir",regla=(PIzda "OBLIG",Pdcha ["x"]), sig_estado=0}):
       (Elem_acc {estado=22,term="s",accion="reducir",regla=(PIzda "OBLIG",Pdcha ["x"]), sig_estado=0}):
       (Elem_acc {estado=22,term="m",accion="reducir",regla=(PIzda "OBLIG",Pdcha ["x"]), sig_estado=0}):
       (Elem_acc {estado=22,term="c",accion="reducir",regla=(PIzda "OBLIG",Pdcha ["x"]), sig_estado=0}):
       (Elem_acc {estado=22,term="f",accion="reducir",regla=(PIzda "OBLIG",Pdcha ["x"]), sig_estado=0}):
       (Elem_acc {estado=22,term="t",accion="reducir",regla=(PIzda "OBLIG",Pdcha ["x"]), sig_estado=0}):
       (Elem_acc {estado=22,term="lit",accion="reducir",regla=(PIzda "OBLIG",Pdcha ["x"]), sig_estado=0}):
       (Elem_acc {estado=22,term="v-r",accion="reducir",regla=(PIzda "OBLIG",Pdcha ["x"]), sig_estado=0}):
       (Elem_acc {estado=22,term="v-b",accion="reducir",regla=(PIzda "OBLIG",Pdcha ["x"]), sig_estado=0}):
       (Elem_acc {estado=22,term="$",accion="reducir",regla=(PIzda "OBLIG",Pdcha ["x"]), sig_estado=0}):
       (Elem_acc {estado=22,term="coma",accion="reducir",regla=(PIzda "OBLIG",Pdcha ["x"]), sig_estado=0}):
       (Elem_acc {estado=22,term="par_ce",accion="reducir",regla=(PIzda "OBLIG",Pdcha ["x"]), sig_estado=0}):
       (Elem_acc {estado=23,term="x",accion="reducir",regla=(PIzda "VAL",Pdcha ["lit"]), sig_estado=0}):
       (Elem_acc {estado=23,term="e",accion="reducir",regla=(PIzda "VAL",Pdcha ["lit"]), sig_estado=0}):
       (Elem_acc {estado=23,term="s",accion="reducir",regla=(PIzda "VAL",Pdcha ["lit"]), sig_estado=0}):
       (Elem_acc {estado=23,term="m",accion="reducir",regla=(PIzda "VAL",Pdcha ["lit"]), sig_estado=0}):
       (Elem_acc {estado=23,term="c",accion="reducir",regla=(PIzda "VAL",Pdcha ["lit"]), sig_estado=0}):
       (Elem_acc {estado=23,term="f",accion="reducir",regla=(PIzda "VAL",Pdcha ["lit"]), sig_estado=0}):
       (Elem_acc {estado=23,term="t",accion="reducir",regla=(PIzda "VAL",Pdcha ["lit"]), sig_estado=0}):
       (Elem_acc {estado=23,term="lit",accion="reducir",regla=(PIzda "VAL",Pdcha ["lit"]), sig_estado=0}):
       (Elem_acc {estado=23,term="v-r",accion="reducir",regla=(PIzda "VAL",Pdcha ["lit"]), sig_estado=0}):
       (Elem_acc {estado=23,term="v-b",accion="reducir",regla=(PIzda "VAL",Pdcha ["lit"]), sig_estado=0}):
       (Elem_acc {estado=23,term="$",accion="reducir",regla=(PIzda "VAL",Pdcha ["lit"]), sig_estado=0}):
       (Elem_acc {estado=23,term="coma",accion="reducir",regla=(PIzda "VAL",Pdcha ["lit"]), sig_estado=0}):
       (Elem_acc {estado=23,term="par_ce",accion="reducir",regla=(PIzda "VAL",Pdcha ["lit"]), sig_estado=0}):
       (Elem_acc {estado=24,term="par_ce",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=29}):
       (Elem_acc {estado=25,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=26}):
       (Elem_acc {estado=26,term="v-r",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=26,term="v-b",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=26,term="$",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=26,term="coma",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=26,term="par_ce",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=26,term="x",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=22}):
       (Elem_acc {estado=26,term="e",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=21}):
       (Elem_acc {estado=26,term="s",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
       (Elem_acc {estado=26,term="m",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=19}):
       (Elem_acc {estado=26,term="c",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=18}):
       (Elem_acc {estado=26,term="f",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=17}):
       (Elem_acc {estado=26,term="t",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=16}):
       (Elem_acc {estado=26,term="lit",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=23}):
       (Elem_acc {estado=27,term="par_ce",accion="reducir",regla=(PIzda "RCAMPOS",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=27,term="coma",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=25}):
       (Elem_acc {estado=28,term="par_ce",accion="reducir",regla=(PIzda "RCAMPOS",Pdcha ["coma","id","ATTRC","RCAMPOS"]), sig_estado=0}):
       (Elem_acc {estado=29,term="v-r",accion="reducir",regla=(PIzda "CAMPOS",Pdcha ["par_ab","id","ATTRC","RCAMPOS","par_ce"]), sig_estado=0}):
       (Elem_acc {estado=29,term="v-b",accion="reducir",regla=(PIzda "CAMPOS",Pdcha ["par_ab","id","ATTRC","RCAMPOS","par_ce"]), sig_estado=0}):
       (Elem_acc {estado=29,term="coma",accion="reducir",regla=(PIzda "CAMPOS",Pdcha ["par_ab","id","ATTRC","RCAMPOS","par_ce"]), sig_estado=0}):
       (Elem_acc {estado=29,term="$",accion="reducir",regla=(PIzda "CAMPOS",Pdcha ["par_ab","id","ATTRC","RCAMPOS","par_ce"]), sig_estado=0}):
       (Elem_acc {estado=30,term="v-r",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["OBLIG","ATTRC"]), sig_estado=0}):
       (Elem_acc {estado=30,term="v-b",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["OBLIG","ATTRC"]), sig_estado=0}):
       (Elem_acc {estado=30,term="$",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["OBLIG","ATTRC"]), sig_estado=0}):
       (Elem_acc {estado=30,term="coma",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["OBLIG","ATTRC"]), sig_estado=0}):
       (Elem_acc {estado=30,term="par_ce",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["OBLIG","ATTRC"]), sig_estado=0}):
       (Elem_acc {estado=31,term="v-r",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["ESM","ATTRC"]), sig_estado=0}):
       (Elem_acc {estado=31,term="v-b",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["ESM","ATTRC"]), sig_estado=0}):
       (Elem_acc {estado=31,term="$",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["ESM","ATTRC"]), sig_estado=0}):
       (Elem_acc {estado=31,term="coma",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["ESM","ATTRC"]), sig_estado=0}):
       (Elem_acc {estado=31,term="par_ce",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["ESM","ATTRC"]), sig_estado=0}):
       (Elem_acc {estado=32,term="v-r",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["LISTA","ATTRC"]), sig_estado=0}):
       (Elem_acc {estado=32,term="v-b",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["LISTA","ATTRC"]), sig_estado=0}):
       (Elem_acc {estado=32,term="$",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["LISTA","ATTRC"]), sig_estado=0}):
       (Elem_acc {estado=32,term="coma",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["LISTA","ATTRC"]), sig_estado=0}):
       (Elem_acc {estado=32,term="par_ce",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["LISTA","ATTRC"]), sig_estado=0}):
       (Elem_acc {estado=33,term="v-r",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["VAL","ATTRC"]), sig_estado=0}):
       (Elem_acc {estado=33,term="v-b",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["VAL","ATTRC"]), sig_estado=0}):
       (Elem_acc {estado=33,term="$",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["VAL","ATTRC"]), sig_estado=0}):
       (Elem_acc {estado=33,term="coma",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["VAL","ATTRC"]), sig_estado=0}):
       (Elem_acc {estado=33,term="par_ce",accion="reducir",regla=(PIzda "ATTRC",Pdcha ["VAL","ATTRC"]), sig_estado=0}):
       (Elem_acc {estado=34,term="coma",accion="reducir",regla=(PIzda "PROCESO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=34,term="$",accion="reducir",regla=(PIzda "PROCESO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=34,term="v-b",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=38}):
       (Elem_acc {estado=35,term="lit",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=36}):
       (Elem_acc {estado=36,term="v-b",accion="reducir",regla=(PIzda "VALREG",Pdcha ["v-r","lit"]), sig_estado=0}):
       (Elem_acc {estado=36,term="coma",accion="reducir",regla=(PIzda "VALREG",Pdcha ["v-r","lit"]), sig_estado=0}):
       (Elem_acc {estado=36,term="$",accion="reducir",regla=(PIzda "VALREG",Pdcha ["v-r","lit"]), sig_estado=0}):
       (Elem_acc {estado=37,term="$",accion="reducir",regla=(PIzda "RBLOQUES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=37,term="coma",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=41}):
       (Elem_acc {estado=38,term="lit",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=39}):
       (Elem_acc {estado=39,term="coma",accion="reducir",regla=(PIzda "PROCESO",Pdcha ["v-b","lit"]), sig_estado=0}):
       (Elem_acc {estado=39,term="$",accion="reducir",regla=(PIzda "PROCESO",Pdcha ["v-b","lit"]), sig_estado=0}):
       (Elem_acc {estado=40,term="$",accion="reducir",regla=(PIzda "BLOQUES",Pdcha ["id","CAMPOS","VALREG","PROCESO","RBLOQUES"]), sig_estado=0}):
       (Elem_acc {estado=41,term="id",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=42}):
       (Elem_acc {estado=42,term="par_ab",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=9}):
       (Elem_acc {estado=43,term="v-b",accion="reducir",regla=(PIzda "VALREG",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=43,term="coma",accion="reducir",regla=(PIzda "VALREG",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=43,term="$",accion="reducir",regla=(PIzda "VALREG",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=43,term="v-r",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=35}):
       (Elem_acc {estado=44,term="coma",accion="reducir",regla=(PIzda "PROCESO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=44,term="$",accion="reducir",regla=(PIzda "PROCESO",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=44,term="v-b",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=38}):
       (Elem_acc {estado=45,term="$",accion="reducir",regla=(PIzda "RBLOQUES",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=45,term="coma",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=41}):
       (Elem_acc {estado=46,term="$",accion="reducir",regla=(PIzda "RBLOQUES",Pdcha ["coma","id","CAMPOS","VALREG","PROCESO","RBLOQUES"]), sig_estado=0}):
       []
data Elem_ira =  Elem_ira {estado2::Int, var::String, sig_estado2::Int}
tabla_ira::[Elem_ira]
tabla_ira=(Elem_ira {estado2=1,var="FORM",sig_estado2=2}):
       (Elem_ira {estado2=3,var="DESCP",sig_estado2=4}):
       (Elem_ira {estado2=4,var="BLOQUES",sig_estado2=6}):
       (Elem_ira {estado2=7,var="CAMPOS",sig_estado2=8}):
       (Elem_ira {estado2=8,var="VALREG",sig_estado2=34}):
       (Elem_ira {estado2=10,var="ATTRC",sig_estado2=15}):
       (Elem_ira {estado2=10,var="OBLIG",sig_estado2=14}):
       (Elem_ira {estado2=10,var="ESM",sig_estado2=13}):
       (Elem_ira {estado2=10,var="LISTA",sig_estado2=12}):
       (Elem_ira {estado2=10,var="VAL",sig_estado2=11}):
       (Elem_ira {estado2=11,var="ATTRC",sig_estado2=33}):
       (Elem_ira {estado2=11,var="OBLIG",sig_estado2=14}):
       (Elem_ira {estado2=11,var="ESM",sig_estado2=13}):
       (Elem_ira {estado2=11,var="LISTA",sig_estado2=12}):
       (Elem_ira {estado2=11,var="VAL",sig_estado2=11}):
       (Elem_ira {estado2=12,var="ATTRC",sig_estado2=32}):
       (Elem_ira {estado2=12,var="OBLIG",sig_estado2=14}):
       (Elem_ira {estado2=12,var="ESM",sig_estado2=13}):
       (Elem_ira {estado2=12,var="LISTA",sig_estado2=12}):
       (Elem_ira {estado2=12,var="VAL",sig_estado2=11}):
       (Elem_ira {estado2=13,var="ATTRC",sig_estado2=31}):
       (Elem_ira {estado2=13,var="OBLIG",sig_estado2=14}):
       (Elem_ira {estado2=13,var="ESM",sig_estado2=13}):
       (Elem_ira {estado2=13,var="LISTA",sig_estado2=12}):
       (Elem_ira {estado2=13,var="VAL",sig_estado2=11}):
       (Elem_ira {estado2=14,var="ATTRC",sig_estado2=30}):
       (Elem_ira {estado2=14,var="OBLIG",sig_estado2=14}):
       (Elem_ira {estado2=14,var="ESM",sig_estado2=13}):
       (Elem_ira {estado2=14,var="LISTA",sig_estado2=12}):
       (Elem_ira {estado2=14,var="VAL",sig_estado2=11}):
       (Elem_ira {estado2=15,var="RCAMPOS",sig_estado2=24}):
       (Elem_ira {estado2=26,var="ATTRC",sig_estado2=27}):
       (Elem_ira {estado2=26,var="OBLIG",sig_estado2=14}):
       (Elem_ira {estado2=26,var="ESM",sig_estado2=13}):
       (Elem_ira {estado2=26,var="LISTA",sig_estado2=12}):
       (Elem_ira {estado2=26,var="VAL",sig_estado2=11}):
       (Elem_ira {estado2=27,var="RCAMPOS",sig_estado2=28}):
       (Elem_ira {estado2=34,var="PROCESO",sig_estado2=37}):
       (Elem_ira {estado2=37,var="RBLOQUES",sig_estado2=40}):
       (Elem_ira {estado2=42,var="CAMPOS",sig_estado2=43}):
       (Elem_ira {estado2=43,var="VALREG",sig_estado2=44}):
       (Elem_ira {estado2=44,var="PROCESO",sig_estado2=45}):
       (Elem_ira {estado2=45,var="RBLOQUES",sig_estado2=46}):
       []
axioma::String
axioma="PFORM"
terminales::[String]
terminales = ["id","lit","coma","par_ab","par_ce","x","e","s","m","c","f","t","v-r","v-b"]
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
