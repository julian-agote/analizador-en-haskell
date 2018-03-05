module Parser_jkl3(parser_slr,parser_slr_arbol) where
import Utilidades
import Arbol
import Scaner_jkl3
--------------- Gramatica: -------------------------------------------------
-- prog
-- P_PROG ID P_COMA PUNTO PVAR PBEGIN PEND ASIGN PREAD PWRITE PWRITC PWRITL PIF PTHEN P_ELSE PWHILE PDO PFORP PTODO OP_OR OP_AND OP_REL OP_ADIT OP_MULT OP_NOT NUM CAD PAR_ABR PAR_CER 
-- prog decl lsent sent sentc pelse expr exprp eand eandp erel erelp arit aritp term termp fact rando 
--prog -> P_PROG ID P_COMA decl sentc PUNTO
--decl -> lambda
--decl -> PVAR ID P_COMA decl
--sentc -> PBEGIN lsent PEND
--lsent -> lambda
--lsent -> sent lsent
--sent -> P_COMA
--sent -> ID ASIGN expr P_COMA
--sent -> sentc
--sent -> PREAD ID P_COMA
--sent -> PWRITE expr P_COMA
--sent -> PWRITC CAD P_COMA
--sent -> PWRITL P_COMA
--sent -> PIF expr PTHEN sent pelse
--sent -> PWHILE expr PDO sent
--sent -> PDO sent PWHILE expr P_COMA
--sent -> PFORP ID ASIGN expr PTODO expr PDO sent
--pelse -> lambda
--pelse -> P_ELSE sent
--expr -> eand exprp
--exprp -> lambda
--exprp -> OP_OR eand exprp
--eand -> erel eandp
--eandp -> lambda
--eandp -> OP_AND erel eandp
--erel -> arit erelp
--erelp -> lambda
--erelp -> OP_REL arit erelp
--arit -> term aritp
--aritp -> lambda
--aritp -> OP_ADIT term aritp
--term -> fact termp
--termp -> lambda
--termp -> OP_MULT fact termp
--fact -> OP_NOT fact
--fact -> OP_ADIT fact
--fact -> rando
--rando -> NUM
--rando -> ID
--rando -> PAR_ABR expr PAR_CER
--Pprog -> prog
----------------------------------------------------------------------------
data Partes = PIzda String|Pdcha [String]
type Regla=(Partes,Partes)
data Elem_acc =  Elem_acc {estado::Int, term::String, accion::String, regla::Regla, sig_estado::Int}
tabla_acc::[Elem_acc]
tabla_acc=(Elem_acc {estado=1,term="P_PROG",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=3}):
       (Elem_acc {estado=2,term="$",accion="aceptar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=3,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=4}):
       (Elem_acc {estado=4,term="P_COMA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=5}):
       (Elem_acc {estado=5,term="PBEGIN",accion="reducir",regla=(PIzda "decl",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=5,term="PVAR",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=7}):
       (Elem_acc {estado=6,term="PBEGIN",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=12}):
       (Elem_acc {estado=7,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=8,term="P_COMA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=9}):
       (Elem_acc {estado=9,term="PBEGIN",accion="reducir",regla=(PIzda "decl",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=9,term="PVAR",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=7}):
       (Elem_acc {estado=10,term="PBEGIN",accion="reducir",regla=(PIzda "decl",Pdcha ["PVAR","ID","P_COMA","decl"]), sig_estado=0}):
       (Elem_acc {estado=11,term="PUNTO",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=93}):
       (Elem_acc {estado=12,term="PEND",accion="reducir",regla=(PIzda "lsent",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=12,term="P_COMA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=24}):
       (Elem_acc {estado=12,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=25}):
       (Elem_acc {estado=12,term="PBEGIN",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=12}):
       (Elem_acc {estado=12,term="PREAD",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=23}):
       (Elem_acc {estado=12,term="PWRITE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=22}):
       (Elem_acc {estado=12,term="PWRITC",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=21}):
       (Elem_acc {estado=12,term="PWRITL",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
       (Elem_acc {estado=12,term="PIF",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=19}):
       (Elem_acc {estado=12,term="PWHILE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=18}):
       (Elem_acc {estado=12,term="PDO",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=17}):
       (Elem_acc {estado=12,term="PFORP",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=16}):
       (Elem_acc {estado=13,term="P_COMA",accion="reducir",regla=(PIzda "sent",Pdcha ["sentc"]), sig_estado=0}):
       (Elem_acc {estado=13,term="ID",accion="reducir",regla=(PIzda "sent",Pdcha ["sentc"]), sig_estado=0}):
       (Elem_acc {estado=13,term="PBEGIN",accion="reducir",regla=(PIzda "sent",Pdcha ["sentc"]), sig_estado=0}):
       (Elem_acc {estado=13,term="PREAD",accion="reducir",regla=(PIzda "sent",Pdcha ["sentc"]), sig_estado=0}):
       (Elem_acc {estado=13,term="PWRITE",accion="reducir",regla=(PIzda "sent",Pdcha ["sentc"]), sig_estado=0}):
       (Elem_acc {estado=13,term="PWRITC",accion="reducir",regla=(PIzda "sent",Pdcha ["sentc"]), sig_estado=0}):
       (Elem_acc {estado=13,term="PWRITL",accion="reducir",regla=(PIzda "sent",Pdcha ["sentc"]), sig_estado=0}):
       (Elem_acc {estado=13,term="PIF",accion="reducir",regla=(PIzda "sent",Pdcha ["sentc"]), sig_estado=0}):
       (Elem_acc {estado=13,term="PDO",accion="reducir",regla=(PIzda "sent",Pdcha ["sentc"]), sig_estado=0}):
       (Elem_acc {estado=13,term="PFORP",accion="reducir",regla=(PIzda "sent",Pdcha ["sentc"]), sig_estado=0}):
       (Elem_acc {estado=13,term="PEND",accion="reducir",regla=(PIzda "sent",Pdcha ["sentc"]), sig_estado=0}):
       (Elem_acc {estado=13,term="P_ELSE",accion="reducir",regla=(PIzda "sent",Pdcha ["sentc"]), sig_estado=0}):
       (Elem_acc {estado=13,term="PWHILE",accion="reducir",regla=(PIzda "sent",Pdcha ["sentc"]), sig_estado=0}):
       (Elem_acc {estado=14,term="PEND",accion="reducir",regla=(PIzda "lsent",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=14,term="P_COMA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=24}):
       (Elem_acc {estado=14,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=25}):
       (Elem_acc {estado=14,term="PBEGIN",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=12}):
       (Elem_acc {estado=14,term="PREAD",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=23}):
       (Elem_acc {estado=14,term="PWRITE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=22}):
       (Elem_acc {estado=14,term="PWRITC",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=21}):
       (Elem_acc {estado=14,term="PWRITL",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
       (Elem_acc {estado=14,term="PIF",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=19}):
       (Elem_acc {estado=14,term="PWHILE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=18}):
       (Elem_acc {estado=14,term="PDO",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=17}):
       (Elem_acc {estado=14,term="PFORP",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=16}):
       (Elem_acc {estado=15,term="PEND",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=91}):
       (Elem_acc {estado=16,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=84}):
       (Elem_acc {estado=17,term="P_COMA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=24}):
       (Elem_acc {estado=17,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=25}):
       (Elem_acc {estado=17,term="PBEGIN",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=12}):
       (Elem_acc {estado=17,term="PREAD",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=23}):
       (Elem_acc {estado=17,term="PWRITE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=22}):
       (Elem_acc {estado=17,term="PWRITC",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=21}):
       (Elem_acc {estado=17,term="PWRITL",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
       (Elem_acc {estado=17,term="PIF",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=19}):
       (Elem_acc {estado=17,term="PWHILE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=18}):
       (Elem_acc {estado=17,term="PDO",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=17}):
       (Elem_acc {estado=17,term="PFORP",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=16}):
       (Elem_acc {estado=18,term="OP_NOT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=36}):
       (Elem_acc {estado=18,term="OP_ADIT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=37}):
       (Elem_acc {estado=18,term="NUM",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=35}):
       (Elem_acc {estado=18,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=38}):
       (Elem_acc {estado=18,term="PAR_ABR",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=34}):
       (Elem_acc {estado=19,term="OP_NOT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=36}):
       (Elem_acc {estado=19,term="OP_ADIT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=37}):
       (Elem_acc {estado=19,term="NUM",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=35}):
       (Elem_acc {estado=19,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=38}):
       (Elem_acc {estado=19,term="PAR_ABR",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=34}):
       (Elem_acc {estado=20,term="P_COMA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=70}):
       (Elem_acc {estado=21,term="CAD",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=68}):
       (Elem_acc {estado=22,term="OP_NOT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=36}):
       (Elem_acc {estado=22,term="OP_ADIT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=37}):
       (Elem_acc {estado=22,term="NUM",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=35}):
       (Elem_acc {estado=22,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=38}):
       (Elem_acc {estado=22,term="PAR_ABR",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=34}):
       (Elem_acc {estado=23,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=64}):
       (Elem_acc {estado=24,term="P_COMA",accion="reducir",regla=(PIzda "sent",Pdcha ["P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=24,term="ID",accion="reducir",regla=(PIzda "sent",Pdcha ["P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=24,term="PBEGIN",accion="reducir",regla=(PIzda "sent",Pdcha ["P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=24,term="PREAD",accion="reducir",regla=(PIzda "sent",Pdcha ["P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=24,term="PWRITE",accion="reducir",regla=(PIzda "sent",Pdcha ["P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=24,term="PWRITC",accion="reducir",regla=(PIzda "sent",Pdcha ["P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=24,term="PWRITL",accion="reducir",regla=(PIzda "sent",Pdcha ["P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=24,term="PIF",accion="reducir",regla=(PIzda "sent",Pdcha ["P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=24,term="PDO",accion="reducir",regla=(PIzda "sent",Pdcha ["P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=24,term="PFORP",accion="reducir",regla=(PIzda "sent",Pdcha ["P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=24,term="PEND",accion="reducir",regla=(PIzda "sent",Pdcha ["P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=24,term="P_ELSE",accion="reducir",regla=(PIzda "sent",Pdcha ["P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=24,term="PWHILE",accion="reducir",regla=(PIzda "sent",Pdcha ["P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=25,term="ASIGN",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=26}):
       (Elem_acc {estado=26,term="OP_NOT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=36}):
       (Elem_acc {estado=26,term="OP_ADIT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=37}):
       (Elem_acc {estado=26,term="NUM",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=35}):
       (Elem_acc {estado=26,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=38}):
       (Elem_acc {estado=26,term="PAR_ABR",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=34}):
       (Elem_acc {estado=27,term="OP_MULT",accion="reducir",regla=(PIzda "fact",Pdcha ["rando"]), sig_estado=0}):
       (Elem_acc {estado=27,term="OP_ADIT",accion="reducir",regla=(PIzda "fact",Pdcha ["rando"]), sig_estado=0}):
       (Elem_acc {estado=27,term="OP_REL",accion="reducir",regla=(PIzda "fact",Pdcha ["rando"]), sig_estado=0}):
       (Elem_acc {estado=27,term="OP_AND",accion="reducir",regla=(PIzda "fact",Pdcha ["rando"]), sig_estado=0}):
       (Elem_acc {estado=27,term="OP_OR",accion="reducir",regla=(PIzda "fact",Pdcha ["rando"]), sig_estado=0}):
       (Elem_acc {estado=27,term="PTHEN",accion="reducir",regla=(PIzda "fact",Pdcha ["rando"]), sig_estado=0}):
       (Elem_acc {estado=27,term="P_COMA",accion="reducir",regla=(PIzda "fact",Pdcha ["rando"]), sig_estado=0}):
       (Elem_acc {estado=27,term="PTODO",accion="reducir",regla=(PIzda "fact",Pdcha ["rando"]), sig_estado=0}):
       (Elem_acc {estado=27,term="PDO",accion="reducir",regla=(PIzda "fact",Pdcha ["rando"]), sig_estado=0}):
       (Elem_acc {estado=27,term="PAR_CER",accion="reducir",regla=(PIzda "fact",Pdcha ["rando"]), sig_estado=0}):
       (Elem_acc {estado=28,term="OP_ADIT",accion="reducir",regla=(PIzda "termp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=28,term="OP_REL",accion="reducir",regla=(PIzda "termp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=28,term="OP_AND",accion="reducir",regla=(PIzda "termp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=28,term="OP_OR",accion="reducir",regla=(PIzda "termp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=28,term="PTHEN",accion="reducir",regla=(PIzda "termp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=28,term="P_COMA",accion="reducir",regla=(PIzda "termp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=28,term="PTODO",accion="reducir",regla=(PIzda "termp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=28,term="PDO",accion="reducir",regla=(PIzda "termp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=28,term="PAR_CER",accion="reducir",regla=(PIzda "termp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=28,term="OP_MULT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=61}):
       (Elem_acc {estado=29,term="OP_REL",accion="reducir",regla=(PIzda "aritp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=29,term="OP_AND",accion="reducir",regla=(PIzda "aritp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=29,term="OP_OR",accion="reducir",regla=(PIzda "aritp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=29,term="PTHEN",accion="reducir",regla=(PIzda "aritp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=29,term="P_COMA",accion="reducir",regla=(PIzda "aritp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=29,term="PTODO",accion="reducir",regla=(PIzda "aritp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=29,term="PDO",accion="reducir",regla=(PIzda "aritp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=29,term="PAR_CER",accion="reducir",regla=(PIzda "aritp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=29,term="OP_ADIT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=57}):
       (Elem_acc {estado=30,term="OP_AND",accion="reducir",regla=(PIzda "erelp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=30,term="OP_OR",accion="reducir",regla=(PIzda "erelp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=30,term="PTHEN",accion="reducir",regla=(PIzda "erelp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=30,term="P_COMA",accion="reducir",regla=(PIzda "erelp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=30,term="PTODO",accion="reducir",regla=(PIzda "erelp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=30,term="PDO",accion="reducir",regla=(PIzda "erelp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=30,term="PAR_CER",accion="reducir",regla=(PIzda "erelp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=30,term="OP_REL",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=53}):
       (Elem_acc {estado=31,term="OP_OR",accion="reducir",regla=(PIzda "eandp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=31,term="PTHEN",accion="reducir",regla=(PIzda "eandp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=31,term="P_COMA",accion="reducir",regla=(PIzda "eandp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=31,term="PTODO",accion="reducir",regla=(PIzda "eandp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=31,term="PDO",accion="reducir",regla=(PIzda "eandp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=31,term="PAR_CER",accion="reducir",regla=(PIzda "eandp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=31,term="OP_AND",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=49}):
       (Elem_acc {estado=32,term="PTHEN",accion="reducir",regla=(PIzda "exprp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=32,term="P_COMA",accion="reducir",regla=(PIzda "exprp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=32,term="PTODO",accion="reducir",regla=(PIzda "exprp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=32,term="PDO",accion="reducir",regla=(PIzda "exprp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=32,term="PAR_CER",accion="reducir",regla=(PIzda "exprp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=32,term="OP_OR",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=45}):
       (Elem_acc {estado=33,term="P_COMA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=43}):
       (Elem_acc {estado=34,term="OP_NOT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=36}):
       (Elem_acc {estado=34,term="OP_ADIT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=37}):
       (Elem_acc {estado=34,term="NUM",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=35}):
       (Elem_acc {estado=34,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=38}):
       (Elem_acc {estado=34,term="PAR_ABR",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=34}):
       (Elem_acc {estado=35,term="OP_MULT",accion="reducir",regla=(PIzda "rando",Pdcha ["NUM"]), sig_estado=0}):
       (Elem_acc {estado=35,term="OP_ADIT",accion="reducir",regla=(PIzda "rando",Pdcha ["NUM"]), sig_estado=0}):
       (Elem_acc {estado=35,term="OP_REL",accion="reducir",regla=(PIzda "rando",Pdcha ["NUM"]), sig_estado=0}):
       (Elem_acc {estado=35,term="OP_AND",accion="reducir",regla=(PIzda "rando",Pdcha ["NUM"]), sig_estado=0}):
       (Elem_acc {estado=35,term="OP_OR",accion="reducir",regla=(PIzda "rando",Pdcha ["NUM"]), sig_estado=0}):
       (Elem_acc {estado=35,term="PTHEN",accion="reducir",regla=(PIzda "rando",Pdcha ["NUM"]), sig_estado=0}):
       (Elem_acc {estado=35,term="P_COMA",accion="reducir",regla=(PIzda "rando",Pdcha ["NUM"]), sig_estado=0}):
       (Elem_acc {estado=35,term="PTODO",accion="reducir",regla=(PIzda "rando",Pdcha ["NUM"]), sig_estado=0}):
       (Elem_acc {estado=35,term="PDO",accion="reducir",regla=(PIzda "rando",Pdcha ["NUM"]), sig_estado=0}):
       (Elem_acc {estado=35,term="PAR_CER",accion="reducir",regla=(PIzda "rando",Pdcha ["NUM"]), sig_estado=0}):
       (Elem_acc {estado=36,term="OP_NOT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=36}):
       (Elem_acc {estado=36,term="OP_ADIT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=37}):
       (Elem_acc {estado=36,term="NUM",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=35}):
       (Elem_acc {estado=36,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=38}):
       (Elem_acc {estado=36,term="PAR_ABR",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=34}):
       (Elem_acc {estado=37,term="OP_NOT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=36}):
       (Elem_acc {estado=37,term="OP_ADIT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=37}):
       (Elem_acc {estado=37,term="NUM",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=35}):
       (Elem_acc {estado=37,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=38}):
       (Elem_acc {estado=37,term="PAR_ABR",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=34}):
       (Elem_acc {estado=38,term="OP_MULT",accion="reducir",regla=(PIzda "rando",Pdcha ["ID"]), sig_estado=0}):
       (Elem_acc {estado=38,term="OP_ADIT",accion="reducir",regla=(PIzda "rando",Pdcha ["ID"]), sig_estado=0}):
       (Elem_acc {estado=38,term="OP_REL",accion="reducir",regla=(PIzda "rando",Pdcha ["ID"]), sig_estado=0}):
       (Elem_acc {estado=38,term="OP_AND",accion="reducir",regla=(PIzda "rando",Pdcha ["ID"]), sig_estado=0}):
       (Elem_acc {estado=38,term="OP_OR",accion="reducir",regla=(PIzda "rando",Pdcha ["ID"]), sig_estado=0}):
       (Elem_acc {estado=38,term="PTHEN",accion="reducir",regla=(PIzda "rando",Pdcha ["ID"]), sig_estado=0}):
       (Elem_acc {estado=38,term="P_COMA",accion="reducir",regla=(PIzda "rando",Pdcha ["ID"]), sig_estado=0}):
       (Elem_acc {estado=38,term="PTODO",accion="reducir",regla=(PIzda "rando",Pdcha ["ID"]), sig_estado=0}):
       (Elem_acc {estado=38,term="PDO",accion="reducir",regla=(PIzda "rando",Pdcha ["ID"]), sig_estado=0}):
       (Elem_acc {estado=38,term="PAR_CER",accion="reducir",regla=(PIzda "rando",Pdcha ["ID"]), sig_estado=0}):
       (Elem_acc {estado=39,term="OP_MULT",accion="reducir",regla=(PIzda "fact",Pdcha ["OP_ADIT","fact"]), sig_estado=0}):
       (Elem_acc {estado=39,term="OP_ADIT",accion="reducir",regla=(PIzda "fact",Pdcha ["OP_ADIT","fact"]), sig_estado=0}):
       (Elem_acc {estado=39,term="OP_REL",accion="reducir",regla=(PIzda "fact",Pdcha ["OP_ADIT","fact"]), sig_estado=0}):
       (Elem_acc {estado=39,term="OP_AND",accion="reducir",regla=(PIzda "fact",Pdcha ["OP_ADIT","fact"]), sig_estado=0}):
       (Elem_acc {estado=39,term="OP_OR",accion="reducir",regla=(PIzda "fact",Pdcha ["OP_ADIT","fact"]), sig_estado=0}):
       (Elem_acc {estado=39,term="PTHEN",accion="reducir",regla=(PIzda "fact",Pdcha ["OP_ADIT","fact"]), sig_estado=0}):
       (Elem_acc {estado=39,term="P_COMA",accion="reducir",regla=(PIzda "fact",Pdcha ["OP_ADIT","fact"]), sig_estado=0}):
       (Elem_acc {estado=39,term="PTODO",accion="reducir",regla=(PIzda "fact",Pdcha ["OP_ADIT","fact"]), sig_estado=0}):
       (Elem_acc {estado=39,term="PDO",accion="reducir",regla=(PIzda "fact",Pdcha ["OP_ADIT","fact"]), sig_estado=0}):
       (Elem_acc {estado=39,term="PAR_CER",accion="reducir",regla=(PIzda "fact",Pdcha ["OP_ADIT","fact"]), sig_estado=0}):
       (Elem_acc {estado=40,term="OP_MULT",accion="reducir",regla=(PIzda "fact",Pdcha ["OP_NOT","fact"]), sig_estado=0}):
       (Elem_acc {estado=40,term="OP_ADIT",accion="reducir",regla=(PIzda "fact",Pdcha ["OP_NOT","fact"]), sig_estado=0}):
       (Elem_acc {estado=40,term="OP_REL",accion="reducir",regla=(PIzda "fact",Pdcha ["OP_NOT","fact"]), sig_estado=0}):
       (Elem_acc {estado=40,term="OP_AND",accion="reducir",regla=(PIzda "fact",Pdcha ["OP_NOT","fact"]), sig_estado=0}):
       (Elem_acc {estado=40,term="OP_OR",accion="reducir",regla=(PIzda "fact",Pdcha ["OP_NOT","fact"]), sig_estado=0}):
       (Elem_acc {estado=40,term="PTHEN",accion="reducir",regla=(PIzda "fact",Pdcha ["OP_NOT","fact"]), sig_estado=0}):
       (Elem_acc {estado=40,term="P_COMA",accion="reducir",regla=(PIzda "fact",Pdcha ["OP_NOT","fact"]), sig_estado=0}):
       (Elem_acc {estado=40,term="PTODO",accion="reducir",regla=(PIzda "fact",Pdcha ["OP_NOT","fact"]), sig_estado=0}):
       (Elem_acc {estado=40,term="PDO",accion="reducir",regla=(PIzda "fact",Pdcha ["OP_NOT","fact"]), sig_estado=0}):
       (Elem_acc {estado=40,term="PAR_CER",accion="reducir",regla=(PIzda "fact",Pdcha ["OP_NOT","fact"]), sig_estado=0}):
       (Elem_acc {estado=41,term="PAR_CER",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=42}):
       (Elem_acc {estado=42,term="OP_MULT",accion="reducir",regla=(PIzda "rando",Pdcha ["PAR_ABR","expr","PAR_CER"]), sig_estado=0}):
       (Elem_acc {estado=42,term="OP_ADIT",accion="reducir",regla=(PIzda "rando",Pdcha ["PAR_ABR","expr","PAR_CER"]), sig_estado=0}):
       (Elem_acc {estado=42,term="OP_REL",accion="reducir",regla=(PIzda "rando",Pdcha ["PAR_ABR","expr","PAR_CER"]), sig_estado=0}):
       (Elem_acc {estado=42,term="OP_AND",accion="reducir",regla=(PIzda "rando",Pdcha ["PAR_ABR","expr","PAR_CER"]), sig_estado=0}):
       (Elem_acc {estado=42,term="OP_OR",accion="reducir",regla=(PIzda "rando",Pdcha ["PAR_ABR","expr","PAR_CER"]), sig_estado=0}):
       (Elem_acc {estado=42,term="PTHEN",accion="reducir",regla=(PIzda "rando",Pdcha ["PAR_ABR","expr","PAR_CER"]), sig_estado=0}):
       (Elem_acc {estado=42,term="P_COMA",accion="reducir",regla=(PIzda "rando",Pdcha ["PAR_ABR","expr","PAR_CER"]), sig_estado=0}):
       (Elem_acc {estado=42,term="PTODO",accion="reducir",regla=(PIzda "rando",Pdcha ["PAR_ABR","expr","PAR_CER"]), sig_estado=0}):
       (Elem_acc {estado=42,term="PDO",accion="reducir",regla=(PIzda "rando",Pdcha ["PAR_ABR","expr","PAR_CER"]), sig_estado=0}):
       (Elem_acc {estado=42,term="PAR_CER",accion="reducir",regla=(PIzda "rando",Pdcha ["PAR_ABR","expr","PAR_CER"]), sig_estado=0}):
       (Elem_acc {estado=43,term="P_COMA",accion="reducir",regla=(PIzda "sent",Pdcha ["ID","ASIGN","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=43,term="ID",accion="reducir",regla=(PIzda "sent",Pdcha ["ID","ASIGN","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=43,term="PBEGIN",accion="reducir",regla=(PIzda "sent",Pdcha ["ID","ASIGN","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=43,term="PREAD",accion="reducir",regla=(PIzda "sent",Pdcha ["ID","ASIGN","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=43,term="PWRITE",accion="reducir",regla=(PIzda "sent",Pdcha ["ID","ASIGN","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=43,term="PWRITC",accion="reducir",regla=(PIzda "sent",Pdcha ["ID","ASIGN","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=43,term="PWRITL",accion="reducir",regla=(PIzda "sent",Pdcha ["ID","ASIGN","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=43,term="PIF",accion="reducir",regla=(PIzda "sent",Pdcha ["ID","ASIGN","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=43,term="PDO",accion="reducir",regla=(PIzda "sent",Pdcha ["ID","ASIGN","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=43,term="PFORP",accion="reducir",regla=(PIzda "sent",Pdcha ["ID","ASIGN","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=43,term="PEND",accion="reducir",regla=(PIzda "sent",Pdcha ["ID","ASIGN","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=43,term="P_ELSE",accion="reducir",regla=(PIzda "sent",Pdcha ["ID","ASIGN","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=43,term="PWHILE",accion="reducir",regla=(PIzda "sent",Pdcha ["ID","ASIGN","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=44,term="PTHEN",accion="reducir",regla=(PIzda "expr",Pdcha ["eand","exprp"]), sig_estado=0}):
       (Elem_acc {estado=44,term="P_COMA",accion="reducir",regla=(PIzda "expr",Pdcha ["eand","exprp"]), sig_estado=0}):
       (Elem_acc {estado=44,term="PTODO",accion="reducir",regla=(PIzda "expr",Pdcha ["eand","exprp"]), sig_estado=0}):
       (Elem_acc {estado=44,term="PDO",accion="reducir",regla=(PIzda "expr",Pdcha ["eand","exprp"]), sig_estado=0}):
       (Elem_acc {estado=44,term="PAR_CER",accion="reducir",regla=(PIzda "expr",Pdcha ["eand","exprp"]), sig_estado=0}):
       (Elem_acc {estado=45,term="OP_NOT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=36}):
       (Elem_acc {estado=45,term="OP_ADIT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=37}):
       (Elem_acc {estado=45,term="NUM",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=35}):
       (Elem_acc {estado=45,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=38}):
       (Elem_acc {estado=45,term="PAR_ABR",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=34}):
       (Elem_acc {estado=46,term="PTHEN",accion="reducir",regla=(PIzda "exprp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=46,term="P_COMA",accion="reducir",regla=(PIzda "exprp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=46,term="PTODO",accion="reducir",regla=(PIzda "exprp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=46,term="PDO",accion="reducir",regla=(PIzda "exprp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=46,term="PAR_CER",accion="reducir",regla=(PIzda "exprp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=46,term="OP_OR",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=45}):
       (Elem_acc {estado=47,term="PTHEN",accion="reducir",regla=(PIzda "exprp",Pdcha ["OP_OR","eand","exprp"]), sig_estado=0}):
       (Elem_acc {estado=47,term="P_COMA",accion="reducir",regla=(PIzda "exprp",Pdcha ["OP_OR","eand","exprp"]), sig_estado=0}):
       (Elem_acc {estado=47,term="PTODO",accion="reducir",regla=(PIzda "exprp",Pdcha ["OP_OR","eand","exprp"]), sig_estado=0}):
       (Elem_acc {estado=47,term="PDO",accion="reducir",regla=(PIzda "exprp",Pdcha ["OP_OR","eand","exprp"]), sig_estado=0}):
       (Elem_acc {estado=47,term="PAR_CER",accion="reducir",regla=(PIzda "exprp",Pdcha ["OP_OR","eand","exprp"]), sig_estado=0}):
       (Elem_acc {estado=48,term="OP_OR",accion="reducir",regla=(PIzda "eand",Pdcha ["erel","eandp"]), sig_estado=0}):
       (Elem_acc {estado=48,term="PTHEN",accion="reducir",regla=(PIzda "eand",Pdcha ["erel","eandp"]), sig_estado=0}):
       (Elem_acc {estado=48,term="P_COMA",accion="reducir",regla=(PIzda "eand",Pdcha ["erel","eandp"]), sig_estado=0}):
       (Elem_acc {estado=48,term="PTODO",accion="reducir",regla=(PIzda "eand",Pdcha ["erel","eandp"]), sig_estado=0}):
       (Elem_acc {estado=48,term="PDO",accion="reducir",regla=(PIzda "eand",Pdcha ["erel","eandp"]), sig_estado=0}):
       (Elem_acc {estado=48,term="PAR_CER",accion="reducir",regla=(PIzda "eand",Pdcha ["erel","eandp"]), sig_estado=0}):
       (Elem_acc {estado=49,term="OP_NOT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=36}):
       (Elem_acc {estado=49,term="OP_ADIT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=37}):
       (Elem_acc {estado=49,term="NUM",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=35}):
       (Elem_acc {estado=49,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=38}):
       (Elem_acc {estado=49,term="PAR_ABR",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=34}):
       (Elem_acc {estado=50,term="OP_OR",accion="reducir",regla=(PIzda "eandp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=50,term="PTHEN",accion="reducir",regla=(PIzda "eandp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=50,term="P_COMA",accion="reducir",regla=(PIzda "eandp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=50,term="PTODO",accion="reducir",regla=(PIzda "eandp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=50,term="PDO",accion="reducir",regla=(PIzda "eandp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=50,term="PAR_CER",accion="reducir",regla=(PIzda "eandp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=50,term="OP_AND",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=49}):
       (Elem_acc {estado=51,term="OP_OR",accion="reducir",regla=(PIzda "eandp",Pdcha ["OP_AND","erel","eandp"]), sig_estado=0}):
       (Elem_acc {estado=51,term="PTHEN",accion="reducir",regla=(PIzda "eandp",Pdcha ["OP_AND","erel","eandp"]), sig_estado=0}):
       (Elem_acc {estado=51,term="P_COMA",accion="reducir",regla=(PIzda "eandp",Pdcha ["OP_AND","erel","eandp"]), sig_estado=0}):
       (Elem_acc {estado=51,term="PTODO",accion="reducir",regla=(PIzda "eandp",Pdcha ["OP_AND","erel","eandp"]), sig_estado=0}):
       (Elem_acc {estado=51,term="PDO",accion="reducir",regla=(PIzda "eandp",Pdcha ["OP_AND","erel","eandp"]), sig_estado=0}):
       (Elem_acc {estado=51,term="PAR_CER",accion="reducir",regla=(PIzda "eandp",Pdcha ["OP_AND","erel","eandp"]), sig_estado=0}):
       (Elem_acc {estado=52,term="OP_AND",accion="reducir",regla=(PIzda "erel",Pdcha ["arit","erelp"]), sig_estado=0}):
       (Elem_acc {estado=52,term="OP_OR",accion="reducir",regla=(PIzda "erel",Pdcha ["arit","erelp"]), sig_estado=0}):
       (Elem_acc {estado=52,term="PTHEN",accion="reducir",regla=(PIzda "erel",Pdcha ["arit","erelp"]), sig_estado=0}):
       (Elem_acc {estado=52,term="P_COMA",accion="reducir",regla=(PIzda "erel",Pdcha ["arit","erelp"]), sig_estado=0}):
       (Elem_acc {estado=52,term="PTODO",accion="reducir",regla=(PIzda "erel",Pdcha ["arit","erelp"]), sig_estado=0}):
       (Elem_acc {estado=52,term="PDO",accion="reducir",regla=(PIzda "erel",Pdcha ["arit","erelp"]), sig_estado=0}):
       (Elem_acc {estado=52,term="PAR_CER",accion="reducir",regla=(PIzda "erel",Pdcha ["arit","erelp"]), sig_estado=0}):
       (Elem_acc {estado=53,term="OP_NOT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=36}):
       (Elem_acc {estado=53,term="OP_ADIT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=37}):
       (Elem_acc {estado=53,term="NUM",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=35}):
       (Elem_acc {estado=53,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=38}):
       (Elem_acc {estado=53,term="PAR_ABR",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=34}):
       (Elem_acc {estado=54,term="OP_AND",accion="reducir",regla=(PIzda "erelp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=54,term="OP_OR",accion="reducir",regla=(PIzda "erelp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=54,term="PTHEN",accion="reducir",regla=(PIzda "erelp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=54,term="P_COMA",accion="reducir",regla=(PIzda "erelp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=54,term="PTODO",accion="reducir",regla=(PIzda "erelp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=54,term="PDO",accion="reducir",regla=(PIzda "erelp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=54,term="PAR_CER",accion="reducir",regla=(PIzda "erelp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=54,term="OP_REL",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=53}):
       (Elem_acc {estado=55,term="OP_AND",accion="reducir",regla=(PIzda "erelp",Pdcha ["OP_REL","arit","erelp"]), sig_estado=0}):
       (Elem_acc {estado=55,term="OP_OR",accion="reducir",regla=(PIzda "erelp",Pdcha ["OP_REL","arit","erelp"]), sig_estado=0}):
       (Elem_acc {estado=55,term="PTHEN",accion="reducir",regla=(PIzda "erelp",Pdcha ["OP_REL","arit","erelp"]), sig_estado=0}):
       (Elem_acc {estado=55,term="P_COMA",accion="reducir",regla=(PIzda "erelp",Pdcha ["OP_REL","arit","erelp"]), sig_estado=0}):
       (Elem_acc {estado=55,term="PTODO",accion="reducir",regla=(PIzda "erelp",Pdcha ["OP_REL","arit","erelp"]), sig_estado=0}):
       (Elem_acc {estado=55,term="PDO",accion="reducir",regla=(PIzda "erelp",Pdcha ["OP_REL","arit","erelp"]), sig_estado=0}):
       (Elem_acc {estado=55,term="PAR_CER",accion="reducir",regla=(PIzda "erelp",Pdcha ["OP_REL","arit","erelp"]), sig_estado=0}):
       (Elem_acc {estado=56,term="OP_REL",accion="reducir",regla=(PIzda "arit",Pdcha ["term","aritp"]), sig_estado=0}):
       (Elem_acc {estado=56,term="OP_AND",accion="reducir",regla=(PIzda "arit",Pdcha ["term","aritp"]), sig_estado=0}):
       (Elem_acc {estado=56,term="OP_OR",accion="reducir",regla=(PIzda "arit",Pdcha ["term","aritp"]), sig_estado=0}):
       (Elem_acc {estado=56,term="PTHEN",accion="reducir",regla=(PIzda "arit",Pdcha ["term","aritp"]), sig_estado=0}):
       (Elem_acc {estado=56,term="P_COMA",accion="reducir",regla=(PIzda "arit",Pdcha ["term","aritp"]), sig_estado=0}):
       (Elem_acc {estado=56,term="PTODO",accion="reducir",regla=(PIzda "arit",Pdcha ["term","aritp"]), sig_estado=0}):
       (Elem_acc {estado=56,term="PDO",accion="reducir",regla=(PIzda "arit",Pdcha ["term","aritp"]), sig_estado=0}):
       (Elem_acc {estado=56,term="PAR_CER",accion="reducir",regla=(PIzda "arit",Pdcha ["term","aritp"]), sig_estado=0}):
       (Elem_acc {estado=57,term="OP_NOT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=36}):
       (Elem_acc {estado=57,term="OP_ADIT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=37}):
       (Elem_acc {estado=57,term="NUM",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=35}):
       (Elem_acc {estado=57,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=38}):
       (Elem_acc {estado=57,term="PAR_ABR",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=34}):
       (Elem_acc {estado=58,term="OP_REL",accion="reducir",regla=(PIzda "aritp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=58,term="OP_AND",accion="reducir",regla=(PIzda "aritp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=58,term="OP_OR",accion="reducir",regla=(PIzda "aritp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=58,term="PTHEN",accion="reducir",regla=(PIzda "aritp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=58,term="P_COMA",accion="reducir",regla=(PIzda "aritp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=58,term="PTODO",accion="reducir",regla=(PIzda "aritp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=58,term="PDO",accion="reducir",regla=(PIzda "aritp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=58,term="PAR_CER",accion="reducir",regla=(PIzda "aritp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=58,term="OP_ADIT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=57}):
       (Elem_acc {estado=59,term="OP_REL",accion="reducir",regla=(PIzda "aritp",Pdcha ["OP_ADIT","term","aritp"]), sig_estado=0}):
       (Elem_acc {estado=59,term="OP_AND",accion="reducir",regla=(PIzda "aritp",Pdcha ["OP_ADIT","term","aritp"]), sig_estado=0}):
       (Elem_acc {estado=59,term="OP_OR",accion="reducir",regla=(PIzda "aritp",Pdcha ["OP_ADIT","term","aritp"]), sig_estado=0}):
       (Elem_acc {estado=59,term="PTHEN",accion="reducir",regla=(PIzda "aritp",Pdcha ["OP_ADIT","term","aritp"]), sig_estado=0}):
       (Elem_acc {estado=59,term="P_COMA",accion="reducir",regla=(PIzda "aritp",Pdcha ["OP_ADIT","term","aritp"]), sig_estado=0}):
       (Elem_acc {estado=59,term="PTODO",accion="reducir",regla=(PIzda "aritp",Pdcha ["OP_ADIT","term","aritp"]), sig_estado=0}):
       (Elem_acc {estado=59,term="PDO",accion="reducir",regla=(PIzda "aritp",Pdcha ["OP_ADIT","term","aritp"]), sig_estado=0}):
       (Elem_acc {estado=59,term="PAR_CER",accion="reducir",regla=(PIzda "aritp",Pdcha ["OP_ADIT","term","aritp"]), sig_estado=0}):
       (Elem_acc {estado=60,term="OP_ADIT",accion="reducir",regla=(PIzda "term",Pdcha ["fact","termp"]), sig_estado=0}):
       (Elem_acc {estado=60,term="OP_REL",accion="reducir",regla=(PIzda "term",Pdcha ["fact","termp"]), sig_estado=0}):
       (Elem_acc {estado=60,term="OP_AND",accion="reducir",regla=(PIzda "term",Pdcha ["fact","termp"]), sig_estado=0}):
       (Elem_acc {estado=60,term="OP_OR",accion="reducir",regla=(PIzda "term",Pdcha ["fact","termp"]), sig_estado=0}):
       (Elem_acc {estado=60,term="PTHEN",accion="reducir",regla=(PIzda "term",Pdcha ["fact","termp"]), sig_estado=0}):
       (Elem_acc {estado=60,term="P_COMA",accion="reducir",regla=(PIzda "term",Pdcha ["fact","termp"]), sig_estado=0}):
       (Elem_acc {estado=60,term="PTODO",accion="reducir",regla=(PIzda "term",Pdcha ["fact","termp"]), sig_estado=0}):
       (Elem_acc {estado=60,term="PDO",accion="reducir",regla=(PIzda "term",Pdcha ["fact","termp"]), sig_estado=0}):
       (Elem_acc {estado=60,term="PAR_CER",accion="reducir",regla=(PIzda "term",Pdcha ["fact","termp"]), sig_estado=0}):
       (Elem_acc {estado=61,term="OP_NOT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=36}):
       (Elem_acc {estado=61,term="OP_ADIT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=37}):
       (Elem_acc {estado=61,term="NUM",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=35}):
       (Elem_acc {estado=61,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=38}):
       (Elem_acc {estado=61,term="PAR_ABR",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=34}):
       (Elem_acc {estado=62,term="OP_ADIT",accion="reducir",regla=(PIzda "termp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=62,term="OP_REL",accion="reducir",regla=(PIzda "termp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=62,term="OP_AND",accion="reducir",regla=(PIzda "termp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=62,term="OP_OR",accion="reducir",regla=(PIzda "termp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=62,term="PTHEN",accion="reducir",regla=(PIzda "termp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=62,term="P_COMA",accion="reducir",regla=(PIzda "termp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=62,term="PTODO",accion="reducir",regla=(PIzda "termp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=62,term="PDO",accion="reducir",regla=(PIzda "termp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=62,term="PAR_CER",accion="reducir",regla=(PIzda "termp",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=62,term="OP_MULT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=61}):
       (Elem_acc {estado=63,term="OP_ADIT",accion="reducir",regla=(PIzda "termp",Pdcha ["OP_MULT","fact","termp"]), sig_estado=0}):
       (Elem_acc {estado=63,term="OP_REL",accion="reducir",regla=(PIzda "termp",Pdcha ["OP_MULT","fact","termp"]), sig_estado=0}):
       (Elem_acc {estado=63,term="OP_AND",accion="reducir",regla=(PIzda "termp",Pdcha ["OP_MULT","fact","termp"]), sig_estado=0}):
       (Elem_acc {estado=63,term="OP_OR",accion="reducir",regla=(PIzda "termp",Pdcha ["OP_MULT","fact","termp"]), sig_estado=0}):
       (Elem_acc {estado=63,term="PTHEN",accion="reducir",regla=(PIzda "termp",Pdcha ["OP_MULT","fact","termp"]), sig_estado=0}):
       (Elem_acc {estado=63,term="P_COMA",accion="reducir",regla=(PIzda "termp",Pdcha ["OP_MULT","fact","termp"]), sig_estado=0}):
       (Elem_acc {estado=63,term="PTODO",accion="reducir",regla=(PIzda "termp",Pdcha ["OP_MULT","fact","termp"]), sig_estado=0}):
       (Elem_acc {estado=63,term="PDO",accion="reducir",regla=(PIzda "termp",Pdcha ["OP_MULT","fact","termp"]), sig_estado=0}):
       (Elem_acc {estado=63,term="PAR_CER",accion="reducir",regla=(PIzda "termp",Pdcha ["OP_MULT","fact","termp"]), sig_estado=0}):
       (Elem_acc {estado=64,term="P_COMA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=65}):
       (Elem_acc {estado=65,term="P_COMA",accion="reducir",regla=(PIzda "sent",Pdcha ["PREAD","ID","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=65,term="ID",accion="reducir",regla=(PIzda "sent",Pdcha ["PREAD","ID","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=65,term="PBEGIN",accion="reducir",regla=(PIzda "sent",Pdcha ["PREAD","ID","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=65,term="PREAD",accion="reducir",regla=(PIzda "sent",Pdcha ["PREAD","ID","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=65,term="PWRITE",accion="reducir",regla=(PIzda "sent",Pdcha ["PREAD","ID","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=65,term="PWRITC",accion="reducir",regla=(PIzda "sent",Pdcha ["PREAD","ID","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=65,term="PWRITL",accion="reducir",regla=(PIzda "sent",Pdcha ["PREAD","ID","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=65,term="PIF",accion="reducir",regla=(PIzda "sent",Pdcha ["PREAD","ID","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=65,term="PDO",accion="reducir",regla=(PIzda "sent",Pdcha ["PREAD","ID","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=65,term="PFORP",accion="reducir",regla=(PIzda "sent",Pdcha ["PREAD","ID","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=65,term="PEND",accion="reducir",regla=(PIzda "sent",Pdcha ["PREAD","ID","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=65,term="P_ELSE",accion="reducir",regla=(PIzda "sent",Pdcha ["PREAD","ID","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=65,term="PWHILE",accion="reducir",regla=(PIzda "sent",Pdcha ["PREAD","ID","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=66,term="P_COMA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=67}):
       (Elem_acc {estado=67,term="P_COMA",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=67,term="ID",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=67,term="PBEGIN",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=67,term="PREAD",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=67,term="PWRITE",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=67,term="PWRITC",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=67,term="PWRITL",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=67,term="PIF",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=67,term="PDO",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=67,term="PFORP",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=67,term="PEND",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=67,term="P_ELSE",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=67,term="PWHILE",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=68,term="P_COMA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=69}):
       (Elem_acc {estado=69,term="P_COMA",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITC","CAD","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=69,term="ID",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITC","CAD","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=69,term="PBEGIN",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITC","CAD","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=69,term="PREAD",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITC","CAD","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=69,term="PWRITE",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITC","CAD","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=69,term="PWRITC",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITC","CAD","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=69,term="PWRITL",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITC","CAD","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=69,term="PIF",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITC","CAD","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=69,term="PDO",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITC","CAD","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=69,term="PFORP",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITC","CAD","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=69,term="PEND",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITC","CAD","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=69,term="P_ELSE",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITC","CAD","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=69,term="PWHILE",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITC","CAD","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=70,term="P_COMA",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITL","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=70,term="ID",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITL","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=70,term="PBEGIN",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITL","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=70,term="PREAD",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITL","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=70,term="PWRITE",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITL","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=70,term="PWRITC",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITL","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=70,term="PWRITL",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITL","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=70,term="PIF",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITL","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=70,term="PDO",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITL","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=70,term="PFORP",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITL","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=70,term="PEND",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITL","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=70,term="P_ELSE",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITL","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=70,term="PWHILE",accion="reducir",regla=(PIzda "sent",Pdcha ["PWRITL","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=71,term="PTHEN",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=72}):
       (Elem_acc {estado=72,term="P_COMA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=24}):
       (Elem_acc {estado=72,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=25}):
       (Elem_acc {estado=72,term="PBEGIN",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=12}):
       (Elem_acc {estado=72,term="PREAD",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=23}):
       (Elem_acc {estado=72,term="PWRITE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=22}):
       (Elem_acc {estado=72,term="PWRITC",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=21}):
       (Elem_acc {estado=72,term="PWRITL",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
       (Elem_acc {estado=72,term="PIF",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=19}):
       (Elem_acc {estado=72,term="PWHILE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=18}):
       (Elem_acc {estado=72,term="PDO",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=17}):
       (Elem_acc {estado=72,term="PFORP",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=16}):
       (Elem_acc {estado=73,term="P_COMA",accion="reducir",regla=(PIzda "pelse",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=73,term="ID",accion="reducir",regla=(PIzda "pelse",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=73,term="PBEGIN",accion="reducir",regla=(PIzda "pelse",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=73,term="PREAD",accion="reducir",regla=(PIzda "pelse",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=73,term="PWRITE",accion="reducir",regla=(PIzda "pelse",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=73,term="PWRITC",accion="reducir",regla=(PIzda "pelse",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=73,term="PWRITL",accion="reducir",regla=(PIzda "pelse",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=73,term="PIF",accion="reducir",regla=(PIzda "pelse",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=73,term="PDO",accion="reducir",regla=(PIzda "pelse",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=73,term="PFORP",accion="reducir",regla=(PIzda "pelse",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=73,term="PEND",accion="reducir",regla=(PIzda "pelse",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=73,term="P_ELSE",accion="reducir",regla=(PIzda "pelse",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=73,term="PWHILE",accion="reducir",regla=(PIzda "pelse",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=73,term="P_ELSE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=75}):
       (Elem_acc {estado=74,term="P_COMA",accion="reducir",regla=(PIzda "sent",Pdcha ["PIF","expr","PTHEN","sent","pelse"]), sig_estado=0}):
       (Elem_acc {estado=74,term="ID",accion="reducir",regla=(PIzda "sent",Pdcha ["PIF","expr","PTHEN","sent","pelse"]), sig_estado=0}):
       (Elem_acc {estado=74,term="PBEGIN",accion="reducir",regla=(PIzda "sent",Pdcha ["PIF","expr","PTHEN","sent","pelse"]), sig_estado=0}):
       (Elem_acc {estado=74,term="PREAD",accion="reducir",regla=(PIzda "sent",Pdcha ["PIF","expr","PTHEN","sent","pelse"]), sig_estado=0}):
       (Elem_acc {estado=74,term="PWRITE",accion="reducir",regla=(PIzda "sent",Pdcha ["PIF","expr","PTHEN","sent","pelse"]), sig_estado=0}):
       (Elem_acc {estado=74,term="PWRITC",accion="reducir",regla=(PIzda "sent",Pdcha ["PIF","expr","PTHEN","sent","pelse"]), sig_estado=0}):
       (Elem_acc {estado=74,term="PWRITL",accion="reducir",regla=(PIzda "sent",Pdcha ["PIF","expr","PTHEN","sent","pelse"]), sig_estado=0}):
       (Elem_acc {estado=74,term="PIF",accion="reducir",regla=(PIzda "sent",Pdcha ["PIF","expr","PTHEN","sent","pelse"]), sig_estado=0}):
       (Elem_acc {estado=74,term="PDO",accion="reducir",regla=(PIzda "sent",Pdcha ["PIF","expr","PTHEN","sent","pelse"]), sig_estado=0}):
       (Elem_acc {estado=74,term="PFORP",accion="reducir",regla=(PIzda "sent",Pdcha ["PIF","expr","PTHEN","sent","pelse"]), sig_estado=0}):
       (Elem_acc {estado=74,term="PEND",accion="reducir",regla=(PIzda "sent",Pdcha ["PIF","expr","PTHEN","sent","pelse"]), sig_estado=0}):
       (Elem_acc {estado=74,term="P_ELSE",accion="reducir",regla=(PIzda "sent",Pdcha ["PIF","expr","PTHEN","sent","pelse"]), sig_estado=0}):
       (Elem_acc {estado=74,term="PWHILE",accion="reducir",regla=(PIzda "sent",Pdcha ["PIF","expr","PTHEN","sent","pelse"]), sig_estado=0}):
       (Elem_acc {estado=75,term="P_COMA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=24}):
       (Elem_acc {estado=75,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=25}):
       (Elem_acc {estado=75,term="PBEGIN",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=12}):
       (Elem_acc {estado=75,term="PREAD",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=23}):
       (Elem_acc {estado=75,term="PWRITE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=22}):
       (Elem_acc {estado=75,term="PWRITC",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=21}):
       (Elem_acc {estado=75,term="PWRITL",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
       (Elem_acc {estado=75,term="PIF",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=19}):
       (Elem_acc {estado=75,term="PWHILE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=18}):
       (Elem_acc {estado=75,term="PDO",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=17}):
       (Elem_acc {estado=75,term="PFORP",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=16}):
       (Elem_acc {estado=76,term="P_COMA",accion="reducir",regla=(PIzda "pelse",Pdcha ["P_ELSE","sent"]), sig_estado=0}):
       (Elem_acc {estado=76,term="ID",accion="reducir",regla=(PIzda "pelse",Pdcha ["P_ELSE","sent"]), sig_estado=0}):
       (Elem_acc {estado=76,term="PBEGIN",accion="reducir",regla=(PIzda "pelse",Pdcha ["P_ELSE","sent"]), sig_estado=0}):
       (Elem_acc {estado=76,term="PREAD",accion="reducir",regla=(PIzda "pelse",Pdcha ["P_ELSE","sent"]), sig_estado=0}):
       (Elem_acc {estado=76,term="PWRITE",accion="reducir",regla=(PIzda "pelse",Pdcha ["P_ELSE","sent"]), sig_estado=0}):
       (Elem_acc {estado=76,term="PWRITC",accion="reducir",regla=(PIzda "pelse",Pdcha ["P_ELSE","sent"]), sig_estado=0}):
       (Elem_acc {estado=76,term="PWRITL",accion="reducir",regla=(PIzda "pelse",Pdcha ["P_ELSE","sent"]), sig_estado=0}):
       (Elem_acc {estado=76,term="PIF",accion="reducir",regla=(PIzda "pelse",Pdcha ["P_ELSE","sent"]), sig_estado=0}):
       (Elem_acc {estado=76,term="PDO",accion="reducir",regla=(PIzda "pelse",Pdcha ["P_ELSE","sent"]), sig_estado=0}):
       (Elem_acc {estado=76,term="PFORP",accion="reducir",regla=(PIzda "pelse",Pdcha ["P_ELSE","sent"]), sig_estado=0}):
       (Elem_acc {estado=76,term="PEND",accion="reducir",regla=(PIzda "pelse",Pdcha ["P_ELSE","sent"]), sig_estado=0}):
       (Elem_acc {estado=76,term="P_ELSE",accion="reducir",regla=(PIzda "pelse",Pdcha ["P_ELSE","sent"]), sig_estado=0}):
       (Elem_acc {estado=76,term="PWHILE",accion="reducir",regla=(PIzda "pelse",Pdcha ["P_ELSE","sent"]), sig_estado=0}):
       (Elem_acc {estado=77,term="PDO",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=78}):
       (Elem_acc {estado=78,term="P_COMA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=24}):
       (Elem_acc {estado=78,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=25}):
       (Elem_acc {estado=78,term="PBEGIN",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=12}):
       (Elem_acc {estado=78,term="PREAD",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=23}):
       (Elem_acc {estado=78,term="PWRITE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=22}):
       (Elem_acc {estado=78,term="PWRITC",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=21}):
       (Elem_acc {estado=78,term="PWRITL",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
       (Elem_acc {estado=78,term="PIF",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=19}):
       (Elem_acc {estado=78,term="PWHILE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=18}):
       (Elem_acc {estado=78,term="PDO",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=17}):
       (Elem_acc {estado=78,term="PFORP",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=16}):
       (Elem_acc {estado=79,term="P_COMA",accion="reducir",regla=(PIzda "sent",Pdcha ["PWHILE","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=79,term="ID",accion="reducir",regla=(PIzda "sent",Pdcha ["PWHILE","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=79,term="PBEGIN",accion="reducir",regla=(PIzda "sent",Pdcha ["PWHILE","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=79,term="PREAD",accion="reducir",regla=(PIzda "sent",Pdcha ["PWHILE","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=79,term="PWRITE",accion="reducir",regla=(PIzda "sent",Pdcha ["PWHILE","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=79,term="PWRITC",accion="reducir",regla=(PIzda "sent",Pdcha ["PWHILE","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=79,term="PWRITL",accion="reducir",regla=(PIzda "sent",Pdcha ["PWHILE","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=79,term="PIF",accion="reducir",regla=(PIzda "sent",Pdcha ["PWHILE","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=79,term="PDO",accion="reducir",regla=(PIzda "sent",Pdcha ["PWHILE","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=79,term="PFORP",accion="reducir",regla=(PIzda "sent",Pdcha ["PWHILE","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=79,term="PEND",accion="reducir",regla=(PIzda "sent",Pdcha ["PWHILE","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=79,term="P_ELSE",accion="reducir",regla=(PIzda "sent",Pdcha ["PWHILE","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=79,term="PWHILE",accion="reducir",regla=(PIzda "sent",Pdcha ["PWHILE","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=80,term="PWHILE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=81}):
       (Elem_acc {estado=81,term="OP_NOT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=36}):
       (Elem_acc {estado=81,term="OP_ADIT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=37}):
       (Elem_acc {estado=81,term="NUM",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=35}):
       (Elem_acc {estado=81,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=38}):
       (Elem_acc {estado=81,term="PAR_ABR",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=34}):
       (Elem_acc {estado=82,term="P_COMA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=83}):
       (Elem_acc {estado=83,term="P_COMA",accion="reducir",regla=(PIzda "sent",Pdcha ["PDO","sent","PWHILE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=83,term="ID",accion="reducir",regla=(PIzda "sent",Pdcha ["PDO","sent","PWHILE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=83,term="PBEGIN",accion="reducir",regla=(PIzda "sent",Pdcha ["PDO","sent","PWHILE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=83,term="PREAD",accion="reducir",regla=(PIzda "sent",Pdcha ["PDO","sent","PWHILE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=83,term="PWRITE",accion="reducir",regla=(PIzda "sent",Pdcha ["PDO","sent","PWHILE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=83,term="PWRITC",accion="reducir",regla=(PIzda "sent",Pdcha ["PDO","sent","PWHILE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=83,term="PWRITL",accion="reducir",regla=(PIzda "sent",Pdcha ["PDO","sent","PWHILE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=83,term="PIF",accion="reducir",regla=(PIzda "sent",Pdcha ["PDO","sent","PWHILE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=83,term="PDO",accion="reducir",regla=(PIzda "sent",Pdcha ["PDO","sent","PWHILE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=83,term="PFORP",accion="reducir",regla=(PIzda "sent",Pdcha ["PDO","sent","PWHILE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=83,term="PEND",accion="reducir",regla=(PIzda "sent",Pdcha ["PDO","sent","PWHILE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=83,term="P_ELSE",accion="reducir",regla=(PIzda "sent",Pdcha ["PDO","sent","PWHILE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=83,term="PWHILE",accion="reducir",regla=(PIzda "sent",Pdcha ["PDO","sent","PWHILE","expr","P_COMA"]), sig_estado=0}):
       (Elem_acc {estado=84,term="ASIGN",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=85}):
       (Elem_acc {estado=85,term="OP_NOT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=36}):
       (Elem_acc {estado=85,term="OP_ADIT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=37}):
       (Elem_acc {estado=85,term="NUM",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=35}):
       (Elem_acc {estado=85,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=38}):
       (Elem_acc {estado=85,term="PAR_ABR",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=34}):
       (Elem_acc {estado=86,term="PTODO",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=87}):
       (Elem_acc {estado=87,term="OP_NOT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=36}):
       (Elem_acc {estado=87,term="OP_ADIT",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=37}):
       (Elem_acc {estado=87,term="NUM",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=35}):
       (Elem_acc {estado=87,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=38}):
       (Elem_acc {estado=87,term="PAR_ABR",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=34}):
       (Elem_acc {estado=88,term="PDO",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=89}):
       (Elem_acc {estado=89,term="P_COMA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=24}):
       (Elem_acc {estado=89,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=25}):
       (Elem_acc {estado=89,term="PBEGIN",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=12}):
       (Elem_acc {estado=89,term="PREAD",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=23}):
       (Elem_acc {estado=89,term="PWRITE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=22}):
       (Elem_acc {estado=89,term="PWRITC",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=21}):
       (Elem_acc {estado=89,term="PWRITL",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
       (Elem_acc {estado=89,term="PIF",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=19}):
       (Elem_acc {estado=89,term="PWHILE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=18}):
       (Elem_acc {estado=89,term="PDO",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=17}):
       (Elem_acc {estado=89,term="PFORP",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=16}):
       (Elem_acc {estado=90,term="P_COMA",accion="reducir",regla=(PIzda "sent",Pdcha ["PFORP","ID","ASIGN","expr","PTODO","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=90,term="ID",accion="reducir",regla=(PIzda "sent",Pdcha ["PFORP","ID","ASIGN","expr","PTODO","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=90,term="PBEGIN",accion="reducir",regla=(PIzda "sent",Pdcha ["PFORP","ID","ASIGN","expr","PTODO","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=90,term="PREAD",accion="reducir",regla=(PIzda "sent",Pdcha ["PFORP","ID","ASIGN","expr","PTODO","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=90,term="PWRITE",accion="reducir",regla=(PIzda "sent",Pdcha ["PFORP","ID","ASIGN","expr","PTODO","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=90,term="PWRITC",accion="reducir",regla=(PIzda "sent",Pdcha ["PFORP","ID","ASIGN","expr","PTODO","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=90,term="PWRITL",accion="reducir",regla=(PIzda "sent",Pdcha ["PFORP","ID","ASIGN","expr","PTODO","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=90,term="PIF",accion="reducir",regla=(PIzda "sent",Pdcha ["PFORP","ID","ASIGN","expr","PTODO","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=90,term="PDO",accion="reducir",regla=(PIzda "sent",Pdcha ["PFORP","ID","ASIGN","expr","PTODO","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=90,term="PFORP",accion="reducir",regla=(PIzda "sent",Pdcha ["PFORP","ID","ASIGN","expr","PTODO","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=90,term="PEND",accion="reducir",regla=(PIzda "sent",Pdcha ["PFORP","ID","ASIGN","expr","PTODO","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=90,term="P_ELSE",accion="reducir",regla=(PIzda "sent",Pdcha ["PFORP","ID","ASIGN","expr","PTODO","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=90,term="PWHILE",accion="reducir",regla=(PIzda "sent",Pdcha ["PFORP","ID","ASIGN","expr","PTODO","expr","PDO","sent"]), sig_estado=0}):
       (Elem_acc {estado=91,term="PUNTO",accion="reducir",regla=(PIzda "sentc",Pdcha ["PBEGIN","lsent","PEND"]), sig_estado=0}):
       (Elem_acc {estado=91,term="P_COMA",accion="reducir",regla=(PIzda "sentc",Pdcha ["PBEGIN","lsent","PEND"]), sig_estado=0}):
       (Elem_acc {estado=91,term="ID",accion="reducir",regla=(PIzda "sentc",Pdcha ["PBEGIN","lsent","PEND"]), sig_estado=0}):
       (Elem_acc {estado=91,term="PBEGIN",accion="reducir",regla=(PIzda "sentc",Pdcha ["PBEGIN","lsent","PEND"]), sig_estado=0}):
       (Elem_acc {estado=91,term="PREAD",accion="reducir",regla=(PIzda "sentc",Pdcha ["PBEGIN","lsent","PEND"]), sig_estado=0}):
       (Elem_acc {estado=91,term="PWRITE",accion="reducir",regla=(PIzda "sentc",Pdcha ["PBEGIN","lsent","PEND"]), sig_estado=0}):
       (Elem_acc {estado=91,term="PWRITC",accion="reducir",regla=(PIzda "sentc",Pdcha ["PBEGIN","lsent","PEND"]), sig_estado=0}):
       (Elem_acc {estado=91,term="PWRITL",accion="reducir",regla=(PIzda "sentc",Pdcha ["PBEGIN","lsent","PEND"]), sig_estado=0}):
       (Elem_acc {estado=91,term="PIF",accion="reducir",regla=(PIzda "sentc",Pdcha ["PBEGIN","lsent","PEND"]), sig_estado=0}):
       (Elem_acc {estado=91,term="PDO",accion="reducir",regla=(PIzda "sentc",Pdcha ["PBEGIN","lsent","PEND"]), sig_estado=0}):
       (Elem_acc {estado=91,term="PFORP",accion="reducir",regla=(PIzda "sentc",Pdcha ["PBEGIN","lsent","PEND"]), sig_estado=0}):
       (Elem_acc {estado=91,term="PEND",accion="reducir",regla=(PIzda "sentc",Pdcha ["PBEGIN","lsent","PEND"]), sig_estado=0}):
       (Elem_acc {estado=91,term="P_ELSE",accion="reducir",regla=(PIzda "sentc",Pdcha ["PBEGIN","lsent","PEND"]), sig_estado=0}):
       (Elem_acc {estado=91,term="PWHILE",accion="reducir",regla=(PIzda "sentc",Pdcha ["PBEGIN","lsent","PEND"]), sig_estado=0}):
       (Elem_acc {estado=92,term="PEND",accion="reducir",regla=(PIzda "lsent",Pdcha ["sent","lsent"]), sig_estado=0}):
       (Elem_acc {estado=93,term="$",accion="reducir",regla=(PIzda "prog",Pdcha ["P_PROG","ID","P_COMA","decl","sentc","PUNTO"]), sig_estado=0}):
       []
data Elem_ira =  Elem_ira {estado2::Int, var::String, sig_estado2::Int}
tabla_ira::[Elem_ira]
tabla_ira=(Elem_ira {estado2=1,var="prog",sig_estado2=2}):
       (Elem_ira {estado2=5,var="decl",sig_estado2=6}):
       (Elem_ira {estado2=6,var="sentc",sig_estado2=11}):
       (Elem_ira {estado2=9,var="decl",sig_estado2=10}):
       (Elem_ira {estado2=12,var="lsent",sig_estado2=15}):
       (Elem_ira {estado2=12,var="sent",sig_estado2=14}):
       (Elem_ira {estado2=12,var="sentc",sig_estado2=13}):
       (Elem_ira {estado2=14,var="lsent",sig_estado2=92}):
       (Elem_ira {estado2=14,var="sent",sig_estado2=14}):
       (Elem_ira {estado2=14,var="sentc",sig_estado2=13}):
       (Elem_ira {estado2=17,var="sent",sig_estado2=80}):
       (Elem_ira {estado2=17,var="sentc",sig_estado2=13}):
       (Elem_ira {estado2=18,var="expr",sig_estado2=77}):
       (Elem_ira {estado2=18,var="eand",sig_estado2=32}):
       (Elem_ira {estado2=18,var="erel",sig_estado2=31}):
       (Elem_ira {estado2=18,var="arit",sig_estado2=30}):
       (Elem_ira {estado2=18,var="term",sig_estado2=29}):
       (Elem_ira {estado2=18,var="fact",sig_estado2=28}):
       (Elem_ira {estado2=18,var="rando",sig_estado2=27}):
       (Elem_ira {estado2=19,var="expr",sig_estado2=71}):
       (Elem_ira {estado2=19,var="eand",sig_estado2=32}):
       (Elem_ira {estado2=19,var="erel",sig_estado2=31}):
       (Elem_ira {estado2=19,var="arit",sig_estado2=30}):
       (Elem_ira {estado2=19,var="term",sig_estado2=29}):
       (Elem_ira {estado2=19,var="fact",sig_estado2=28}):
       (Elem_ira {estado2=19,var="rando",sig_estado2=27}):
       (Elem_ira {estado2=22,var="expr",sig_estado2=66}):
       (Elem_ira {estado2=22,var="eand",sig_estado2=32}):
       (Elem_ira {estado2=22,var="erel",sig_estado2=31}):
       (Elem_ira {estado2=22,var="arit",sig_estado2=30}):
       (Elem_ira {estado2=22,var="term",sig_estado2=29}):
       (Elem_ira {estado2=22,var="fact",sig_estado2=28}):
       (Elem_ira {estado2=22,var="rando",sig_estado2=27}):
       (Elem_ira {estado2=26,var="expr",sig_estado2=33}):
       (Elem_ira {estado2=26,var="eand",sig_estado2=32}):
       (Elem_ira {estado2=26,var="erel",sig_estado2=31}):
       (Elem_ira {estado2=26,var="arit",sig_estado2=30}):
       (Elem_ira {estado2=26,var="term",sig_estado2=29}):
       (Elem_ira {estado2=26,var="fact",sig_estado2=28}):
       (Elem_ira {estado2=26,var="rando",sig_estado2=27}):
       (Elem_ira {estado2=28,var="termp",sig_estado2=60}):
       (Elem_ira {estado2=29,var="aritp",sig_estado2=56}):
       (Elem_ira {estado2=30,var="erelp",sig_estado2=52}):
       (Elem_ira {estado2=31,var="eandp",sig_estado2=48}):
       (Elem_ira {estado2=32,var="exprp",sig_estado2=44}):
       (Elem_ira {estado2=34,var="expr",sig_estado2=41}):
       (Elem_ira {estado2=34,var="eand",sig_estado2=32}):
       (Elem_ira {estado2=34,var="erel",sig_estado2=31}):
       (Elem_ira {estado2=34,var="arit",sig_estado2=30}):
       (Elem_ira {estado2=34,var="term",sig_estado2=29}):
       (Elem_ira {estado2=34,var="fact",sig_estado2=28}):
       (Elem_ira {estado2=34,var="rando",sig_estado2=27}):
       (Elem_ira {estado2=36,var="fact",sig_estado2=40}):
       (Elem_ira {estado2=36,var="rando",sig_estado2=27}):
       (Elem_ira {estado2=37,var="fact",sig_estado2=39}):
       (Elem_ira {estado2=37,var="rando",sig_estado2=27}):
       (Elem_ira {estado2=45,var="eand",sig_estado2=46}):
       (Elem_ira {estado2=45,var="erel",sig_estado2=31}):
       (Elem_ira {estado2=45,var="arit",sig_estado2=30}):
       (Elem_ira {estado2=45,var="term",sig_estado2=29}):
       (Elem_ira {estado2=45,var="fact",sig_estado2=28}):
       (Elem_ira {estado2=45,var="rando",sig_estado2=27}):
       (Elem_ira {estado2=46,var="exprp",sig_estado2=47}):
       (Elem_ira {estado2=49,var="erel",sig_estado2=50}):
       (Elem_ira {estado2=49,var="arit",sig_estado2=30}):
       (Elem_ira {estado2=49,var="term",sig_estado2=29}):
       (Elem_ira {estado2=49,var="fact",sig_estado2=28}):
       (Elem_ira {estado2=49,var="rando",sig_estado2=27}):
       (Elem_ira {estado2=50,var="eandp",sig_estado2=51}):
       (Elem_ira {estado2=53,var="arit",sig_estado2=54}):
       (Elem_ira {estado2=53,var="term",sig_estado2=29}):
       (Elem_ira {estado2=53,var="fact",sig_estado2=28}):
       (Elem_ira {estado2=53,var="rando",sig_estado2=27}):
       (Elem_ira {estado2=54,var="erelp",sig_estado2=55}):
       (Elem_ira {estado2=57,var="term",sig_estado2=58}):
       (Elem_ira {estado2=57,var="fact",sig_estado2=28}):
       (Elem_ira {estado2=57,var="rando",sig_estado2=27}):
       (Elem_ira {estado2=58,var="aritp",sig_estado2=59}):
       (Elem_ira {estado2=61,var="fact",sig_estado2=62}):
       (Elem_ira {estado2=61,var="rando",sig_estado2=27}):
       (Elem_ira {estado2=62,var="termp",sig_estado2=63}):
       (Elem_ira {estado2=72,var="sent",sig_estado2=73}):
       (Elem_ira {estado2=72,var="sentc",sig_estado2=13}):
       (Elem_ira {estado2=73,var="pelse",sig_estado2=74}):
       (Elem_ira {estado2=75,var="sent",sig_estado2=76}):
       (Elem_ira {estado2=75,var="sentc",sig_estado2=13}):
       (Elem_ira {estado2=78,var="sent",sig_estado2=79}):
       (Elem_ira {estado2=78,var="sentc",sig_estado2=13}):
       (Elem_ira {estado2=81,var="expr",sig_estado2=82}):
       (Elem_ira {estado2=81,var="eand",sig_estado2=32}):
       (Elem_ira {estado2=81,var="erel",sig_estado2=31}):
       (Elem_ira {estado2=81,var="arit",sig_estado2=30}):
       (Elem_ira {estado2=81,var="term",sig_estado2=29}):
       (Elem_ira {estado2=81,var="fact",sig_estado2=28}):
       (Elem_ira {estado2=81,var="rando",sig_estado2=27}):
       (Elem_ira {estado2=85,var="expr",sig_estado2=86}):
       (Elem_ira {estado2=85,var="eand",sig_estado2=32}):
       (Elem_ira {estado2=85,var="erel",sig_estado2=31}):
       (Elem_ira {estado2=85,var="arit",sig_estado2=30}):
       (Elem_ira {estado2=85,var="term",sig_estado2=29}):
       (Elem_ira {estado2=85,var="fact",sig_estado2=28}):
       (Elem_ira {estado2=85,var="rando",sig_estado2=27}):
       (Elem_ira {estado2=87,var="expr",sig_estado2=88}):
       (Elem_ira {estado2=87,var="eand",sig_estado2=32}):
       (Elem_ira {estado2=87,var="erel",sig_estado2=31}):
       (Elem_ira {estado2=87,var="arit",sig_estado2=30}):
       (Elem_ira {estado2=87,var="term",sig_estado2=29}):
       (Elem_ira {estado2=87,var="fact",sig_estado2=28}):
       (Elem_ira {estado2=87,var="rando",sig_estado2=27}):
       (Elem_ira {estado2=89,var="sent",sig_estado2=90}):
       (Elem_ira {estado2=89,var="sentc",sig_estado2=13}):
       []
axioma::String
axioma="Pprog"
terminales::[String]
terminales = ["P_PROG","ID","P_COMA","PUNTO","PVAR","PBEGIN","PEND","ASIGN","PREAD","PWRITE","PWRITC","PWRITL","PIF","PTHEN","P_ELSE","PWHILE","PDO","PFORP","PTODO","OP_OR","OP_AND","OP_REL","OP_ADIT","OP_MULT","OP_NOT","NUM","CAD","PAR_ABR","PAR_CER"]
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
visualizar_pila []=" "
parser_slr::String->[Int]->String
parser_slr x (y:ys)|(busca_accion tabla_acc y (fst (sigToken x)))=="desplazar" = parser_slr (substr x ((posb (snd (buscaSigToken x "")) 1 1 x)+(length (snd (buscaSigToken x "")))) (length x)) ((desplazar_a tabla_acc y (fst (sigToken x))):y:ys)
                        |(busca_accion tabla_acc y (fst (sigToken x)))=="reducir"  && 
                           (busca_ira tabla_ira (head (dropInt (if((reduce_por tabla_acc y (fst (sigToken x)))!!0=="lambda") then 0 else (length (reduce_por tabla_acc y (fst (sigToken x))))) (y:ys))) (reduce_a tabla_acc y (fst (sigToken x))))>0  = 
                              (visualizar_pila (y:ys))++"      "++(reduce_a tabla_acc y (fst (sigToken x)))++"->"++(if((reduce_por tabla_acc y (fst (sigToken x)))!!0=="lambda") then "lambda" else (concat (reduce_por tabla_acc y (fst (sigToken x)))))++"   "++
                             parser_slr x ((busca_ira tabla_ira (head (dropInt (if((reduce_por tabla_acc y (fst (sigToken x)))!!0=="lambda") then 0 else (length (reduce_por tabla_acc y (fst (sigToken x))))) (y:ys))) (reduce_a tabla_acc y (fst (sigToken x)))):(if((reduce_por tabla_acc y (fst (sigToken x)))!!0=="lambda") then (y:ys) else (dropInt (length (reduce_por tabla_acc y (fst (sigToken x)))) (y:ys)))) 
                      |(busca_accion tabla_acc y (fst (sigToken x)))=="reducir"    = (visualizar_pila (y:ys))++"      "++(reduce_a tabla_acc y (fst (sigToken x)))++"->"++(concat (reduce_por tabla_acc y (fst (sigToken x))))++"      "++x++
                                                                            "falta entrada en la tabla ira, estado="++show (head (dropInt (if((reduce_por tabla_acc y (fst (sigToken x)))!!0=="lambda") then 0 else (length (reduce_por tabla_acc y (fst (sigToken x))))) (y:ys)))++",variable="++(reduce_a tabla_acc y (fst (sigToken x))) 
                      |(busca_accion tabla_acc y (fst (sigToken x)))=="aceptar"   = "entrada correcta" 
                      |(busca_accion tabla_acc y (fst (sigToken x)))=="error"       = (visualizar_pila (y:ys))++"      "++x++
                                                                            "falta entrada en la tabla accion, estado="++(show y)++", terminal="++(fst (sigToken x))
parser_slr_arbol::String->[Int]->[Arbolsintactico]->Arbolsintactico
parser_slr_arbol x (y:ys) pila_sem |(busca_accion tabla_acc y (fst (sigToken x)))=="desplazar" = parser_slr_arbol (substr x ((posb (snd (buscaSigToken x "")) 1 1 x)+(length (snd (buscaSigToken x "")))) (length x)) ((desplazar_a tabla_acc y (fst (sigToken x))):y:ys) ((devolver_hoja (sigToken x)):pila_sem)
                        |(busca_accion tabla_acc y (fst (sigToken x)))=="reducir"  && 
                           (busca_ira tabla_ira (head (dropInt (if((reduce_por tabla_acc y (fst (sigToken x)))!!0=="lambda") then 0 else (length (reduce_por tabla_acc y (fst (sigToken x))))) (y:ys))) (reduce_a tabla_acc y (fst (sigToken x))))>0  = 
                                parser_slr_arbol x ((busca_ira tabla_ira (head (dropInt (if((reduce_por tabla_acc y (fst (sigToken x)))!!0=="lambda") then 0 else (length (reduce_por tabla_acc y (fst (sigToken x))))) (y:ys))) (reduce_a tabla_acc y (fst (sigToken x)))):(if((reduce_por tabla_acc y (fst (sigToken x)))!!0=="lambda") then (y:ys) else (dropInt (length (reduce_por tabla_acc y (fst (sigToken x)))) (y:ys)))) 
                                                        (if((reduce_por tabla_acc y (fst (sigToken x)))!!0=="lambda") then (devolver_rama_vacia (reduce_a tabla_acc y (fst (sigToken x)))):pila_sem else (devolver_rama (reduce_a tabla_acc y (fst (sigToken x))) (takeInt (length (reduce_por tabla_acc y (fst (sigToken x)))) pila_sem)):(dropInt (length (reduce_por tabla_acc y (fst (sigToken x)))) pila_sem))
                        |(busca_accion tabla_acc y (fst (sigToken x)))=="aceptar"   = (head pila_sem)
