module Parser_trans(parser_slr,parser_slr_arbol) where
import Utilidades
import Arbol
import Scaner_trans
--------------- Gramatica: -------------------------------------------------
-- irudia
-- MOZKETA BIRAKETA ISLAPENA LEKU_ALDATZEA ID ESLEI P_Z P_X P_Y P_B P_C P_D P_F P_G P_H ZENB P_IRE P_ITXI P_COMA G_IRE G_ITXI ARDATZAREN_INGURUKO_BIRAKETA ZATIHIP ZATIPAR SPLINE BEZIER BSPLINE BIRATU_OSOA 
-- irudia transformatu aldaketa_segida aldaketa posizio_bektorea posizio_bektore_multsoa lerroak bolumena elementua ardatza kurba lista_id rlista_id 
--irudia -> bolumena P_COMA transformatu
--bolumena -> ID G_IRE posizio_bektore_multsoa G_ITXI lerroak
--transformatu -> ID ESLEI ID aldaketa_segida
--aldaketa_segida -> aldaketa aldaketa_segida
--aldaketa_segida -> lambda
--aldaketa -> MOZKETA elementua ZENB
--aldaketa -> BIRAKETA ardatza ZENB
--aldaketa -> ISLAPENA ardatza
--aldaketa -> LEKU_ALDATZEA posizio_bektorea
--aldaketa -> ARDATZAREN_INGURUKO_BIRAKETA posizio_bektorea posizio_bektorea ZENB
--aldaketa -> BIRATU_OSOA ardatza
--ardatza -> P_X
--ardatza -> P_Y
--ardatza -> P_Z
--elementua -> P_B
--elementua -> P_C
--elementua -> P_D
--elementua -> P_F
--elementua -> P_G
--elementua -> P_H
--lerroak -> P_IRE ID ID kurba P_ITXI lerroak
--lerroak -> SPLINE lista_id lerroak
--lerroak -> BEZIER lista_id lerroak
--lerroak -> BSPLINE ZENB lista_id lerroak
--lerroak -> lambda
--posizio_bektore_multsoa -> posizio_bektorea posizio_bektore_multsoa
--posizio_bektore_multsoa -> lambda
--posizio_bektorea -> ID P_IRE ZENB ZENB ZENB ZENB P_ITXI
--kurba -> ZATIHIP ZENB ZENB aldaketa_segida
--kurba -> ZATIPAR ZENB aldaketa_segida
--kurba -> lambda
--lista_id -> ID rlista_id
--rlista_id -> ID rlista_id
--rlista_id -> lambda
--Pirudia -> irudia
----------------------------------------------------------------------------
data Partes = PIzda String|Pdcha [String]
type Regla=(Partes,Partes)
data Elem_acc =  Elem_acc {estado::Int, term::String, accion::String, regla::Regla, sig_estado::Int}
tabla_acc::[Elem_acc]
tabla_acc=(Elem_acc {estado=1,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=4}):
       (Elem_acc {estado=2,term="P_COMA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=72}):
       (Elem_acc {estado=3,term="$",accion="aceptar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=4,term="G_IRE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=5}):
       (Elem_acc {estado=5,term="G_ITXI",accion="reducir",regla=(PIzda "posizio_bektore_multsoa",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=5,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=6,term="G_ITXI",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=16}):
       (Elem_acc {estado=7,term="G_ITXI",accion="reducir",regla=(PIzda "posizio_bektore_multsoa",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=7,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=8,term="P_IRE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=9}):
       (Elem_acc {estado=9,term="ZENB",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=10}):
       (Elem_acc {estado=10,term="ZENB",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=11}):
       (Elem_acc {estado=11,term="ZENB",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=12}):
       (Elem_acc {estado=12,term="ZENB",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=13}):
       (Elem_acc {estado=13,term="P_ITXI",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=14}):
       (Elem_acc {estado=14,term="MOZKETA",accion="reducir",regla=(PIzda "posizio_bektorea",Pdcha ["ID","P_IRE","ZENB","ZENB","ZENB","ZENB","P_ITXI"]), sig_estado=0}):
       (Elem_acc {estado=14,term="BIRAKETA",accion="reducir",regla=(PIzda "posizio_bektorea",Pdcha ["ID","P_IRE","ZENB","ZENB","ZENB","ZENB","P_ITXI"]), sig_estado=0}):
       (Elem_acc {estado=14,term="ISLAPENA",accion="reducir",regla=(PIzda "posizio_bektorea",Pdcha ["ID","P_IRE","ZENB","ZENB","ZENB","ZENB","P_ITXI"]), sig_estado=0}):
       (Elem_acc {estado=14,term="LEKU_ALDATZEA",accion="reducir",regla=(PIzda "posizio_bektorea",Pdcha ["ID","P_IRE","ZENB","ZENB","ZENB","ZENB","P_ITXI"]), sig_estado=0}):
       (Elem_acc {estado=14,term="ARDATZAREN_INGURUKO_BIRAKETA",accion="reducir",regla=(PIzda "posizio_bektorea",Pdcha ["ID","P_IRE","ZENB","ZENB","ZENB","ZENB","P_ITXI"]), sig_estado=0}):
       (Elem_acc {estado=14,term="BIRATU_OSOA",accion="reducir",regla=(PIzda "posizio_bektorea",Pdcha ["ID","P_IRE","ZENB","ZENB","ZENB","ZENB","P_ITXI"]), sig_estado=0}):
       (Elem_acc {estado=14,term="$",accion="reducir",regla=(PIzda "posizio_bektorea",Pdcha ["ID","P_IRE","ZENB","ZENB","ZENB","ZENB","P_ITXI"]), sig_estado=0}):
       (Elem_acc {estado=14,term="P_ITXI",accion="reducir",regla=(PIzda "posizio_bektorea",Pdcha ["ID","P_IRE","ZENB","ZENB","ZENB","ZENB","P_ITXI"]), sig_estado=0}):
       (Elem_acc {estado=14,term="ZENB",accion="reducir",regla=(PIzda "posizio_bektorea",Pdcha ["ID","P_IRE","ZENB","ZENB","ZENB","ZENB","P_ITXI"]), sig_estado=0}):
       (Elem_acc {estado=14,term="ID",accion="reducir",regla=(PIzda "posizio_bektorea",Pdcha ["ID","P_IRE","ZENB","ZENB","ZENB","ZENB","P_ITXI"]), sig_estado=0}):
       (Elem_acc {estado=14,term="G_ITXI",accion="reducir",regla=(PIzda "posizio_bektorea",Pdcha ["ID","P_IRE","ZENB","ZENB","ZENB","ZENB","P_ITXI"]), sig_estado=0}):
       (Elem_acc {estado=15,term="G_ITXI",accion="reducir",regla=(PIzda "posizio_bektore_multsoa",Pdcha ["posizio_bektorea","posizio_bektore_multsoa"]), sig_estado=0}):
       (Elem_acc {estado=16,term="P_COMA",accion="reducir",regla=(PIzda "lerroak",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=16,term="P_IRE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=21}):
       (Elem_acc {estado=16,term="SPLINE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
       (Elem_acc {estado=16,term="BEZIER",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=19}):
       (Elem_acc {estado=16,term="BSPLINE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=18}):
       (Elem_acc {estado=17,term="P_COMA",accion="reducir",regla=(PIzda "bolumena",Pdcha ["ID","G_IRE","posizio_bektore_multsoa","G_ITXI","lerroak"]), sig_estado=0}):
       (Elem_acc {estado=18,term="ZENB",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=69}):
       (Elem_acc {estado=19,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=62}):
       (Elem_acc {estado=20,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=62}):
       (Elem_acc {estado=21,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=22}):
       (Elem_acc {estado=22,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=23}):
       (Elem_acc {estado=23,term="P_ITXI",accion="reducir",regla=(PIzda "kurba",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=23,term="ZATIHIP",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=26}):
       (Elem_acc {estado=23,term="ZATIPAR",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=25}):
       (Elem_acc {estado=24,term="P_ITXI",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=59}):
       (Elem_acc {estado=25,term="ZENB",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=57}):
       (Elem_acc {estado=26,term="ZENB",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=27}):
       (Elem_acc {estado=27,term="ZENB",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=28}):
       (Elem_acc {estado=28,term="$",accion="reducir",regla=(PIzda "aldaketa_segida",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=28,term="P_ITXI",accion="reducir",regla=(PIzda "aldaketa_segida",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=28,term="MOZKETA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=36}):
       (Elem_acc {estado=28,term="BIRAKETA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=35}):
       (Elem_acc {estado=28,term="ISLAPENA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=34}):
       (Elem_acc {estado=28,term="LEKU_ALDATZEA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=33}):
       (Elem_acc {estado=28,term="ARDATZAREN_INGURUKO_BIRAKETA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=32}):
       (Elem_acc {estado=28,term="BIRATU_OSOA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=31}):
       (Elem_acc {estado=29,term="$",accion="reducir",regla=(PIzda "aldaketa_segida",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=29,term="P_ITXI",accion="reducir",regla=(PIzda "aldaketa_segida",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=29,term="MOZKETA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=36}):
       (Elem_acc {estado=29,term="BIRAKETA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=35}):
       (Elem_acc {estado=29,term="ISLAPENA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=34}):
       (Elem_acc {estado=29,term="LEKU_ALDATZEA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=33}):
       (Elem_acc {estado=29,term="ARDATZAREN_INGURUKO_BIRAKETA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=32}):
       (Elem_acc {estado=29,term="BIRATU_OSOA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=31}):
       (Elem_acc {estado=30,term="P_ITXI",accion="reducir",regla=(PIzda "kurba",Pdcha ["ZATIHIP","ZENB","ZENB","aldaketa_segida"]), sig_estado=0}):
       (Elem_acc {estado=31,term="P_X",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=47}):
       (Elem_acc {estado=31,term="P_Y",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=46}):
       (Elem_acc {estado=31,term="P_Z",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=48}):
       (Elem_acc {estado=32,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=33,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=34,term="P_X",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=47}):
       (Elem_acc {estado=34,term="P_Y",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=46}):
       (Elem_acc {estado=34,term="P_Z",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=48}):
       (Elem_acc {estado=35,term="P_X",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=47}):
       (Elem_acc {estado=35,term="P_Y",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=46}):
       (Elem_acc {estado=35,term="P_Z",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=48}):
       (Elem_acc {estado=36,term="P_B",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=43}):
       (Elem_acc {estado=36,term="P_C",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=42}):
       (Elem_acc {estado=36,term="P_D",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=41}):
       (Elem_acc {estado=36,term="P_F",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=40}):
       (Elem_acc {estado=36,term="P_G",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=39}):
       (Elem_acc {estado=36,term="P_H",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=38}):
       (Elem_acc {estado=37,term="ZENB",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=44}):
       (Elem_acc {estado=38,term="ZENB",accion="reducir",regla=(PIzda "elementua",Pdcha ["P_H"]), sig_estado=0}):
       (Elem_acc {estado=39,term="ZENB",accion="reducir",regla=(PIzda "elementua",Pdcha ["P_G"]), sig_estado=0}):
       (Elem_acc {estado=40,term="ZENB",accion="reducir",regla=(PIzda "elementua",Pdcha ["P_F"]), sig_estado=0}):
       (Elem_acc {estado=41,term="ZENB",accion="reducir",regla=(PIzda "elementua",Pdcha ["P_D"]), sig_estado=0}):
       (Elem_acc {estado=42,term="ZENB",accion="reducir",regla=(PIzda "elementua",Pdcha ["P_C"]), sig_estado=0}):
       (Elem_acc {estado=43,term="ZENB",accion="reducir",regla=(PIzda "elementua",Pdcha ["P_B"]), sig_estado=0}):
       (Elem_acc {estado=44,term="MOZKETA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["MOZKETA","elementua","ZENB"]), sig_estado=0}):
       (Elem_acc {estado=44,term="BIRAKETA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["MOZKETA","elementua","ZENB"]), sig_estado=0}):
       (Elem_acc {estado=44,term="ISLAPENA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["MOZKETA","elementua","ZENB"]), sig_estado=0}):
       (Elem_acc {estado=44,term="LEKU_ALDATZEA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["MOZKETA","elementua","ZENB"]), sig_estado=0}):
       (Elem_acc {estado=44,term="ARDATZAREN_INGURUKO_BIRAKETA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["MOZKETA","elementua","ZENB"]), sig_estado=0}):
       (Elem_acc {estado=44,term="BIRATU_OSOA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["MOZKETA","elementua","ZENB"]), sig_estado=0}):
       (Elem_acc {estado=44,term="$",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["MOZKETA","elementua","ZENB"]), sig_estado=0}):
       (Elem_acc {estado=44,term="P_ITXI",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["MOZKETA","elementua","ZENB"]), sig_estado=0}):
       (Elem_acc {estado=45,term="ZENB",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=49}):
       (Elem_acc {estado=46,term="ZENB",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_Y"]), sig_estado=0}):
       (Elem_acc {estado=46,term="MOZKETA",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_Y"]), sig_estado=0}):
       (Elem_acc {estado=46,term="BIRAKETA",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_Y"]), sig_estado=0}):
       (Elem_acc {estado=46,term="ISLAPENA",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_Y"]), sig_estado=0}):
       (Elem_acc {estado=46,term="LEKU_ALDATZEA",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_Y"]), sig_estado=0}):
       (Elem_acc {estado=46,term="ARDATZAREN_INGURUKO_BIRAKETA",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_Y"]), sig_estado=0}):
       (Elem_acc {estado=46,term="BIRATU_OSOA",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_Y"]), sig_estado=0}):
       (Elem_acc {estado=46,term="$",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_Y"]), sig_estado=0}):
       (Elem_acc {estado=46,term="P_ITXI",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_Y"]), sig_estado=0}):
       (Elem_acc {estado=47,term="ZENB",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_X"]), sig_estado=0}):
       (Elem_acc {estado=47,term="MOZKETA",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_X"]), sig_estado=0}):
       (Elem_acc {estado=47,term="BIRAKETA",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_X"]), sig_estado=0}):
       (Elem_acc {estado=47,term="ISLAPENA",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_X"]), sig_estado=0}):
       (Elem_acc {estado=47,term="LEKU_ALDATZEA",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_X"]), sig_estado=0}):
       (Elem_acc {estado=47,term="ARDATZAREN_INGURUKO_BIRAKETA",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_X"]), sig_estado=0}):
       (Elem_acc {estado=47,term="BIRATU_OSOA",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_X"]), sig_estado=0}):
       (Elem_acc {estado=47,term="$",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_X"]), sig_estado=0}):
       (Elem_acc {estado=47,term="P_ITXI",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_X"]), sig_estado=0}):
       (Elem_acc {estado=48,term="ZENB",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_Z"]), sig_estado=0}):
       (Elem_acc {estado=48,term="MOZKETA",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_Z"]), sig_estado=0}):
       (Elem_acc {estado=48,term="BIRAKETA",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_Z"]), sig_estado=0}):
       (Elem_acc {estado=48,term="ISLAPENA",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_Z"]), sig_estado=0}):
       (Elem_acc {estado=48,term="LEKU_ALDATZEA",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_Z"]), sig_estado=0}):
       (Elem_acc {estado=48,term="ARDATZAREN_INGURUKO_BIRAKETA",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_Z"]), sig_estado=0}):
       (Elem_acc {estado=48,term="BIRATU_OSOA",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_Z"]), sig_estado=0}):
       (Elem_acc {estado=48,term="$",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_Z"]), sig_estado=0}):
       (Elem_acc {estado=48,term="P_ITXI",accion="reducir",regla=(PIzda "ardatza",Pdcha ["P_Z"]), sig_estado=0}):
       (Elem_acc {estado=49,term="MOZKETA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["BIRAKETA","ardatza","ZENB"]), sig_estado=0}):
       (Elem_acc {estado=49,term="BIRAKETA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["BIRAKETA","ardatza","ZENB"]), sig_estado=0}):
       (Elem_acc {estado=49,term="ISLAPENA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["BIRAKETA","ardatza","ZENB"]), sig_estado=0}):
       (Elem_acc {estado=49,term="LEKU_ALDATZEA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["BIRAKETA","ardatza","ZENB"]), sig_estado=0}):
       (Elem_acc {estado=49,term="ARDATZAREN_INGURUKO_BIRAKETA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["BIRAKETA","ardatza","ZENB"]), sig_estado=0}):
       (Elem_acc {estado=49,term="BIRATU_OSOA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["BIRAKETA","ardatza","ZENB"]), sig_estado=0}):
       (Elem_acc {estado=49,term="$",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["BIRAKETA","ardatza","ZENB"]), sig_estado=0}):
       (Elem_acc {estado=49,term="P_ITXI",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["BIRAKETA","ardatza","ZENB"]), sig_estado=0}):
       (Elem_acc {estado=50,term="MOZKETA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["ISLAPENA","ardatza"]), sig_estado=0}):
       (Elem_acc {estado=50,term="BIRAKETA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["ISLAPENA","ardatza"]), sig_estado=0}):
       (Elem_acc {estado=50,term="ISLAPENA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["ISLAPENA","ardatza"]), sig_estado=0}):
       (Elem_acc {estado=50,term="LEKU_ALDATZEA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["ISLAPENA","ardatza"]), sig_estado=0}):
       (Elem_acc {estado=50,term="ARDATZAREN_INGURUKO_BIRAKETA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["ISLAPENA","ardatza"]), sig_estado=0}):
       (Elem_acc {estado=50,term="BIRATU_OSOA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["ISLAPENA","ardatza"]), sig_estado=0}):
       (Elem_acc {estado=50,term="$",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["ISLAPENA","ardatza"]), sig_estado=0}):
       (Elem_acc {estado=50,term="P_ITXI",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["ISLAPENA","ardatza"]), sig_estado=0}):
       (Elem_acc {estado=51,term="MOZKETA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["LEKU_ALDATZEA","posizio_bektorea"]), sig_estado=0}):
       (Elem_acc {estado=51,term="BIRAKETA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["LEKU_ALDATZEA","posizio_bektorea"]), sig_estado=0}):
       (Elem_acc {estado=51,term="ISLAPENA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["LEKU_ALDATZEA","posizio_bektorea"]), sig_estado=0}):
       (Elem_acc {estado=51,term="LEKU_ALDATZEA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["LEKU_ALDATZEA","posizio_bektorea"]), sig_estado=0}):
       (Elem_acc {estado=51,term="ARDATZAREN_INGURUKO_BIRAKETA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["LEKU_ALDATZEA","posizio_bektorea"]), sig_estado=0}):
       (Elem_acc {estado=51,term="BIRATU_OSOA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["LEKU_ALDATZEA","posizio_bektorea"]), sig_estado=0}):
       (Elem_acc {estado=51,term="$",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["LEKU_ALDATZEA","posizio_bektorea"]), sig_estado=0}):
       (Elem_acc {estado=51,term="P_ITXI",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["LEKU_ALDATZEA","posizio_bektorea"]), sig_estado=0}):
       (Elem_acc {estado=52,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=53,term="ZENB",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=54}):
       (Elem_acc {estado=54,term="MOZKETA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["ARDATZAREN_INGURUKO_BIRAKETA","posizio_bektorea","posizio_bektorea","ZENB"]), sig_estado=0}):
       (Elem_acc {estado=54,term="BIRAKETA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["ARDATZAREN_INGURUKO_BIRAKETA","posizio_bektorea","posizio_bektorea","ZENB"]), sig_estado=0}):
       (Elem_acc {estado=54,term="ISLAPENA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["ARDATZAREN_INGURUKO_BIRAKETA","posizio_bektorea","posizio_bektorea","ZENB"]), sig_estado=0}):
       (Elem_acc {estado=54,term="LEKU_ALDATZEA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["ARDATZAREN_INGURUKO_BIRAKETA","posizio_bektorea","posizio_bektorea","ZENB"]), sig_estado=0}):
       (Elem_acc {estado=54,term="ARDATZAREN_INGURUKO_BIRAKETA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["ARDATZAREN_INGURUKO_BIRAKETA","posizio_bektorea","posizio_bektorea","ZENB"]), sig_estado=0}):
       (Elem_acc {estado=54,term="BIRATU_OSOA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["ARDATZAREN_INGURUKO_BIRAKETA","posizio_bektorea","posizio_bektorea","ZENB"]), sig_estado=0}):
       (Elem_acc {estado=54,term="$",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["ARDATZAREN_INGURUKO_BIRAKETA","posizio_bektorea","posizio_bektorea","ZENB"]), sig_estado=0}):
       (Elem_acc {estado=54,term="P_ITXI",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["ARDATZAREN_INGURUKO_BIRAKETA","posizio_bektorea","posizio_bektorea","ZENB"]), sig_estado=0}):
       (Elem_acc {estado=55,term="MOZKETA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["BIRATU_OSOA","ardatza"]), sig_estado=0}):
       (Elem_acc {estado=55,term="BIRAKETA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["BIRATU_OSOA","ardatza"]), sig_estado=0}):
       (Elem_acc {estado=55,term="ISLAPENA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["BIRATU_OSOA","ardatza"]), sig_estado=0}):
       (Elem_acc {estado=55,term="LEKU_ALDATZEA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["BIRATU_OSOA","ardatza"]), sig_estado=0}):
       (Elem_acc {estado=55,term="ARDATZAREN_INGURUKO_BIRAKETA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["BIRATU_OSOA","ardatza"]), sig_estado=0}):
       (Elem_acc {estado=55,term="BIRATU_OSOA",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["BIRATU_OSOA","ardatza"]), sig_estado=0}):
       (Elem_acc {estado=55,term="$",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["BIRATU_OSOA","ardatza"]), sig_estado=0}):
       (Elem_acc {estado=55,term="P_ITXI",accion="reducir",regla=(PIzda "aldaketa",Pdcha ["BIRATU_OSOA","ardatza"]), sig_estado=0}):
       (Elem_acc {estado=56,term="$",accion="reducir",regla=(PIzda "aldaketa_segida",Pdcha ["aldaketa","aldaketa_segida"]), sig_estado=0}):
       (Elem_acc {estado=56,term="P_ITXI",accion="reducir",regla=(PIzda "aldaketa_segida",Pdcha ["aldaketa","aldaketa_segida"]), sig_estado=0}):
       (Elem_acc {estado=57,term="$",accion="reducir",regla=(PIzda "aldaketa_segida",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=57,term="P_ITXI",accion="reducir",regla=(PIzda "aldaketa_segida",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=57,term="MOZKETA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=36}):
       (Elem_acc {estado=57,term="BIRAKETA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=35}):
       (Elem_acc {estado=57,term="ISLAPENA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=34}):
       (Elem_acc {estado=57,term="LEKU_ALDATZEA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=33}):
       (Elem_acc {estado=57,term="ARDATZAREN_INGURUKO_BIRAKETA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=32}):
       (Elem_acc {estado=57,term="BIRATU_OSOA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=31}):
       (Elem_acc {estado=58,term="P_ITXI",accion="reducir",regla=(PIzda "kurba",Pdcha ["ZATIPAR","ZENB","aldaketa_segida"]), sig_estado=0}):
       (Elem_acc {estado=59,term="P_COMA",accion="reducir",regla=(PIzda "lerroak",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=59,term="P_IRE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=21}):
       (Elem_acc {estado=59,term="SPLINE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
       (Elem_acc {estado=59,term="BEZIER",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=19}):
       (Elem_acc {estado=59,term="BSPLINE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=18}):
       (Elem_acc {estado=60,term="P_COMA",accion="reducir",regla=(PIzda "lerroak",Pdcha ["P_IRE","ID","ID","kurba","P_ITXI","lerroak"]), sig_estado=0}):
       (Elem_acc {estado=61,term="P_COMA",accion="reducir",regla=(PIzda "lerroak",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=61,term="P_IRE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=21}):
       (Elem_acc {estado=61,term="SPLINE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
       (Elem_acc {estado=61,term="BEZIER",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=19}):
       (Elem_acc {estado=61,term="BSPLINE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=18}):
       (Elem_acc {estado=62,term="P_IRE",accion="reducir",regla=(PIzda "rlista_id",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=62,term="SPLINE",accion="reducir",regla=(PIzda "rlista_id",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=62,term="BEZIER",accion="reducir",regla=(PIzda "rlista_id",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=62,term="BSPLINE",accion="reducir",regla=(PIzda "rlista_id",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=62,term="P_COMA",accion="reducir",regla=(PIzda "rlista_id",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=62,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=64}):
       (Elem_acc {estado=63,term="P_IRE",accion="reducir",regla=(PIzda "lista_id",Pdcha ["ID","rlista_id"]), sig_estado=0}):
       (Elem_acc {estado=63,term="SPLINE",accion="reducir",regla=(PIzda "lista_id",Pdcha ["ID","rlista_id"]), sig_estado=0}):
       (Elem_acc {estado=63,term="BEZIER",accion="reducir",regla=(PIzda "lista_id",Pdcha ["ID","rlista_id"]), sig_estado=0}):
       (Elem_acc {estado=63,term="BSPLINE",accion="reducir",regla=(PIzda "lista_id",Pdcha ["ID","rlista_id"]), sig_estado=0}):
       (Elem_acc {estado=63,term="P_COMA",accion="reducir",regla=(PIzda "lista_id",Pdcha ["ID","rlista_id"]), sig_estado=0}):
       (Elem_acc {estado=64,term="P_IRE",accion="reducir",regla=(PIzda "rlista_id",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=64,term="SPLINE",accion="reducir",regla=(PIzda "rlista_id",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=64,term="BEZIER",accion="reducir",regla=(PIzda "rlista_id",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=64,term="BSPLINE",accion="reducir",regla=(PIzda "rlista_id",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=64,term="P_COMA",accion="reducir",regla=(PIzda "rlista_id",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=64,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=64}):
       (Elem_acc {estado=65,term="P_IRE",accion="reducir",regla=(PIzda "rlista_id",Pdcha ["ID","rlista_id"]), sig_estado=0}):
       (Elem_acc {estado=65,term="SPLINE",accion="reducir",regla=(PIzda "rlista_id",Pdcha ["ID","rlista_id"]), sig_estado=0}):
       (Elem_acc {estado=65,term="BEZIER",accion="reducir",regla=(PIzda "rlista_id",Pdcha ["ID","rlista_id"]), sig_estado=0}):
       (Elem_acc {estado=65,term="BSPLINE",accion="reducir",regla=(PIzda "rlista_id",Pdcha ["ID","rlista_id"]), sig_estado=0}):
       (Elem_acc {estado=65,term="P_COMA",accion="reducir",regla=(PIzda "rlista_id",Pdcha ["ID","rlista_id"]), sig_estado=0}):
       (Elem_acc {estado=66,term="P_COMA",accion="reducir",regla=(PIzda "lerroak",Pdcha ["SPLINE","lista_id","lerroak"]), sig_estado=0}):
       (Elem_acc {estado=67,term="P_COMA",accion="reducir",regla=(PIzda "lerroak",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=67,term="P_IRE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=21}):
       (Elem_acc {estado=67,term="SPLINE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
       (Elem_acc {estado=67,term="BEZIER",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=19}):
       (Elem_acc {estado=67,term="BSPLINE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=18}):
       (Elem_acc {estado=68,term="P_COMA",accion="reducir",regla=(PIzda "lerroak",Pdcha ["BEZIER","lista_id","lerroak"]), sig_estado=0}):
       (Elem_acc {estado=69,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=62}):
       (Elem_acc {estado=70,term="P_COMA",accion="reducir",regla=(PIzda "lerroak",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=70,term="P_IRE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=21}):
       (Elem_acc {estado=70,term="SPLINE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
       (Elem_acc {estado=70,term="BEZIER",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=19}):
       (Elem_acc {estado=70,term="BSPLINE",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=18}):
       (Elem_acc {estado=71,term="P_COMA",accion="reducir",regla=(PIzda "lerroak",Pdcha ["BSPLINE","ZENB","lista_id","lerroak"]), sig_estado=0}):
       (Elem_acc {estado=72,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=74}):
       (Elem_acc {estado=73,term="$",accion="reducir",regla=(PIzda "irudia",Pdcha ["bolumena","P_COMA","transformatu"]), sig_estado=0}):
       (Elem_acc {estado=74,term="ESLEI",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=75}):
       (Elem_acc {estado=75,term="ID",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=76}):
       (Elem_acc {estado=76,term="$",accion="reducir",regla=(PIzda "aldaketa_segida",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=76,term="P_ITXI",accion="reducir",regla=(PIzda "aldaketa_segida",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=76,term="MOZKETA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=36}):
       (Elem_acc {estado=76,term="BIRAKETA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=35}):
       (Elem_acc {estado=76,term="ISLAPENA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=34}):
       (Elem_acc {estado=76,term="LEKU_ALDATZEA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=33}):
       (Elem_acc {estado=76,term="ARDATZAREN_INGURUKO_BIRAKETA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=32}):
       (Elem_acc {estado=76,term="BIRATU_OSOA",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=31}):
       (Elem_acc {estado=77,term="$",accion="reducir",regla=(PIzda "transformatu",Pdcha ["ID","ESLEI","ID","aldaketa_segida"]), sig_estado=0}):
       []
data Elem_ira =  Elem_ira {estado2::Int, var::String, sig_estado2::Int}
tabla_ira::[Elem_ira]
tabla_ira=(Elem_ira {estado2=1,var="irudia",sig_estado2=3}):
       (Elem_ira {estado2=1,var="bolumena",sig_estado2=2}):
       (Elem_ira {estado2=5,var="posizio_bektore_multsoa",sig_estado2=6}):
       (Elem_ira {estado2=5,var="posizio_bektorea",sig_estado2=7}):
       (Elem_ira {estado2=7,var="posizio_bektore_multsoa",sig_estado2=15}):
       (Elem_ira {estado2=7,var="posizio_bektorea",sig_estado2=7}):
       (Elem_ira {estado2=16,var="lerroak",sig_estado2=17}):
       (Elem_ira {estado2=19,var="lista_id",sig_estado2=67}):
       (Elem_ira {estado2=20,var="lista_id",sig_estado2=61}):
       (Elem_ira {estado2=23,var="kurba",sig_estado2=24}):
       (Elem_ira {estado2=28,var="aldaketa_segida",sig_estado2=30}):
       (Elem_ira {estado2=28,var="aldaketa",sig_estado2=29}):
       (Elem_ira {estado2=29,var="aldaketa_segida",sig_estado2=56}):
       (Elem_ira {estado2=29,var="aldaketa",sig_estado2=29}):
       (Elem_ira {estado2=31,var="ardatza",sig_estado2=55}):
       (Elem_ira {estado2=32,var="posizio_bektorea",sig_estado2=52}):
       (Elem_ira {estado2=33,var="posizio_bektorea",sig_estado2=51}):
       (Elem_ira {estado2=34,var="ardatza",sig_estado2=50}):
       (Elem_ira {estado2=35,var="ardatza",sig_estado2=45}):
       (Elem_ira {estado2=36,var="elementua",sig_estado2=37}):
       (Elem_ira {estado2=52,var="posizio_bektorea",sig_estado2=53}):
       (Elem_ira {estado2=57,var="aldaketa_segida",sig_estado2=58}):
       (Elem_ira {estado2=57,var="aldaketa",sig_estado2=29}):
       (Elem_ira {estado2=59,var="lerroak",sig_estado2=60}):
       (Elem_ira {estado2=61,var="lerroak",sig_estado2=66}):
       (Elem_ira {estado2=62,var="rlista_id",sig_estado2=63}):
       (Elem_ira {estado2=64,var="rlista_id",sig_estado2=65}):
       (Elem_ira {estado2=67,var="lerroak",sig_estado2=68}):
       (Elem_ira {estado2=69,var="lista_id",sig_estado2=70}):
       (Elem_ira {estado2=70,var="lerroak",sig_estado2=71}):
       (Elem_ira {estado2=72,var="transformatu",sig_estado2=73}):
       (Elem_ira {estado2=76,var="aldaketa_segida",sig_estado2=77}):
       (Elem_ira {estado2=76,var="aldaketa",sig_estado2=29}):
       []
axioma::String
axioma="Pirudia"
terminales::[String]
terminales = ["MOZKETA","BIRAKETA","ISLAPENA","LEKU_ALDATZEA","ID","ESLEI","P_Z","P_X","P_Y","P_B","P_C","P_D","P_F","P_G","P_H","ZENB","P_IRE","P_ITXI","P_COMA","G_IRE","G_ITXI","ARDATZAREN_INGURUKO_BIRAKETA","ZATIHIP","ZATIPAR","SPLINE","BEZIER","BSPLINE","BIRATU_OSOA"]
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
parser_slr::String->[Int]->String
parser_slr x (y:ys)|(busca_accion tabla_acc y (fst (sigToken x)))=="desplazar" = parser_slr (substr x ((posb (snd (buscaSigToken x "")) 1 1 x)+(length (snd (buscaSigToken x "")))) (length x)) ((desplazar_a tabla_acc y (fst (sigToken x))):y:ys)
                        |(busca_accion tabla_acc y (fst (sigToken x)))=="reducir"  && 
                           (busca_ira tabla_ira (head (dropInt (if((reduce_por tabla_acc y (fst (sigToken x)))!!0=="lambda") then 0 else (length (reduce_por tabla_acc y (fst (sigToken x))))) (y:ys))) (reduce_a tabla_acc y (fst (sigToken x))))>0  = 
                              (visualizar_pila (y:ys))++"      "++(reduce_a tabla_acc y (fst (sigToken x)))++"->"++(concat (reduce_por tabla_acc y (fst (sigToken x))))++"      "++x++"   "++
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
