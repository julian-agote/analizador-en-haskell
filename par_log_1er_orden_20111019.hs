import Utilidades
import Data.Char(ord)
data Partes = PIzda String|Pdcha [String]
type Regla=(Partes,Partes)
data Elem_acc =  Elem_acc {estado::Int, term::String, accion::String, regla::Regla, sig_estado::Int}
tabla_acc::[Elem_acc]
tabla_acc=(Elem_acc {estado=1,term="neg",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=7}):
       (Elem_acc {estado=1,term="(",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=1,term="prop",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=9}):
       (Elem_acc {estado=2,term="and",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=2,term="or",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=2,term="$",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=2,term="coma",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=2,term="=>",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=2,term=")",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=2,term="impl",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=30}):
       (Elem_acc {estado=2,term="coimpl",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=29}):
       (Elem_acc {estado=3,term="$",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=3,term="coma",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=3,term="=>",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=3,term=")",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=3,term="and",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=21}):
       (Elem_acc {estado=3,term="or",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
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
       (Elem_acc {estado=9,term="and",accion="reducir",regla=(PIzda "G",Pdcha ["prop"]), sig_estado=0}):
       (Elem_acc {estado=9,term="or",accion="reducir",regla=(PIzda "G",Pdcha ["prop"]), sig_estado=0}):
       (Elem_acc {estado=9,term="$",accion="reducir",regla=(PIzda "G",Pdcha ["prop"]), sig_estado=0}):
       (Elem_acc {estado=9,term="coma",accion="reducir",regla=(PIzda "G",Pdcha ["prop"]), sig_estado=0}):
       (Elem_acc {estado=9,term="=>",accion="reducir",regla=(PIzda "G",Pdcha ["prop"]), sig_estado=0}):
       (Elem_acc {estado=9,term=")",accion="reducir",regla=(PIzda "G",Pdcha ["prop"]), sig_estado=0}):
       (Elem_acc {estado=10,term=")",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=11}):
       (Elem_acc {estado=11,term="impl",accion="reducir",regla=(PIzda "G",Pdcha ["(","FCONJ",")"]), sig_estado=0}):
       (Elem_acc {estado=11,term="coimpl",accion="reducir",regla=(PIzda "G",Pdcha ["(","FCONJ",")"]), sig_estado=0}):
       (Elem_acc {estado=11,term="and",accion="reducir",regla=(PIzda "G",Pdcha ["(","FCONJ",")"]), sig_estado=0}):
       (Elem_acc {estado=11,term="or",accion="reducir",regla=(PIzda "G",Pdcha ["(","FCONJ",")"]), sig_estado=0}):
       (Elem_acc {estado=11,term="$",accion="reducir",regla=(PIzda "G",Pdcha ["(","FCONJ",")"]), sig_estado=0}):
       (Elem_acc {estado=11,term="coma",accion="reducir",regla=(PIzda "G",Pdcha ["(","FCONJ",")"]), sig_estado=0}):
       (Elem_acc {estado=11,term="=>",accion="reducir",regla=(PIzda "G",Pdcha ["(","FCONJ",")"]), sig_estado=0}):
       (Elem_acc {estado=11,term=")",accion="reducir",regla=(PIzda "G",Pdcha ["(","FCONJ",")"]), sig_estado=0}):
       (Elem_acc {estado=12,term="impl",accion="reducir",regla=(PIzda "G",Pdcha ["neg","G"]), sig_estado=0}):
       (Elem_acc {estado=12,term="coimpl",accion="reducir",regla=(PIzda "G",Pdcha ["neg","G"]), sig_estado=0}):
       (Elem_acc {estado=12,term="and",accion="reducir",regla=(PIzda "G",Pdcha ["neg","G"]), sig_estado=0}):
       (Elem_acc {estado=12,term="or",accion="reducir",regla=(PIzda "G",Pdcha ["neg","G"]), sig_estado=0}):
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
       (Elem_acc {estado=21,term="neg",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=7}):
       (Elem_acc {estado=21,term="(",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=21,term="prop",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=9}):
       (Elem_acc {estado=22,term="and",accion="reducir",regla=(PIzda "MCONJ",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=22,term="or",accion="reducir",regla=(PIzda "MCONJ",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=22,term="$",accion="reducir",regla=(PIzda "MCONJ",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=22,term="coma",accion="reducir",regla=(PIzda "MCONJ",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=22,term="=>",accion="reducir",regla=(PIzda "MCONJ",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=22,term=")",accion="reducir",regla=(PIzda "MCONJ",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=23,term="$",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=23,term="coma",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=23,term="=>",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=23,term=")",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=23,term="and",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=21}):
       (Elem_acc {estado=23,term="or",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
       (Elem_acc {estado=24,term="$",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["and","FIMPL","MCONJ","FCONJP"]), sig_estado=0}):
       (Elem_acc {estado=24,term="coma",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["and","FIMPL","MCONJ","FCONJP"]), sig_estado=0}):
       (Elem_acc {estado=24,term="=>",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["and","FIMPL","MCONJ","FCONJP"]), sig_estado=0}):
       (Elem_acc {estado=24,term=")",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["and","FIMPL","MCONJ","FCONJP"]), sig_estado=0}):
       (Elem_acc {estado=25,term="and",accion="reducir",regla=(PIzda "MDISY",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=25,term="or",accion="reducir",regla=(PIzda "MDISY",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=25,term="$",accion="reducir",regla=(PIzda "MDISY",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=25,term="coma",accion="reducir",regla=(PIzda "MDISY",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=25,term="=>",accion="reducir",regla=(PIzda "MDISY",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=25,term=")",accion="reducir",regla=(PIzda "MDISY",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=26,term="$",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=26,term="coma",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=26,term="=>",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=26,term=")",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=26,term="and",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=21}):
       (Elem_acc {estado=26,term="or",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=20}):
       (Elem_acc {estado=27,term="$",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["or","FIMPL","MDISY","FCONJP"]), sig_estado=0}):
       (Elem_acc {estado=27,term="coma",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["or","FIMPL","MDISY","FCONJP"]), sig_estado=0}):
       (Elem_acc {estado=27,term="=>",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["or","FIMPL","MDISY","FCONJP"]), sig_estado=0}):
       (Elem_acc {estado=27,term=")",accion="reducir",regla=(PIzda "FCONJP",Pdcha ["or","FIMPL","MDISY","FCONJP"]), sig_estado=0}):
       (Elem_acc {estado=28,term="and",accion="reducir",regla=(PIzda "FIMPL",Pdcha ["G","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=28,term="or",accion="reducir",regla=(PIzda "FIMPL",Pdcha ["G","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=28,term="$",accion="reducir",regla=(PIzda "FIMPL",Pdcha ["G","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=28,term="coma",accion="reducir",regla=(PIzda "FIMPL",Pdcha ["G","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=28,term="=>",accion="reducir",regla=(PIzda "FIMPL",Pdcha ["G","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=28,term=")",accion="reducir",regla=(PIzda "FIMPL",Pdcha ["G","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=29,term="neg",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=7}):
       (Elem_acc {estado=29,term="(",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=29,term="prop",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=9}):
       (Elem_acc {estado=30,term="neg",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=7}):
       (Elem_acc {estado=30,term="(",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=8}):
       (Elem_acc {estado=30,term="prop",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=9}):
       (Elem_acc {estado=31,term="impl",accion="reducir",regla=(PIzda "MIMPL",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=31,term="coimpl",accion="reducir",regla=(PIzda "MIMPL",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=31,term="and",accion="reducir",regla=(PIzda "MIMPL",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=31,term="or",accion="reducir",regla=(PIzda "MIMPL",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=31,term="$",accion="reducir",regla=(PIzda "MIMPL",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=31,term="coma",accion="reducir",regla=(PIzda "MIMPL",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=31,term="=>",accion="reducir",regla=(PIzda "MIMPL",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=31,term=")",accion="reducir",regla=(PIzda "MIMPL",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=32,term="and",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=32,term="or",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=32,term="$",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=32,term="coma",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=32,term="=>",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=32,term=")",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=32,term="impl",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=30}):
       (Elem_acc {estado=32,term="coimpl",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=29}):
       (Elem_acc {estado=33,term="and",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["impl","G","MIMPL","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=33,term="or",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["impl","G","MIMPL","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=33,term="$",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["impl","G","MIMPL","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=33,term="coma",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["impl","G","MIMPL","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=33,term="=>",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["impl","G","MIMPL","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=33,term=")",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["impl","G","MIMPL","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=34,term="impl",accion="reducir",regla=(PIzda "MCOIMP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=34,term="coimpl",accion="reducir",regla=(PIzda "MCOIMP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=34,term="and",accion="reducir",regla=(PIzda "MCOIMP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=34,term="or",accion="reducir",regla=(PIzda "MCOIMP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=34,term="$",accion="reducir",regla=(PIzda "MCOIMP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=34,term="coma",accion="reducir",regla=(PIzda "MCOIMP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=34,term="=>",accion="reducir",regla=(PIzda "MCOIMP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=34,term=")",accion="reducir",regla=(PIzda "MCOIMP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=35,term="and",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=35,term="or",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=35,term="$",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=35,term="coma",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=35,term="=>",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=35,term=")",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["lambda"]), sig_estado=0}):
       (Elem_acc {estado=35,term="impl",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=30}):
       (Elem_acc {estado=35,term="coimpl",accion="desplazar",regla=(PIzda "A",Pdcha ["lambda"]), sig_estado=29}):
       (Elem_acc {estado=36,term="and",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["coimpl","G","MCOIMP","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=36,term="or",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["coimpl","G","MCOIMP","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=36,term="$",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["coimpl","G","MCOIMP","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=36,term="coma",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["coimpl","G","MCOIMP","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=36,term="=>",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["coimpl","G","MCOIMP","FIMPLP"]), sig_estado=0}):
       (Elem_acc {estado=36,term=")",accion="reducir",regla=(PIzda "FIMPLP",Pdcha ["coimpl","G","MCOIMP","FIMPLP"]), sig_estado=0}):
       []
data Elem_ira =  Elem_ira {estado2::Int, var::String, sig_estado2::Int}
tabla_ira::[Elem_ira]
tabla_ira=(Elem_ira {estado2=1,var="A",sig_estado2=6}):
       (Elem_ira {estado2=1,var="P",sig_estado2=5}):
       (Elem_ira {estado2=1,var="FCONJ",sig_estado2=4}):
       (Elem_ira {estado2=1,var="FIMPL",sig_estado2=3}):
       (Elem_ira {estado2=1,var="G",sig_estado2=2}):
       (Elem_ira {estado2=2,var="FIMPLP",sig_estado2=28}):
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
       (Elem_ira {estado2=20,var="FIMPL",sig_estado2=25}):
       (Elem_ira {estado2=20,var="G",sig_estado2=2}):
       (Elem_ira {estado2=21,var="FIMPL",sig_estado2=22}):
       (Elem_ira {estado2=21,var="G",sig_estado2=2}):
       (Elem_ira {estado2=22,var="MCONJ",sig_estado2=23}):
       (Elem_ira {estado2=23,var="FCONJP",sig_estado2=24}):
       (Elem_ira {estado2=25,var="MDISY",sig_estado2=26}):
       (Elem_ira {estado2=26,var="FCONJP",sig_estado2=27}):
       (Elem_ira {estado2=29,var="G",sig_estado2=34}):
       (Elem_ira {estado2=30,var="G",sig_estado2=31}):
       (Elem_ira {estado2=31,var="MIMPL",sig_estado2=32}):
       (Elem_ira {estado2=32,var="FIMPLP",sig_estado2=33}):
       (Elem_ira {estado2=34,var="MCOIMP",sig_estado2=35}):
       (Elem_ira {estado2=35,var="FIMPLP",sig_estado2=36}):
       []
axioma::String
axioma="PA"
terminales::[String]
terminales = ["prop","(",")","coma","=>","and","or","impl","coimpl","neg"]
buscaSigToken::String->String->(String,String)
buscaSigToken [] "" =("$","")
buscaSigToken [] y =("prop",y)
buscaSigToken x y |((head x)=='\n'||(head x)==' ') && (trim y)==""=(buscaSigToken  (tail x) "")
			  |(head x)==',' && (trim y)==""=("coma",(substr x 1 1))
			  |(head x)=='~' && (trim y)==""=("neg",(substr x 1 1))
			  |(head x)=='^' && (trim y)==""=("and",(substr x 1 1))
			  |(head x)=='v' && (trim y)==""=("or",(substr x 1 1))
			  |(head x)=='(' && (trim y)==""=("(",(substr x 1 1))
			  |(head x)==')' && (trim y)==""=(")",(substr x 1 1))
			  |(substr x 1 2)=="->" && (trim y)==""=("impl","->")
			  |(substr x 1 3)=="<->" && (trim y)==""=("coimpl","<->")
			  |(substr x 1 2)=="=>" && (trim y)==""=("=>","=>")
			  |ord(head x)>=ord('a') && ord(head x)<=ord('z')=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
                          |otherwise = ("prop",y)
sigToken::String->[(String,String)]
sigToken [] = [("$","")]
sigToken x = (buscaSigToken (trim x) ""):(sigToken (substr (trim x) ((length (snd (buscaSigToken (trim x) "")))+1) (length x)))
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
                               |(busca_accion tabla_acc y (fst x))=="reducir"  && (busca_ira tabla_ira (head (dropInt (if((reduce_por tabla_acc y (fst x))!!0=="lambda") then 0 else (length (reduce_por tabla_acc y (fst x)))) (y:ys))) (reduce_a tabla_acc y (fst x)))>0  = (visualizar_pila (y:ys))++"      "++(reduce_a tabla_acc y (fst x))++"->"++(concat (reduce_por tabla_acc y (fst x)))++"      "++(visualizar_resto_entrada (x:xs))++"\n"++
			                                                                                                                                                                                                                                                                                                                                      parser_slr (x:xs) ((busca_ira tabla_ira (head (dropInt (if((reduce_por tabla_acc y (fst x))!!0=="lambda") then 0 else (length (reduce_por tabla_acc y (fst x)))) (y:ys))) (reduce_a tabla_acc y (fst x))):
																																											      (if((reduce_por tabla_acc y (fst x))!!0=="lambda") then (y:ys) else (dropInt (length (reduce_por tabla_acc y (fst x))) (y:ys))))
			       |(busca_accion tabla_acc y (fst x))=="reducir"    = (visualizar_pila (y:ys))++"      "++(reduce_a tabla_acc y (fst x))++"->"++(concat (reduce_por tabla_acc y (fst x)))++"      "++(visualizar_resto_entrada (x:xs))++"\n"++ "falta entrada en la tabla ira, estado="++show (head (dropInt (if((reduce_por tabla_acc y (fst x))!!0=="lambda") then 0 else (length (reduce_por tabla_acc y (fst x)))) (y:ys)))++",variable="++(reduce_a tabla_acc y (fst x))
			       |(busca_accion tabla_acc y (fst x))=="aceptar"   = "entrada correcta"
			       |(busca_accion tabla_acc y (fst x))=="error"       = (visualizar_pila (y:ys))++"      "++(visualizar_resto_entrada (x:xs))++"\n"++"falta entrada en la tabla accion, estado="++(show y)++", terminal="++(fst x)
data Arbolprop = Hoja String| Rama  String (Arbolprop) (Arbolprop)| Ramaun String (Arbolprop)
visualizar_prop::[Arbolprop]->String
visualizar_prop []=""
visualizar_prop (x:xs)=(visualizar_arbol x)++","++(visualizar_prop xs)
-- realiza el analisis sintactico de la entrada pero en esta ocasion ejecutando las acciones semanticas para la gramatica, conducentes a obtener las estructuras de datos (en forma de arbol) de las formulas leidas
obtener_prop::[(String,String)]->[Int]->[Arbolprop]->(String,String)->[Arbolprop]
obtener_prop (x:xs) (y:ys) pila_sem ult_token|(busca_accion tabla_acc y (fst x))=="desplazar" = obtener_prop xs ((desplazar_a tabla_acc y (fst x)):y:ys) pila_sem x
							|(busca_accion tabla_acc y (fst x))=="reducir"  = obtener_prop (x:xs) ((busca_ira tabla_ira (head (dropInt (if((reduce_por tabla_acc y (fst x))!!0=="lambda") then 0 else (length (reduce_por tabla_acc y (fst x)))) (y:ys))) (reduce_a tabla_acc y (fst x))):
														    (if((reduce_por tabla_acc y (fst x))!!0=="lambda") then (y:ys) else (dropInt (length (reduce_por tabla_acc y (fst x))) (y:ys))))
														    (if(reduce_a tabla_acc y (fst x))=="MIMPL" then (Rama "->" (head (tail pila_sem)) (head pila_sem)):(dropInt 2 pila_sem) else
														     if(reduce_a tabla_acc y (fst x))=="G" && (reduce_por tabla_acc y (fst x))!!0=="prop" then (Hoja (snd ult_token)):pila_sem else
														     if(reduce_a tabla_acc y (fst x))=="MCONJ" then (Rama "^" (head (tail pila_sem)) (head pila_sem)):(dropInt 2 pila_sem) else
														     if(reduce_a tabla_acc y (fst x))=="MDISY" then (Rama "v" (head (tail pila_sem)) (head pila_sem)):(dropInt 2 pila_sem) else
														     if(reduce_a tabla_acc y (fst x))=="G" && (reduce_por tabla_acc y (fst x))!!0=="neg" then (Ramaun "~" (head pila_sem)):(dropInt 1 pila_sem) else
														     if(reduce_a tabla_acc y (fst x))=="G" && (reduce_por tabla_acc y (fst x))!!0=="(" then (Ramaun "()" (head pila_sem)):(dropInt 1 pila_sem) else
														     if(reduce_a tabla_acc y (fst x))=="MCOIMP" then (Rama "<->" (head (tail pila_sem)) (head pila_sem)):(dropInt 2 pila_sem) else
														     pila_sem) ult_token
			                                |(busca_accion tabla_acc y (fst x))=="aceptar"   =pila_sem
-- obtiene la formula equivalente pero solo con las conectivas de implicacion y de negacion							
convertir_a_impl::Arbolprop->Arbolprop
convertir_a_impl (Rama "v" (Ramaun "~" x) y) = (Rama "->" (convertir_a_impl x) (convertir_a_impl y))
convertir_a_impl (Rama "v" x y) = (Rama "->" (Ramaun "~" (convertir_a_impl x)) (convertir_a_impl y))
convertir_a_impl (Rama "^" x y) = (Ramaun "~" (Ramaun "()" (Rama "->" (convertir_a_impl x) (Ramaun "~" (convertir_a_impl y)))))
convertir_a_impl (Rama "<->" x y) = (Ramaun "~" (Ramaun "()" (Rama "->" (Rama "->" (convertir_a_impl x) (convertir_a_impl y)) (Ramaun "~" (Rama "->" (convertir_a_impl y) (convertir_a_impl x))))))
convertir_a_impl (Rama e x y) = (Rama e (convertir_a_impl x) (convertir_a_impl y))
convertir_a_impl (Ramaun e x) = (Ramaun e (convertir_a_impl x))
convertir_a_impl (Hoja x) = (Hoja x)
-- pone todas las formulas como del sistema L
pasar_a_L::[Arbolprop]->[Arbolprop]
pasar_a_L []=[]
pasar_a_L (x:xs)=(convertir_a_impl x):(pasar_a_L xs)
-- compara si son iguales dos formulas
igual::Arbolprop->Arbolprop->Bool
igual (Hoja x1) (Ramaun "()" (Hoja x2)) = x1==x2
igual (Ramaun "()" (Hoja x1)) (Hoja x2) = x1==x2
igual (Rama et1 x1 y1) (Ramaun "()" (Rama et2 x2 y2)) = et1==et2 && (igual x1 x2) && (igual y1 y2)
igual (Ramaun "()" (Rama et1 x1 y1)) (Rama et2 x2 y2) = et1==et2 && (igual x1 x2) && (igual y1 y2)
igual (Rama et1 x1 y1) (Rama et2 x2 y2) = et1==et2 && (igual x1 x2) && (igual y1 y2)
igual (Ramaun et1 x1) (Ramaun et2 x2) = et1==et2 && (igual x1 x2)
igual (Hoja x1) (Hoja x2)=x1==x2
igual _ _ = False
-- ver si hay alguna entre las formulas del segundo parametro que es igual a la formula del primer parametro (que es el antecedente de una implicacion) y en caso de no encontrarse ninguno
-- ver si es igual a alguno de los axiomas o teoremas del sistema
obtener_antecedente::Arbolprop->[(Arbolprop,(String,[Arbolprop]))]->(String,[Arbolprop])
obtener_antecedente p (x:xs)|(igual (quitar_parentesis p) (quitar_parentesis (fst x))) = ("M",[p])
                                        |otherwise = obtener_antecedente p xs
obtener_antecedente (Rama "->" p1 (Rama "->" p2 p3)) []|igual p1 p3 = ("A1",[p1,p2])
obtener_antecedente (Rama "->" p1 (Ramaun "()" (Rama "->" p2 p3))) []|igual p1 p3 = ("A1",[p1,p2])
obtener_antecedente (Rama "->" (Ramaun "()" (Rama "->" p1 (Ramaun "()" (Rama "->" p2 p3)))) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p4 p5)) (Ramaun "()" (Rama "->" p6 p7))))) []|(igual p1 p4)&&(igual p1 p6)&&(igual p2 p5)&&(igual p3 p7) = ("A2",[p1,p2,p3])
obtener_antecedente (Rama "->" (Rama "->" p1 (Ramaun "()" (Rama "->" p2 p3))) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p4 p5)) (Ramaun "()" (Rama "->" p6 p7))))) []|(igual p1 p4)&&(igual p1 p6)&&(igual p2 p5)&&(igual p3 p7) = ("A2",[p1,p2,p3])
obtener_antecedente (Rama "->" (Ramaun "()"(Rama "->" (Ramaun "~" p1) (Ramaun "~" p2))) (Ramaun "()" (Rama "->" p3 p4))) []|(igual p1 p4)&&(igual p2 p3)=("A3",[p1,p2])
obtener_antecedente (Rama "->" (Ramaun "()"(Rama "->" p1 p2)) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p3 p4)) (Ramaun "()" (Rama "->" p5 p6))))) []|(igual p2 p3)&&(igual p1 p5)&&(igual p4 p6)=("T2",[p1,p2,p4])
obtener_antecedente _ [] = ("",[])
visualizar_arbol::Arbolprop->String
visualizar_arbol (Hoja x) = x
visualizar_arbol (Rama  x li ld)=(visualizar_arbol li)++x++(visualizar_arbol ld)
visualizar_arbol (Ramaun  x li)|x=="()"="("++(visualizar_arbol li)++")"
                                       |otherwise = x++(visualizar_arbol li)
miembro::(Arbolprop,(String,[Arbolprop]))->[(Arbolprop,(String,[Arbolprop]))]->Bool
miembro (p1,(s1,sust1)) ((p2,(s2,sust2)):xs)|(igual p1 p2) && s1==s2 = True
                                                            |otherwise = miembro (p1,(s1,sust1)) xs
miembro (p1,(s1,sust1)) [] = False	
find::Arbolprop->[Arbolprop]->Bool
find x (y:ys)|(igual (quitar_parentesis x) (quitar_parentesis y)) = True
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
-- buscar obtener la conclusion del tercer parametro c, aplicando el modus ponens entre las formulas existentes, e introduciendo los axiomas y teoremas del sistema L
buscar_implicaciones::[Arbolprop]->[(Arbolprop,(String,[Arbolprop]))]->Arbolprop->[(Arbolprop,(String,[Arbolprop]))]
buscar_implicaciones ((Rama "->" x y):xs) lc c|(fst(obtener_antecedente x lc)/="") && not(miembro (y,("MP("++show (posicion (Rama "->" x y) (reverse lc) 1)++")("++show (posicion x (reverse lc) 1)++")",snd(obtener_antecedente x lc))) lc)= buscar_implicaciones (y:(Rama "->" x y):xs) ([(y,("MP("++show (posicion (Rama "->" x y) (reverse lc) 1)++")("++show (posicion x (reverse lc) 1)++")",snd(obtener_antecedente x lc)))]++lc) c
buscar_implicaciones ((Ramaun "()" (Rama "->" x y)):xs) lc c|(fst(obtener_antecedente x lc)/="") && not(miembro (y,("MP("++show (posicion (Ramaun "()" (Rama "->" x y)) (reverse lc) 1)++")("++show (posicion x (reverse lc) 1)++")",snd(obtener_antecedente x lc))) lc)= buscar_implicaciones (y:(Rama "->" x y):xs) ([(y,("MP("++show (posicion (Ramaun "()" (Rama "->" x y)) (reverse lc) 1)++")("++show (posicion x (reverse lc) 1)++")",snd(obtener_antecedente x lc)))]++lc) c
buscar_implicaciones (x:xs) lc c|(igual (quitar_parentesis x) (quitar_parentesis c))= reverse lc
buscar_implicaciones (x:xs) lc c|fst (snd (buscar_aplicar_t2 (quitar_parentesis x) (resto_formulas x lc)))/="" && not(miembro (buscar_aplicar_t2 (quitar_parentesis x) (resto_formulas x lc)) lc)=buscar_implicaciones ((fst(buscar_aplicar_t2 (quitar_parentesis x) (resto_formulas x lc))):x:xs) ([(buscar_aplicar_t2 (quitar_parentesis x) (resto_formulas x lc))]++lc) c
buscar_implicaciones (x:xs) lc c|fst (snd (buscar_aplicar_t3 x (resto_formulas x lc)))/="" && not(miembro (buscar_aplicar_t3 x (resto_formulas x lc)) lc)=buscar_implicaciones ((fst(buscar_aplicar_t3 x (resto_formulas x lc))):x:xs) ([(buscar_aplicar_t3 x (resto_formulas x lc))]++lc) c
buscar_implicaciones (x:xs) lc c|fst (snd (buscar_aplicar_tmp x (resto_formulas x lc)))/="" && not(miembro (buscar_aplicar_tmp x (resto_formulas x lc)) lc)=buscar_implicaciones ((fst(buscar_aplicar_tmp x (resto_formulas x lc))):x:xs) ([(buscar_aplicar_tmp x (resto_formulas x lc))]++lc) c
buscar_implicaciones (x:xs) lc c|fst (snd (buscar_aplicar_t4 x (resto_formulas x lc) c))/="" && not(miembro (buscar_aplicar_t4 x (resto_formulas x lc) c) lc)=buscar_implicaciones ((fst(buscar_aplicar_t4 x (resto_formulas x lc) c)):x:xs) ([(buscar_aplicar_t4 x (resto_formulas x lc) c)]++lc) c
buscar_implicaciones (x:xs) lc c|fst (snd (buscar_aplicar_t5 x (resto_formulas x lc) c))/="" && not(miembro (buscar_aplicar_t5 x (resto_formulas x lc) c) lc)=buscar_implicaciones ((fst(buscar_aplicar_t5 x (resto_formulas x lc) c)):x:xs) ([(buscar_aplicar_t5 x (resto_formulas x lc) c)]++lc) c
buscar_implicaciones (x:xs) lc c|fst (snd (buscar_aplicar_t8 x (resto_formulas x lc) c))/="" && not(miembro (buscar_aplicar_t8 x (resto_formulas x lc) c) lc)=buscar_implicaciones ((fst(buscar_aplicar_t8 x (resto_formulas x lc) c)):x:xs) ([(buscar_aplicar_t8 x (resto_formulas x lc) c)]++lc) c
buscar_implicaciones (x:xs) lc c|fst (snd (buscar_aplicar_A1 x (resto_formulas x lc) c))/="" && not(miembro (buscar_aplicar_A1 x (resto_formulas x lc) c) lc)=buscar_implicaciones ((fst(buscar_aplicar_A1 x (resto_formulas x lc) c)):x:xs) ([(buscar_aplicar_A1 x (resto_formulas x lc) c)]++lc) c
buscar_implicaciones (x:xs) lc c|fst (snd (buscar_aplicar_A2 (quitar_parentesis x) (resto_formulas x lc) c))/="" && not(miembro (buscar_aplicar_A2 (quitar_parentesis x) (resto_formulas x lc) c) lc)=buscar_implicaciones ((fst(buscar_aplicar_A2 (quitar_parentesis x) (resto_formulas x lc) c)):x:xs) ([(buscar_aplicar_A2 (quitar_parentesis x) (resto_formulas x lc) c)]++lc) c
buscar_implicaciones (x:xs) lc c|fst (snd (buscar_aplicar_A3 (quitar_parentesis x) (resto_formulas x lc) (quitar_parentesis c)))/="" && not(miembro (buscar_aplicar_A3 (quitar_parentesis x) (resto_formulas x lc) (quitar_parentesis c)) lc)=buscar_implicaciones ((fst(buscar_aplicar_A3 (quitar_parentesis x) (resto_formulas x lc) (quitar_parentesis c))):x:xs) ([(buscar_aplicar_A3 (quitar_parentesis x) (resto_formulas x lc) (quitar_parentesis c))]++lc) c
buscar_implicaciones (x:xs) lc c= buscar_implicaciones xs lc c
buscar_implicaciones [] lc _= reverse lc
-- ver si hay dos formulas que sean antecedentes del T2 (A->B)->((B->C)->(A->C)), es decir buscar (A->B) y (B->C) en cuyo caso se a�ade T2 al proceso de demostracion
buscar_aplicar_t2::Arbolprop->[Arbolprop]->(Arbolprop,(String,[Arbolprop]))
buscar_aplicar_t2 (Rama "->" p1 p2) ((Ramaun "()" (Rama "->" p3 p4)):xs)|(igual p2 p3)=((Rama "->" (Ramaun "()" (Rama "->" p1 p2)) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p3 p4)) (Ramaun "()" (Rama "->" p1 p4))))),("T2",[p1,p2,p4]))
buscar_aplicar_t2 (Rama "->" p1 p2) ((Rama "->" p3 p4):xs)|(igual p2 p3)=((Rama "->" (Ramaun "()" (Rama "->" p1 p2)) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p3 p4)) (Ramaun "()" (Rama "->" p1 p4))))),("T2",[p1,p2,p4]))
buscar_aplicar_t2 (Rama "->" p1 p2) ((Ramaun "()" (Rama "->" p3 p4)):xs)|(igual p4 p1)=((Rama "->" (Ramaun "()" (Rama "->" p3 p4)) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p1 p2)) (Ramaun "()" (Rama "->" p3 p2))))),("T2",[p3,p4,p2]))
buscar_aplicar_t2 (Rama "->" p1 p2) ((Rama "->" p3 p4):xs)|(igual p4 p1)=((Rama "->" (Ramaun "()" (Rama "->" p3 p4)) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p1 p2)) (Ramaun "()" (Rama "->" p3 p2))))),("T2",[p3,p4,p2]))
buscar_aplicar_t2 p (x:xs)= buscar_aplicar_t2 p xs
buscar_aplicar_t2 p [] = ((Hoja ""),("",[]))
-- ver si hay dos formulas que sean antecedentes del T3 (A->B)->((A->(B->C))->(A->C)), es decir buscar (A->B) y (A->(B->C)) en cuyo caso se a�ade T3 al proceso de demostracion
buscar_aplicar_t3::Arbolprop->[Arbolprop]->(Arbolprop,(String,[Arbolprop]))
buscar_aplicar_t3 (Ramaun "()" (Rama "->" p1 p2)) ((Ramaun "()" (Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5)))):xs)|(igual p1 p3)&&(igual p2 p4)=((Rama "->" (Ramaun "()" (Rama "->" p1 p2)) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5)))) (Ramaun "()" (Rama "->" p1 p5))))),("T3",[p1,p2,p3]))
															                                      |otherwise = (buscar_aplicar_t3 (Ramaun "()" (Rama "->" p1 p2)) xs)
buscar_aplicar_t3 (Ramaun "()" (Rama "->" p1 p2)) ((Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5))):xs)|(igual p1 p3)&&(igual p2 p4)=((Rama "->" (Ramaun "()" (Rama "->" p1 p2)) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5)))) (Ramaun "()" (Rama "->" p1 p5))))),("T3",[p1,p2,p3]))
															                   |otherwise = (buscar_aplicar_t3 (Ramaun "()" (Rama "->" p1 p2)) xs)
buscar_aplicar_t3 (Rama "->" p1 p2) ((Ramaun "()" (Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5)))):xs)|(igual p1 p3)&&(igual p2 p4)=((Rama "->" (Ramaun "()" (Rama "->" p1 p2)) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5)))) (Ramaun "()" (Rama "->" p1 p5))))),("T3",[p1,p2,p3]))
															                   |otherwise = (buscar_aplicar_t3 (Ramaun "()" (Rama "->" p1 p2)) xs)
buscar_aplicar_t3 (Rama "->" p1 p2) ((Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5))):xs)|(igual p1 p3)&&(igual p2 p4)=((Rama "->" (Ramaun "()" (Rama "->" p1 p2)) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5)))) (Ramaun "()" (Rama "->" p1 p5))))),("T3",[p1,p2,p3]))
															|otherwise = (buscar_aplicar_t3 (Ramaun "()" (Rama "->" p1 p2)) xs)
--buscar_aplicar_t3 (Ramaun "~" p) xs = buscar_aplicar_t3 (head xs) (tail xs)
--buscar_aplicar_t3 (Ramaun "()" (Hoja _)) xs = buscar_aplicar_t3 (head xs) (tail xs)
--buscar_aplicar_t3 (Ramaun "()" (Ramaun _ _)) xs = buscar_aplicar_t3 (head xs) (tail xs)
--buscar_aplicar_t3 (Hoja p) xs = buscar_aplicar_t3 (head xs) (tail xs)
buscar_aplicar_t3 (Ramaun "()" (Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5)))) ((Ramaun "()" (Rama "->" p1 p2)):xs)|(igual p1 p3)&&(igual p2 p4)=((Rama "->" (Ramaun "()" (Rama "->" p1 p2)) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5)))) (Ramaun "()" (Rama "->" p1 p5))))),("T3",[p1,p2,p3]))
															                                      |otherwise = (buscar_aplicar_t3 (Ramaun "()" (Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5)))) xs)
buscar_aplicar_t3 (Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5))) ((Ramaun "()" (Rama "->" p1 p2)):xs)|(igual p1 p3)&&(igual p2 p4)=((Rama "->" (Ramaun "()" (Rama "->" p1 p2)) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5)))) (Ramaun "()" (Rama "->" p1 p5))))),("T3",[p1,p2,p3]))
															                   |otherwise = (buscar_aplicar_t3 (Ramaun "()" (Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5)))) xs)
buscar_aplicar_t3 (Ramaun "()" (Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5)))) ((Rama "->" p1 p2):xs)|(igual p1 p3)&&(igual p2 p4)=((Rama "->" (Ramaun "()" (Rama "->" p1 p2)) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5)))) (Ramaun "()" (Rama "->" p1 p5))))),("T3",[p1,p2,p3]))
															                   |otherwise = (buscar_aplicar_t3 (Ramaun "()" (Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5)))) xs)
buscar_aplicar_t3 (Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5))) ((Rama "->" p1 p2):xs)|(igual p1 p3)&&(igual p2 p4)=((Rama "->" (Ramaun "()" (Rama "->" p1 p2)) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5)))) (Ramaun "()" (Rama "->" p1 p5))))),("T3",[p1,p2,p3]))
															|otherwise = (buscar_aplicar_t3 (Ramaun "()" (Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5)))) xs)
buscar_aplicar_t3 p (x:xs)= buscar_aplicar_t3 p xs
buscar_aplicar_t3 p [] = ((Hoja ""),("",[]))
-- dada una formula x puedo querer utilizar el A1, tal es el caso cuando la conclusion esta formada por una implicacion y el consecuente es igual a x, la variable B en el axioma habria que 
-- sustituirla por el antecedente de la conclusion, otro caso para utilizar el axioma seria si hay alguna formula con antecedente igual a una implicacion que tuviese como consecuente x 
-- la B se sustituiria por el antecedente de dicha implicacion
buscar_aplicar_A1::Arbolprop->[Arbolprop]->Arbolprop->(Arbolprop,(String,[Arbolprop]))
buscar_aplicar_A1 x _ (Rama "->" p1 p2)|(igual x p2)=((Rama "->" x (Ramaun "()" (Rama "->" p1 x))),("A3",[x,p1]))
buscar_aplicar_A1 x ((Rama "->" (Ramaun "()" (Rama "->" p1 p2)) p3):xs) _ |(igual x p2) = ((Rama "->" x (Ramaun "()" (Rama "->" p1 x))),("A3",[x,p1]))
buscar_aplicar_A1 x (p:xs) c = buscar_aplicar_A1 x xs c
buscar_aplicar_A1 x [] _ = ((Hoja ""),("",[]))
-- aplicar el T8 (~A->B)->(~B->A) siempre y cuando la conclusion sea de la forma (~B->A)  y tengamos otra que sea igual a A o tengamos una formula (~A->B) y otra cuyo antecedente de una implicacion sea (~B->A)
buscar_aplicar_t8::Arbolprop->[Arbolprop]->Arbolprop->(Arbolprop,(String,[Arbolprop]))
buscar_aplicar_t8 x _ (Rama "->" (Ramaun "~" p1) p2)|(igual x p1)=((Rama "->" (Ramaun "()" (Rama "->" (Ramaun "~" p2) p1)) (Ramaun "()" (Rama "->"  (Ramaun "~" p1) p2))),("T8",[x,p1]))
buscar_aplicar_t8 (Rama "->" (Ramaun "~" p1) p2) ((Rama "->" (Ramaun "()" (Rama "->" (Ramaun "~" p3) p4)) p5):xs) _|(igual p1 p4) && (igual p2 p3)=((Rama "->" (Ramaun "()" (Rama "->" (Ramaun "~" p1) p2)) (Ramaun "()" (Rama "->"  (Ramaun "~" p2) p1))),("T8",[p1,p2]))                                                                                                 
buscar_aplicar_t8 x (p:xs) c = buscar_aplicar_t8 x xs c
buscar_aplicar_t8 x [] _ = ((Hoja ""),("",[]))
-- siempre que aparezca una formula de la forma A->(B->C) es posible utilizar el A2, seria beneficioso si (A->B)->(A->C) (*) fuese la conclusion o igual que se hizo con el A1 hubiese otra formula
-- que fuese una implicacion cuyo antecedente sea igual a (*) o exista la formula A->B y (A->C es la conclusion o exista otra formula en forma de implicacion cuyo antecedente sea igual a A->C)
-- el consecuente del A2 va a ser la conculsion?
buscar_aplicar_A2::Arbolprop->[Arbolprop]->Arbolprop->(Arbolprop,(String,[Arbolprop]))
buscar_aplicar_A2 (Rama "->" p1 (Ramaun "()" (Rama "->" p2 p3))) _ (Rama "->" (Ramaun "()" (Rama "->" p4 p5)) (Ramaun "()" (Rama "->" p6 p7)))|(igual p1 p4)&&(igual p1 p6)&&(igual p2 p5)&&(igual p3 p7)=
                                                                                                                     ((Rama "->" (Ramaun "()" (Rama "->" p1 (Ramaun "()" (Rama "->" p2 p3)))) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p1 p2)) (Ramaun "()" (Rama "->" p1 p3))))),("A2",[p1,p2,p3]))
-- hay un formula cuyo antecedente es el consecuente de A2?														     
buscar_aplicar_A2 (Rama "->" p1 (Ramaun "()" (Rama "->" p2 p3))) ((Rama "->" (Rama "->" (Ramaun "()" (Rama "->" p4 p5)) (Ramaun "()" (Rama "->" p6 p7))) p8):xs) c|(igual p1 p4)&&(igual p1 p6)&&(igual p2 p5)&&(igual p3 p7)=
                                                                                                                     ((Rama "->" (Ramaun "()" (Rama "->" p1 (Ramaun "()" (Rama "->" p2 p3)))) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p1 p2)) (Ramaun "()" (Rama "->" p1 p3))))),("A2",[p1,p2,p3]))
buscar_aplicar_A2 (Rama "->" p1 (Ramaun "()" (Rama "->" p2 p3))) ((Rama "->" (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p4 p5)) (Ramaun "()" (Rama "->" p6 p7)))) p8):xs) c|(igual p1 p4)&&(igual p1 p6)&&(igual p2 p5)&&(igual p3 p7)=
                                                                                                                     ((Rama "->" (Ramaun "()" (Rama "->" p1 (Ramaun "()" (Rama "->" p2 p3)))) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p1 p2)) (Ramaun "()" (Rama "->" p1 p3))))),("A2",[p1,p2,p3]))
-- la conclusion es la 2a parte del consecuente de A2 y existe otra formula igual a la primera parte?														     
buscar_aplicar_A2 (Rama "->" p1 (Ramaun "()" (Rama "->" p2 p3))) ((Ramaun "()" (Rama "->" p4 p5)):xs) (Ramaun "()" (Rama "->" p6 p7))|(igual p1 p4)&&(igual p1 p6)&&(igual p2 p5)&&(igual p3 p7)=	
                                                                                                                     ((Rama "->" (Ramaun "()" (Rama "->" p1 (Ramaun "()" (Rama "->" p2 p3)))) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p1 p2)) (Ramaun "()" (Rama "->" p1 p3))))),("A2",[p1,p2,p3]))
--existe otra formula igual a la primera parte del consecuente de A2 y la segunda parte aparece como antecedente de otra formula
buscar_aplicar_A2 (Rama "->" p1 (Ramaun "()" (Rama "->" p2 p3))) ((Ramaun "()" (Rama "->" p4 p5)):(Rama "->" (Ramaun "()" (Rama "->" p6 p7)) p8):xs) _|(igual p1 p4)&&(igual p1 p6)&&(igual p2 p5)&&(igual p3 p7)=	
                                                                                                                     ((Rama "->" (Ramaun "()" (Rama "->" p1 (Ramaun "()" (Rama "->" p2 p3)))) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p1 p2)) (Ramaun "()" (Rama "->" p1 p3))))),("A2",[p1,p2,p3]))
buscar_aplicar_A2 (Rama "->" p1 (Ramaun "()" (Rama "->" p2 p3))) ((Rama "->" (Ramaun "()" (Rama "->" p6 p7)) p8):(Ramaun "()" (Rama "->" p4 p5)):xs) _|(igual p1 p4)&&(igual p1 p6)&&(igual p2 p5)&&(igual p3 p7)=	
                                                                                                                     ((Rama "->" (Ramaun "()" (Rama "->" p1 (Ramaun "()" (Rama "->" p2 p3)))) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p1 p2)) (Ramaun "()" (Rama "->" p1 p3))))),("A2",[p1,p2,p3]))
buscar_aplicar_A2 (Rama "->" p1 (Ramaun "()" (Rama "->" p2 p3))) ((Ramaun "()" (Rama "->" p4 p5)):p:xs) c|(igual p1 p4)&&(igual p2 p5) = buscar_aplicar_A2 (Rama "->" p1 (Ramaun "()" (Rama "->" p2 p3))) ((Ramaun "()" (Rama "->" p4 p5)):xs) c
buscar_aplicar_A2 (Rama "->" p1 (Ramaun "()" (Rama "->" p2 p3))) ((Rama "->" (Ramaun "()" (Rama "->" p6 p7)) p8):p:xs) c|(igual p1 p6)&&(igual p3 p7) = buscar_aplicar_A2 (Rama "->" p1 (Ramaun "()" (Rama "->" p2 p3))) ((Rama "->" (Ramaun "()" (Rama "->" p6 p7)) p8):xs) c
buscar_aplicar_A2 x (p:xs) c = buscar_aplicar_A2 x xs c
buscar_aplicar_A2 x [] _ = ((Hoja ""),("",[]))
buscar_aplicar_A3::Arbolprop->[Arbolprop]->Arbolprop->(Arbolprop,(String,[Arbolprop]))
buscar_aplicar_A3 (Rama "->" (Ramaun "~" p1) (Ramaun "~" p2)) _ (Rama "->" p3 p4)|(igual p1 p4)&&(igual p2 p3) = ((Rama "->" (Ramaun "()" (Rama "->" (Ramaun "~" p1) (Ramaun "~" p2))) (Ramaun "()" (Rama "->" p2 p1))),("A3",[p1,p2]))
buscar_aplicar_A3 (Rama "->" (Ramaun "~" p1) (Ramaun "~" p2)) ((Rama "->" (Rama "->" p3 p4) p5):xs) _|(igual p1 p4)&&(igual p2 p3) = ((Rama "->" (Ramaun "()" (Rama "->" (Ramaun "~" p1) (Ramaun "~" p2))) (Ramaun "()" (Rama "->" p2 p1))),("A3",[p1,p2]))
buscar_aplicar_A3 (Rama "->" (Ramaun "~" p1) (Ramaun "~" p2)) ((Rama "->" (Ramaun "()" (Rama "->" p3 p4)) p5):xs) _|(igual p1 p4)&&(igual p2 p3) = ((Rama "->" (Ramaun "()" (Rama "->" (Ramaun "~" p1) (Ramaun "~" p2))) (Ramaun "()" (Rama "->" p2 p1))),("A3",[p1,p2]))
buscar_aplicar_A3 (Rama "->" (Ramaun "~" p1) (Ramaun "~" p2)) ((Rama "->"  p5 (Ramaun "()" (Rama "->" p3 p4))):xs) _|(igual p1 p4)&&(igual p2 p3) = ((Rama "->" (Ramaun "()" (Rama "->" (Ramaun "~" p1) (Ramaun "~" p2))) (Ramaun "()" (Rama "->" p2 p1))),("A3",[p1,p2]))
buscar_aplicar_A3 x (p:xs) c = buscar_aplicar_A3 x xs c
buscar_aplicar_A3 x [] _ = ((Hoja ""),("",[]))
-- eliminacion de la doble negacion, la utilizamos si existe una formula que esta doblemente negada y existe otra formula igual sin negar
buscar_aplicar_t4::Arbolprop->[Arbolprop]->Arbolprop->(Arbolprop,(String,[Arbolprop]))
buscar_aplicar_t4 (Ramaun "~" (Ramaun "~" p1)) _ p2|(igual (quitar_parentesis p1) (quitar_parentesis p2))=((Rama "->" (Ramaun "~" (Ramaun "~" p1)) p2),("T4",[p1]))
buscar_aplicar_t4 (Ramaun "~" (Ramaun "~" p1)) (p2:xs) _|(igual (quitar_parentesis p1) (quitar_parentesis p2))=((Rama "->" (Ramaun "~" (Ramaun "~" p1)) p2),("T4",[p1]))
buscar_aplicar_t4 x (p:xs) c = buscar_aplicar_t4 x xs c
buscar_aplicar_t4 x [] _ = ((Hoja ""),("",[]))
-- contraposicion
buscar_aplicar_t5::Arbolprop->[Arbolprop]->Arbolprop->(Arbolprop,(String,[Arbolprop]))
buscar_aplicar_t5 (Rama "->" p1 (Ramaun "~" p2)) _ (Rama "->" p3 (Ramaun "~" p4))|(igual p1 p4)&&(igual p2 p3)=((Rama "->" (Ramaun "()" (Rama "->" p1 (Ramaun "~" p2))) (Ramaun "()" (Rama "->" p2 (Ramaun "~" p1)))),("T5",[p1,p2]))
buscar_aplicar_t5 (Rama "->" p1 (Ramaun "~" p2)) ((Rama "->" (Rama "->" p3 (Ramaun "~" p4)) p5):xs) _|(igual p1 p4)&&(igual p2 p3)=((Rama "->" (Ramaun "()" (Rama "->" p1 (Ramaun "~" p2))) (Ramaun "()" (Rama "->" p2 (Ramaun "~" p1)))),("T5",[p1,p2]))
buscar_aplicar_t5 (Rama "->" p1 (Ramaun "~" p2)) ((Rama "->" (Ramaun "()" (Rama "->" p3 (Ramaun "~" p4))) p5):xs) _|(igual p1 p4)&&(igual p2 p3)=((Rama "->" (Ramaun "()" (Rama "->" p1 (Ramaun "~" p2))) (Ramaun "()" (Rama "->" p2 (Ramaun "~" p1)))),("T5",[p1,p2]))
buscar_aplicar_t5 (Rama "->" p1 (Ramaun "~" p2)) ((Rama "->" p5 (Rama "->" p3 (Ramaun "~" p4))):xs) _|(igual p1 p4)&&(igual p2 p3)=((Rama "->" (Ramaun "()" (Rama "->" p1 (Ramaun "~" p2))) (Ramaun "()" (Rama "->" p2 (Ramaun "~" p1)))),("T5",[p1,p2]))
buscar_aplicar_t5 (Rama "->" p1 (Ramaun "~" p2)) ((Rama "->" p5 (Ramaun "()" (Rama "->" p3 (Ramaun "~" p4)))):xs) _|(igual p1 p4)&&(igual p2 p3)=((Rama "->" (Ramaun "()" (Rama "->" p1 (Ramaun "~" p2))) (Ramaun "()" (Rama "->" p2 (Ramaun "~" p1)))),("T5",[p1,p2]))
buscar_aplicar_t5 (Rama "->" p1 (Ramaun "~" p2)) ((Rama "->" p3 p4):xs) _|(igual p2 p4)=((Rama "->" (Ramaun "()" (Rama "->" p1 (Ramaun "~" p2))) (Ramaun "()" (Rama "->" p2 (Ramaun "~" p1)))),("T5",[p1,p2]))
buscar_aplicar_t5 (Rama "->" p1 (Ramaun "~" p2)) ((Rama "->" (Ramaun "~" p3) p4):xs) _|(igual p1 p3)=((Rama "->" (Ramaun "()" (Rama "->" p1 (Ramaun "~" p2))) (Ramaun "()" (Rama "->" p2 (Ramaun "~" p1)))),("T5",[p1,p2]))
buscar_aplicar_t5 x (p:xs) c = buscar_aplicar_t5 x xs c
buscar_aplicar_t5 x [] _ = ((Hoja ""),("",[]))
-- teorema del modus ponens, la utilizamos si existe una formula A y otra que es una implicacion cuyo antecedente es (A->B)->B
buscar_aplicar_tmp::Arbolprop->[Arbolprop]->(Arbolprop,(String,[Arbolprop]))
buscar_aplicar_tmp p1 ((Rama "->" (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p2  p3)) p4)) p5):xs)|(igual p1 p2) && (igual p3 p4)=((Rama "->" p1 (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p1 p2)) p3))),("TMP",[p1,p2]))
buscar_aplicar_tmp x (p:xs) = buscar_aplicar_tmp x xs
buscar_aplicar_tmp x [] = ((Hoja ""),("",[]))
-- identificar las premisas de la demostracion
poner_como_premisa::Arbolprop->(Arbolprop,(String,[Arbolprop]))
poner_como_premisa x=(x,("P",[]))
prop_sl::String->[Arbolprop]
prop_sl y = pasar_a_L (obtener_prop (sigToken y) [1] [] ("",""))
ver_demostracion::[(Arbolprop,(String,[Arbolprop]))]->Int->String
ver_demostracion ((p,(op,sust)):xs) c= "("++show c++")"++(visualizar_arbol p)++" "++op++"/"++join ',' (map visualizar_arbol sust)++"\n"++(ver_demostracion xs (c+1))
ver_demostracion [] _= ""
main:: IO ()
main = do
        putStrLn "Introduce una cadena:"
        y <- getLine
        --if (instrb (parser_slr (sigToken y) [1]) "entrada correcta" 1 1)>0 then putStrLn (visualizar_prop (pasar_a_L (obtener_prop (sigToken y) [1] [] ("","")))) else putStrLn ((parser_slr (sigToken y) [1]))	
	if (instrb (parser_slr (sigToken y) [1]) "entrada correcta" 1 1)>0 then putStrLn (ver_demostracion (buscar_implicaciones (tail (prop_sl y)) (map poner_como_premisa (tail (prop_sl y))) (head (prop_sl y))) 1 ) 
	                                                                                       else putStrLn ((parser_slr (sigToken y) [1]))	
	--putStrLn (visualizar_resto_entrada (sigToken y))