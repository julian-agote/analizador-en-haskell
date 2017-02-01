module Arbol(Arbolsintactico,obtener_regla_aplicable,devolver_ramas,devolver_valor,devolver_hoja,devolver_rama_vacia,devolver_rama) where
import Utilidades
import Data.Char
data Arbolsintactico = Hoja (String,String)|Rama String [Arbolsintactico]|Rama_vacia String
parte_izda::[Arbolsintactico]->String
parte_izda ((Rama a x):as)=a++" "++(parte_izda as)
parte_izda ((Hoja (a,b)):as)=a++" "++(parte_izda as)
parte_izda ((Rama_vacia a):as)=a++" "++(parte_izda as)
parte_izda []=""

obtener_regla_aplicable::Arbolsintactico->String
obtener_regla_aplicable (Rama a x)=a++" -> "++(rtrim (parte_izda (reverse x)))
obtener_regla_aplicable (Hoja (a,b))=a 
obtener_regla_aplicable (Rama_vacia a)=a++" -> "     
devolver_ramas::Arbolsintactico->[Arbolsintactico]  
devolver_ramas (Rama a x) = x
devolver_valor::Arbolsintactico->Int
devolver_valor (Hoja (a,b))=(convertir_a_entero b)
devolver_hoja::(String,String)->Arbolsintactico 
devolver_hoja (a,b)=(Hoja (a,b))
devolver_rama_vacia::String->Arbolsintactico
devolver_rama_vacia a=(Rama_vacia a)
devolver_rama::String->[Arbolsintactico]->Arbolsintactico 
devolver_rama a x=(Rama a x)
pow::Int->Int->Int
pow a b|(b>1)=a*(pow a (b-1))
       |(b==1)=a
       |(b==0)=1 
convertir_a_entero::String->Int
--convertir_a_entero a |((substr a 1 1)=="0" && (length a)>1)=(convertir_a_entero (substr a 2 (length a)))
--                     |((substr a 1 1)=="1" && (length a)>1)=(pow 10 ((length a)-1)) + (convertir_a_entero (substr a 2 (length a)))
--                     |((substr a 1 1)=="2" && (length a)>1)=2*(pow 10 ((length a)-1)) + (convertir_a_entero (substr a 2 (length a)))
--                     |((substr a 1 1)=="3" && (length a)>1)=3*(pow 10 ((length a)-1)) + (convertir_a_entero (substr a 2 (length a)))
--                     |((substr a 1 1)=="4" && (length a)>1)=4*(pow 10 ((length a)-1)) + (convertir_a_entero (substr a 2 (length a)))
--                     |((substr a 1 1)=="5" && (length a)>1)=5*(pow 10 ((length a)-1)) + (convertir_a_entero (substr a 2 (length a)))
--                     |((substr a 1 1)=="6" && (length a)>1)=6*(pow 10 ((length a)-1)) + (convertir_a_entero (substr a 2 (length a)))
--                     |((substr a 1 1)=="7" && (length a)>1)=7*(pow 10 ((length a)-1)) + (convertir_a_entero (substr a 2 (length a)))
--                     |((substr a 1 1)=="8" && (length a)>1)=8*(pow 10 ((length a)-1)) + (convertir_a_entero (substr a 2 (length a)))
--                     |((substr a 1 1)=="9" && (length a)>1)=9*(pow 10 ((length a)-1)) + (convertir_a_entero (substr a 2 (length a)))
--                     |((substr a 1 1)=="0" && (length a)==1)=0
--                     |((substr a 1 1)=="1" && (length a)==1)=1
--                     |((substr a 1 1)=="2" && (length a)==1)=2
--                     |((substr a 1 1)=="3" && (length a)==1)=3
--                     |((substr a 1 1)=="4" && (length a)==1)=4
--                     |((substr a 1 1)=="5" && (length a)==1)=5
--                     |((substr a 1 1)=="6" && (length a)==1)=6
--                     |((substr a 1 1)=="7" && (length a)==1)=7
--                     |((substr a 1 1)=="8" && (length a)==1)=8
--                     |((substr a 1 1)=="9" && (length a)==1)=9
obtiene_caracter (a,b)=a
convertir_a_entero a |((length a)>1)=(digitToInt (obtiene_caracter (head (readLitChar (substr a 1 1)))))*(pow 10 ((length a)-1)) + (convertir_a_entero (substr a 2 (length a)))
                     |otherwise = (digitToInt (obtiene_caracter (head (readLitChar (substr a 1 1)))))

