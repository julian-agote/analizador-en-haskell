--module Snippets(member,split,join,rtrim,substr,instrb,replace,primero,siguiente,cerradura,showRegla,producciones,showElemento,colec_slr,initElem) where
import Data.Char(ord)
import Data.List(delete,reverse)
replicar 1 x = [x]
replicar i x |i>1= x:replicar  (i - 1) x
--replicar i x |i<1=do putStrLn ("el primer parametro tiene que ser >=1")
--replicar 1 []
(!!!)::[a]-> Int -> a
(x:xs) !!! 0 = x
(x:xs) !!! i = xs !!! (i - 1)
zipe [] []=[]
zipe (x:xs) (y:ys) = (x,y):zipe xs ys
takeInt:: Int -> [a] -> [a]
takeInt 0 _ = []
takeInt _ [] = []
takeInt 1 (n:_) = [n]
takeInt m (n:ns) = n:takeInt (m-1) ns
dropInt:: Int -> [a] -> [a]
dropInt 0 l = l
dropInt m (n:ns) | m>(length (n:ns)) = []
                       | otherwise = dropInt (m-1) ns
sumInt::[Int]->Int
sumInt [] = 0
sumInt (n:ns) = n+sumInt ns
scanSum::[Int]->[Int]
--scanSumR::[Int]->[Int]
--scanSumR [] = []
--scanSumR (n:ns) = sumInt (n:ns):scanSumR(ns)
--scanSum x = reverse (scanSumR (reverse x))
scanSumN::Int ->[Int] ->[Int]
scanSum x= scanSumN 1 x
scanSumN m x|m>length x = []
scanSumN m x = sumInt (takeInt m x):scanSumN (m+1) x
diffs::[Int]->[Int]
diffs (n:m:[]) = [(m-n)]
diffs (n:m:ns) = (m-n):diffs (m:ns)
diffLists::[Int]->[Int]->[Int]
diffLists [] _ = []
diffLists _ [] = []
diffLists (n:ns) (m:ms) = (m-n):diffLists ns ms
at::[a]->Int->a
at x 1 = head x
at x m = at (tail x) (m-1)
isEven :: Integer -> Bool
isEven n
	| n < 0 = error "isEven needs a positive integer"
	| ((mod n 2) == 0) = True -- Even numbers have no remainder when divided by 2
	| otherwise = False -- If it has a remainder of anything but 0, it is not even
isOdd n = not (isEven n)
retainOdd::[Integer]->[Integer]
retainOdd [] = []
retainOdd (e:es) | (isOdd e ) && e>1 = e:retainOdd es
                       |otherwise = retainOdd es
esIgual::Char->Char->Bool
esIgual a b = if a==b then True else False
cuentaPrimVerd::[Bool]->Int
cuentaPrimVerd []  = 0
cuentaPrimVerd (e:[]) = if e then 1 else 0
cuentaPrimVerd (e:es) = if e then 1 + cuentaPrimVerd es else 0
encode::[Char]->[(Int,Char)]
encode [] = []
encode (e:es)=(cuentaPrimVerd (map (esIgual e) es) + 1,e):encode (dropInt (cuentaPrimVerd (map (esIgual e) es)) es)
lista 1 c = [c]
lista n c = c:lista (n-1) c
decode [(_,' ')]=[]
decode []=[]
decode ((n,c):ns)=(lista n c)++decode ns
mayor::Ord a=>a->a->a
mayor a b = if a>=b 
                  then a 
		  else b
maximun::Ord a=>[a]->a
maximun = foldr1 mayor 
--maximun = foldr1 (\a b-> if a>=b then a else b)
fact::Int->Int
fact 0 = 1
fact n = n*fact (n-1)
factList::Int->[Int]
factList n = map fact (takeInt n [1..])
member::Eq a =>a->[a]->Bool
member x [] = False
member x (e:es) |x==e = True
                       |otherwise = member x es
pos::Eq a=>a->Int->Int->[a]->Int
pos x c _ []=0
pos x c 1 (e:es) | x==e = c
		|otherwise = pos x (c+1) 1 es
pos x c m (e:es) | x==e = pos x (c+1) (m-1) es
		|otherwise = pos x (c+1) m es
posb::[Char]->Int->Int->[Char]->Int
posb y c _ []=0
posb y c 1 x | (substr x 1 (length y))==y = c
		|otherwise = posb y (c+1) 1 (tail x)
posb y c m x | (substr x 1 (length y))==y = posb y (c+(length y)) (m-1) (dropInt (length y) x)
		|otherwise = posb y (c+1) m (tail x)
split::Char->[Char]->[[Char]]
split x [] = []		 
split x l = takeInt (if (pos x 1 1 (trim l))==0 then length (trim l) else ((pos x 1 1 (trim l))-1)) (trim l):split x (dropInt (if (pos x 1 1 (trim l))==0 then length (trim l) else (pos x 1 1 (trim l))) (trim l))		
join::Char->[String]->String
join x [] = ""
join x (e:es) = e++[x]++join x es
data Partes = PIzda String
          |Pdcha [String]
type Regla=(Partes,Partes)
type Reglas = [Regla]
r1PIzda::Partes
r1Pdcha::Partes
r1PIzda=PIzda "E"
r1Pdcha=Pdcha (split ' ' "T EP")
r1::Regla
r1 = (r1PIzda,r1Pdcha)
producciones::Reglas
producciones=r1:((PIzda "EP"),(Pdcha (split ' ' "+ T EP"))):((PIzda "EP"),(Pdcha (split ' ' "lambda"))):((PIzda "T"),(Pdcha (split ' ' "F TP"))):((PIzda "TP"),(Pdcha (split ' ' "* F TP"))):((PIzda "TP"),(Pdcha (split ' ' "lambda"))):((PIzda "F"),(Pdcha (split ' ' "( E )"))):((PIzda "F"),(Pdcha (split ' ' "n"))):[]
variables=["E","EP","T","TP","F"]
terminales=["n","+","*","(",")"]
axioma="E"
esIgualPizda::String->Regla->Bool
esIgualPizda x ((PIzda y),_)|x==y=True
                                     |otherwise = False
reglasde::String->Reglas				
reglasde a = filter (esIgualPizda a) producciones
contienePdcha::String->Regla->Bool
contienePdcha x ((PIzda a),(Pdcha y))|(pos x 1 1 y)>0 = True
                                                    |otherwise = False
reglascontienen::String->Reglas
reglascontienen a = filter (contienePdcha a) producciones
showPartes::Partes->String
showPartes (PIzda x) = x++" -> "
showPartes (Pdcha x) = rtrim (join ' ' x)
showRegla::Regla->String
showRegla x = showPartes (fst x) ++showPartes (snd x) 
primerodcha::Partes->[String]
primerodcha (Pdcha []) = []
primerodcha (Pdcha x) = primero (head x)++(if (member "lambda" (primero (head x))) then (primerodcha (Pdcha (tail x))) else [])
primero::String->[String]
primero x |(member x (terminales++["lambda"])) = [x]
	     |otherwise = concat (map primerodcha [(snd n)|n<- (reglasde x)])
primero_cad::[String]->[String]
primero_cad [] = []
primero_cad (x:xs) = primero x ++ (if (member "lambda" (primero x)) then primero_cad xs else [])
eliminar_rep::[String]->[String]
eliminar_rep []=[]
eliminar_rep (x:xs) = x:(eliminar_rep (delete x xs))
--se define Siguiente(A) para el no terminal A como el cjto. de terminales que pueden aparecer a la dcha de A
--en alguna forma sentencial derivada del axioma de la gramatica
siguiente::String->[String]
siguiente x |x==axioma = "$":(eliminar_rep (concat [if (pos x 1 1 n)<(length n) then primero_cad (dropInt (pos x 1 1 n) n)++(if (member "lambda" (primero_cad (dropInt (pos x 1 1 n) n))) then if not(x==y) then (siguiente y) else [] else []) else if not(x==y) then (siguiente y) else []|(PIzda y, Pdcha n)<-reglascontienen x]))
               |otherwise = eliminar_rep (concat [if (pos x 1 1 n)<(length n) then primero_cad (dropInt (pos x 1 1 n) n)++(if (member "lambda" (primero_cad (dropInt (pos x 1 1 n) n))) then if not(x==y) then (siguiente y) else [] else []) else if not(x==y) then (siguiente y) else []|(PIzda y, Pdcha n)<-reglascontienen x])
-- si existe una regla A->alfa el analizador sintactico debe expandir por esta regla si en la cadena de entrada el
-- simbolo en curso pertenece a Primero(alfa). Si alfa=Lambda o deriva en Lambda (Lambda pertenece a Primero(alfa))
-- entonces se expandira por esta regla si el simbolo en curso de la entrada pertenece a siguiente(A)
tabla_analisis_desc::String->String->[String]
tabla_analisis_desc x y = concat [m|(PIzda n,Pdcha m)<-(reglasde x),(member y (primero_cad m))||((member "lambda" (primero_cad m)) && (member y (siguiente x)))]
parser_desc::[String]->[String]->String
parser_desc ("$":[]) ("$":[]) = "entrada correcta"
parser_desc [] x = "error: entrada terminada antes de tiempo, estado "++head(x)
parser_desc (x:xs) (y:ys) | x == y = parser_desc xs ys
				   |null (tabla_analisis_desc y x) = "error: no hay entrada en la tabla para el simbolo de entrada "++x++" estado "++y
				   |otherwise = parser_desc (x:xs) ((if not ((head(tabla_analisis_desc y x))=="lambda") then (tabla_analisis_desc y x) else [])++ys)
data Elemento =  Elemento {numero::Int, conjunto::[Regla]}
initElem = Elemento {numero = 1,conjunto = cerradura ((PIzda ("P"++axioma),Pdcha [".",axioma]):[])}
showElemento::[Elemento]->[String]
showElemento []=[]
showElemento ((Elemento {numero=n,conjunto=c}):xs) = (show n:(map showRegla c))++(showElemento xs)
ir_a::Elemento->String->Reglas
ir_a (Elemento {numero=n,conjunto=c}) x= cerradura [(PIzda r, Pdcha (avanzarpunto m))|(PIzda r, Pdcha m)<-(filter (contienePdcha x) c),m!!(if ((pos x 1 1 m)==1) then 0 else ((pos x 1 1 m)-2))=="."]
avanzarpunto::[String]->[String]
avanzarpunto x = (takeInt ((pos "." 1 1 x)-1) x)++[x!!(pos "." 1 1 x)]++["."]++(dropInt ((pos "." 1 1 x)+1) x)
cerradura::Reglas->Reglas	    
cerradura [] = []
cerradura ((PIzda x,Pdcha y):xs) | (pos "." 1 1 y)<(length y) && (member (y!!(pos "." 1 1 y)) variables) = [(PIzda x,Pdcha y)]++(cerradura ([n|n<-(anadirpunto (reglasde (y!!(pos "." 1 1 y)))),noexiste n xs]++xs))
                                 | otherwise = [(PIzda x,Pdcha y)]++(cerradura xs)
anadirpunto::Reglas->Reglas
anadirpunto [] = []
anadirpunto ((PIzda x,Pdcha y):xs) = [(PIzda x,Pdcha (".":y))]++(anadirpunto xs)
noexiste::Regla->Reglas->Bool
noexiste x y = not (member x y)
instance Eq Partes where
      (PIzda x)==(PIzda y) = x == y
      (Pdcha x)==(Pdcha y) = x == y
igualesReglas::Reglas->Reglas->Bool
igualesReglas [] _ = True
igualesReglas (x:xs) y |(member x y) = igualesReglas xs y
                       |otherwise = False
pertenece::Reglas->[Elemento]->Int->Int
pertenece _ [] n=n
pertenece c ((Elemento {numero=_,conjunto=x}):xs) n |(igualesReglas c x) && (igualesReglas x c) = n
                                                    |otherwise = pertenece c xs (n+1)
colec_slr::[Elemento]->[Elemento]->[Elemento]
colec_slr [] y = y      
colec_slr (x:xs) y = colec_slr ([Elemento {numero=(pertenece n y 1) , conjunto=n}|n<-(map (ir_a x) (terminales++variables)),(length n)>0 && (pertenece n y 1)>(length y)]++xs) ([Elemento {numero=(pertenece n y 1) , conjunto=n}|n<-(map (ir_a x) (terminales++variables)),(length n)>0&& (pertenece n y 1)>(length y)]++y)
cambiar_numero::[Elemento]->[Elemento]
cambiar_numero (x:[])=[x]
cambiar_numero ((Elemento {numero=n1,conjunto=c1}):(Elemento {numero=n2,conjunto=c2}):xs)|n2<=n1=(Elemento {numero=n1,conjunto=c1}):(cambiar_numero ((Elemento {numero=(n1+1),conjunto=c2}):xs))
                                                                                         |otherwise=(Elemento {numero=n1,conjunto=c1}):(cambiar_numero ((Elemento {numero=n2,conjunto=c2}):xs))
calc_colec_slr::[Elemento]->[Elemento] 
calc_colec_slr x = cambiar_numero (reverse (colec_slr x x))
data Elem_acc =  Elem_acc {estado::Int, term::String, accion::String, regla::Regla}
tabla_acc::[Elemento]->[[Elem_acc]]
tabla_acc []=[]
tabla_acc ((Elemento {numero=n,conjunto=c}):xs)=[[(Elem_acc {estado=n,term=z,accion="reducir",regla=(PIzda x,Pdcha y)})|z<-(siguiente x)]|(PIzda x,Pdcha y)<-reducir c]++(tabla_acc xs)
puntoAlFinal::Partes->Bool
puntoAlFinal (Pdcha y)=if ((pos "." 1 1 y)==(length y)) then True else False
reducir::[Regla]->[Regla]
reducir []=[]
reducir ((PIzda x,Pdcha y):xs)|(puntoAlFinal (Pdcha y))=(PIzda x,Pdcha y):reducir xs
                              |otherwise=reducir xs
showElem_acc::[Elem_acc]->String
showElem_acc []=""
showElem_acc ( Elem_acc {estado=n, term=z, accion=acc, regla=r}:xs)=show n++": en " ++z++" reducir por "++(showRegla r)++" "++(showElem_acc xs)
subir_de_nivel::[[Elem_acc]]->[Elem_acc]
subir_de_nivel ([])=[]
subir_de_nivel ([]:ys)=subir_de_nivel ys
subir_de_nivel ((x:xs):ys)=x:(subir_de_nivel (xs:ys))
mostrar_tabla_accion::[[Elem_acc]]->String
mostrar_tabla_accion []=""
mostrar_tabla_accion (x:xs) = (showElem_acc x)++(mostrar_tabla_accion xs)
rtrim::[Char]->[Char]
rtrim x = decode (encode x)
trim::[Char]->[Char]
trim []=[]
trim x = if snd (head (encode x)) == ' ' then decode (tail (encode x)) else decode (encode x)
substr::[Char]->Int->Int->[Char]
substr x n m = takeInt m (dropInt (n-1) x)
instr::[Char]->Char->Int->Int->Int
instr x y n m = if (pos y 1 m (dropInt (n-1) x))>0 then (pos y 1 m (dropInt (n-1) x))+(n-1) else 0
instrb::[Char]->[Char]->Int->Int->Int
instrb x y n m = if (posb y 1 m (dropInt (n-1) x))>0 then (posb y 1 m (dropInt (n-1) x))+(n-1) else 0
replace::[Char]->[Char]->[Char]->[Char]
replace [] _ _ = []
replace x y z | (instrb x y 1 1)>0= takeInt ((instrb x y 1 1)-1) x ++z++replace (dropInt ((instrb x y 1 1)-1+(length y)) x) y z
                  |otherwise = x
insensitive x y | member x (['a'..'z']++['A'..'Z']) && (member y (['a'..'z']++['A'..'Z'])) =  compare (if (member x ['a'..'z']) then (ord x)- (ord 'a') else (ord x)-(ord 'A'))
                                                                                                                               (if (member y ['a'..'z']) then (ord y)- (ord 'a') else (ord y)-(ord 'A'))
	            |otherwise = compare x y
verificar_elementos_cadena::[Char]->Char->Int		    
verificar_elementos_cadena [] _ = 0
verificar_elementos_cadena x c | (instr x c 1 1)>0 = 1 + (verificar_elementos_cadena (dropInt (instr x c 1 1) x) c)
					   | otherwise = 1
obtener_elemento_n::[Char]->Int->Char->[Char]					   
obtener_elemento_n x 1 c | (instr x c 1 1)>0 =	takeInt ((instr x c 1 1)-1) x
                                    | otherwise = x
obtener_elemento_n x n c = obtener_elemento_n (dropInt (instr x c 1 1) x) (n-1) c 				    
--quickSort comparison [] = []
--quickSort comparison [x] = [x]
--quickSort comparison (x : xs) = (quickSort comparison less) ++ (x : igual) ++ (quickSort comparison more)
--					    where less   = filter (\y -> comparison y x == LT) xs
--					  	       igual = filter (\y -> comparison y x == EQ) xs
--						       more  = filter (\y -> comparison y x == GT) xs		
--for :: a -> (a->Bool) -> (a->a) -> (a-> IO ()) -> IO ()
--for i p f job = if (p i) then do (job i)
--                                       (for (f i) p f job)
--			    else do putStrLn ""

                                                                          

		       
