module Utilidades(takeInt,dropInt,member,pos_punto,pos,posb,split,join,rtrim,trim,substr,instrb,replace,cuentaVerd,elimina_rep) where 
takeInt:: Int -> [a] -> [a]
takeInt 0 _ = []
takeInt _ [] = []
takeInt 1 (n:_) = [n]
takeInt m (n:ns) = n:takeInt (m-1) ns
dropInt:: Int -> [a] -> [a]
dropInt 0 l = l
dropInt m (n:ns) | m>(length (n:ns)) = []
                       | otherwise = dropInt (m-1) ns
esIgual::Char->Char->Bool
esIgual a b = if a==b then True else False
cuentaPrimVerd::[Bool]->Int
cuentaPrimVerd []  = 0
cuentaPrimVerd (e:[]) = if e then 1 else 0
cuentaPrimVerd (e:es) = if e then 1 + cuentaPrimVerd es else 0
cuentaVerd::[Bool]->Int
cuentaVerd []  = 0
cuentaVerd (e:[]) = if e then 1 else 0
cuentaVerd (e:es) = if e then 1 + (cuentaVerd es) else cuentaVerd es
encode::[Char]->[(Int,Char)]
encode [] = []
encode (e:es)=(cuentaPrimVerd (map (esIgual e) es) + 1,e):encode (dropInt (cuentaPrimVerd (map (esIgual e) es)) es)
lista 1 c = [c]
lista n c = c:lista (n-1) c
decode [(_,' ')]=[]
decode []=[]
decode ((n,c):ns)=(lista n c)++decode ns               
member::Eq a =>a->[a]->Bool
member x [] = False
member x (e:es) |x==e = True
                       |otherwise = member x es
pos_punto::Eq a=>a->a->Int->[a]->Int
pos_punto x b c []=0
pos_punto x b c (e:es) | (x==e && not ((b `elem` es) && (x `elem` es))) = c
        |otherwise = pos_punto x b (c+1) es
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
-- elimina los elementos que aparecen repetidos en una lista
elimina_rep::[String]->[String]
elimina_rep (x:xs) = if(null x) then elimina_rep xs else if (member x xs) then elimina_rep xs else x:(elimina_rep xs)
elimina_rep []=[]
               
