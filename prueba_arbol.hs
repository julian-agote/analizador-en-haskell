import Utilidades
data Arbolprop = Hoja String| Rama  String (Arbolprop) (Arbolprop)| Ramaun String (Arbolprop)
arbolprop1::Arbolprop 
arbolprop1=(Rama "->" (Hoja "p") (Ramaun "()" (Rama "->" (Ramaun "~" (Hoja "q")) (Hoja "r"))))
arbolprop2::Arbolprop 
arbolprop2= (Rama "->" (Hoja "q") (Hoja "r"))
arbolprop3::Arbolprop 
arbolprop3= (Rama "->" (Hoja "r") (Hoja "s"))
arbolprop4::Arbolprop 
arbolprop4= (Hoja "p")
arbolprop5::Arbolprop 
arbolprop5= (Hoja "s")
-- compara si son iguales dos formulas
igual::Arbolprop->Arbolprop->Bool
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
buscar_implicaciones ((Rama "->" x y):xs) lc c|(fst(obtener_antecedente x lc)/="") && not(miembro (y,("MP("++show (posicion (Rama "->" x y) (reverse lc) 1)++")("++show (posicion x (reverse lc) 1)++")",snd(obtener_antecedente x lc))) lc)= buscar_implicaciones (y:xs) ([(y,("MP("++show (posicion (Rama "->" x y) (reverse lc) 1)++")("++show (posicion x (reverse lc) 1)++")",snd(obtener_antecedente x lc)))]++lc) c
buscar_implicaciones ((Ramaun "()" (Rama "->" x y)):xs) lc c|(fst(obtener_antecedente x lc)/="") && not(miembro (y,("MP("++show (posicion (Ramaun "()" (Rama "->" x y)) (reverse lc) 1)++")("++show (posicion x (reverse lc) 1)++")",snd(obtener_antecedente x lc))) lc)= buscar_implicaciones (y:xs) ([(y,("MP("++show (posicion (Ramaun "()" (Rama "->" x y)) (reverse lc) 1)++")("++show (posicion x (reverse lc) 1)++")",snd(obtener_antecedente x lc)))]++lc) c
buscar_implicaciones (x:xs) lc c|(igual (quitar_parentesis x) (quitar_parentesis c))= reverse lc
buscar_implicaciones (x:xs) lc c|fst (snd (buscar_aplicar_t2 (quitar_parentesis x) (resto_formulas x lc)))/="" && not(miembro (buscar_aplicar_t2 (quitar_parentesis x) (resto_formulas x lc)) lc)=buscar_implicaciones ((fst(buscar_aplicar_t2 (quitar_parentesis x) (resto_formulas x lc))):x:xs) ([(buscar_aplicar_t2 (quitar_parentesis x) (resto_formulas x lc))]++lc) c
buscar_implicaciones (x:xs) lc c|fst (snd (buscar_aplicar_t3 x (resto_formulas x lc)))/="" && not(miembro (buscar_aplicar_t3 x (resto_formulas x lc)) lc)=buscar_implicaciones ((fst(buscar_aplicar_t3 x (resto_formulas x lc))):x:xs) ([(buscar_aplicar_t3 x (resto_formulas x lc))]++lc) c
buscar_implicaciones (x:xs) lc c|fst (snd (buscar_aplicar_t4 x (resto_formulas x lc) c))/="" && not(miembro (buscar_aplicar_t4 x (resto_formulas x lc) c) lc)=buscar_implicaciones ((fst(buscar_aplicar_t4 x (resto_formulas x lc) c)):x:xs) ([(buscar_aplicar_t4 x (resto_formulas x lc) c)]++lc) c
buscar_implicaciones (x:xs) lc c|fst (snd (buscar_aplicar_A1 x (resto_formulas x lc) c))/="" && not(miembro (buscar_aplicar_A1 x (resto_formulas x lc) c) lc)=buscar_implicaciones ((fst(buscar_aplicar_A1 x (resto_formulas x lc) c)):x:xs) ([(buscar_aplicar_A1 x (resto_formulas x lc) c)]++lc) c
buscar_implicaciones (x:xs) lc c|fst (snd (buscar_aplicar_A2 (quitar_parentesis x) (resto_formulas x lc) c))/="" && not(miembro (buscar_aplicar_A2 (quitar_parentesis x) (resto_formulas x lc) c) lc)=buscar_implicaciones ((fst(buscar_aplicar_A2 (quitar_parentesis x) (resto_formulas x lc) c)):x:xs) ([(buscar_aplicar_A2 (quitar_parentesis x) (resto_formulas x lc) c)]++lc) c
buscar_implicaciones (x:xs) lc c|fst (snd (buscar_aplicar_A3 (quitar_parentesis x) (resto_formulas x lc) (quitar_parentesis c)))/="" && not(miembro (buscar_aplicar_A3 (quitar_parentesis x) (resto_formulas x lc) (quitar_parentesis c)) lc)=buscar_implicaciones ((fst(buscar_aplicar_A3 (quitar_parentesis x) (resto_formulas x lc) (quitar_parentesis c))):x:xs) ([(buscar_aplicar_A3 (quitar_parentesis x) (resto_formulas x lc) (quitar_parentesis c))]++lc) c
buscar_implicaciones (x:xs) lc c= buscar_implicaciones xs lc c
buscar_implicaciones [] lc _= reverse lc
-- ver si hay dos formulas que sean antecedentes del T2 (A->B)->((B->C)->(A->C)), es decir buscar (A->B) y (B->C) en cuyo caso se añade T2 al proceso de demostracion
buscar_aplicar_t2::Arbolprop->[Arbolprop]->(Arbolprop,(String,[Arbolprop]))
buscar_aplicar_t2 (Rama "->" p1 p2) ((Ramaun "()" (Rama "->" p3 p4)):xs)|(igual p2 p3)=((Rama "->" (Ramaun "()" (Rama "->" p1 p2)) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p3 p4)) (Ramaun "()" (Rama "->" p1 p4))))),("T2",[p1,p2,p4]))
buscar_aplicar_t2 (Rama "->" p1 p2) ((Rama "->" p3 p4):xs)|(igual p2 p3)=((Rama "->" (Ramaun "()" (Rama "->" p1 p2)) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p3 p4)) (Ramaun "()" (Rama "->" p1 p4))))),("T2",[p1,p2,p4]))
buscar_aplicar_t2 p (x:xs)= buscar_aplicar_t2 p xs
buscar_aplicar_t2 p [] = ((Hoja ""),("",[]))
-- ver si hay dos formulas que sean antecedentes del T3 (A->B)->((A->(B->C))->(A->C)), es decir buscar (A->B) y (A->(B->C)) en cuyo caso se añade T3 al proceso de demostracion
buscar_aplicar_t3::Arbolprop->[Arbolprop]->(Arbolprop,(String,[Arbolprop]))
buscar_aplicar_t3 (Ramaun "()" (Rama "->" p1 p2)) ((Ramaun "()" (Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5)))):xs)|(igual p1 p3)&&(igual p2 p4)=((Rama "->" (Ramaun "()" (Rama "->" p1 p2)) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5)))) (Ramaun "()" (Rama "->" p1 p5))))),("T3",[p1,p2,p3]))
															                                      |otherwise = (buscar_aplicar_t3 (Ramaun "()" (Rama "->" p1 p2)) xs)
buscar_aplicar_t3 (Ramaun "()" (Rama "->" p1 p2)) ((Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5))):xs)|(igual p1 p3)&&(igual p2 p4)=((Rama "->" (Ramaun "()" (Rama "->" p1 p2)) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5)))) (Ramaun "()" (Rama "->" p1 p5))))),("T3",[p1,p2,p3]))
															                   |otherwise = (buscar_aplicar_t3 (Ramaun "()" (Rama "->" p1 p2)) xs)
buscar_aplicar_t3 (Rama "->" p1 p2) ((Ramaun "()" (Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5)))):xs)|(igual p1 p3)&&(igual p2 p4)=((Rama "->" (Ramaun "()" (Rama "->" p1 p2)) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5)))) (Ramaun "()" (Rama "->" p1 p5))))),("T3",[p1,p2,p3]))
															                   |otherwise = (buscar_aplicar_t3 (Ramaun "()" (Rama "->" p1 p2)) xs)
buscar_aplicar_t3 (Rama "->" p1 p2) ((Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5))):xs)|(igual p1 p3)&&(igual p2 p4)=((Rama "->" (Ramaun "()" (Rama "->" p1 p2)) (Ramaun "()" (Rama "->" (Ramaun "()" (Rama "->" p3 (Ramaun "()" (Rama "->" p4 p5)))) (Ramaun "()" (Rama "->" p1 p5))))),("T3",[p1,p2,p3]))
															|otherwise = (buscar_aplicar_t3 (Ramaun "()" (Rama "->" p1 p2)) xs)
buscar_aplicar_t3 (Ramaun "~" p) xs = buscar_aplicar_t3 (head xs) (tail xs)
buscar_aplicar_t3 (Ramaun "()" (Hoja _)) xs = buscar_aplicar_t3 (head xs) (tail xs)
buscar_aplicar_t3 (Ramaun "()" (Ramaun _ _)) xs = buscar_aplicar_t3 (head xs) (tail xs)
buscar_aplicar_t3 (Hoja p) xs = buscar_aplicar_t3 (head xs) (tail xs)
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
buscar_aplicar_A2 (Rama "->" p1 (Ramaun "()" (Rama "->" p2 p3))) ((Ramaun "()" (Rama "->" p4 p5)):p:xs) c|(igual p1 p4)&&(igual p2 p5) = buscar_aplicar_A2 (Rama "->" p1 (Ramaun "()" (Rama "->" p2 p3))) ((Ramaun "()" (Rama "->" p4 p5)):xs) c
buscar_aplicar_A2 x (p:xs) c = buscar_aplicar_A2 x xs c
buscar_aplicar_A2 x [] _ = ((Hoja ""),("",[]))
buscar_aplicar_A3::Arbolprop->[Arbolprop]->Arbolprop->(Arbolprop,(String,[Arbolprop]))
buscar_aplicar_A3 (Rama "->" (Ramaun "~" p1) (Ramaun "~" p2)) _ (Rama "->" p3 p4)|(igual p1 p4)&&(igual p2 p3) = ((Rama "->" (Ramaun "()" (Rama "->" (Ramaun "~" p1) (Ramaun "~" p2))) (Ramaun "()" (Rama "->" p2 p1))),("A3",[p1,p2]))
buscar_aplicar_A3 (Rama "->" (Ramaun "~" p1) (Ramaun "~" p2)) ((Rama "->" (Rama "->" p3 p4) p5):xs) _|(igual p1 p4)&&(igual p2 p3) = ((Rama "->" (Ramaun "()" (Rama "->" (Ramaun "~" p1) (Ramaun "~" p2))) (Ramaun "()" (Rama "->" p2 p1))),("A3",[p1,p2]))
buscar_aplicar_A3 (Rama "->" (Ramaun "~" p1) (Ramaun "~" p2)) ((Rama "->" (Ramaun "()" (Rama "->" p3 p4)) p5):xs) _|(igual p1 p4)&&(igual p2 p3) = ((Rama "->" (Ramaun "()" (Rama "->" (Ramaun "~" p1) (Ramaun "~" p2))) (Ramaun "()" (Rama "->" p2 p1))),("A3",[p1,p2]))
buscar_aplicar_A3 x (p:xs) c = buscar_aplicar_A3 x xs c
buscar_aplicar_A3 x [] _ = ((Hoja ""),("",[]))
-- eliminacion de la doble negacion
buscar_aplicar_t4::Arbolprop->[Arbolprop]->Arbolprop->(Arbolprop,(String,[Arbolprop]))
buscar_aplicar_t4 (Ramaun "~" (Ramaun "~" p1)) _ p2|(igual (quitar_parentesis p1) (quitar_parentesis p2))=((Rama "->" (Ramaun "~" (Ramaun "~" p1)) p2),("T4",[p1]))
buscar_aplicar_t4 (Ramaun "~" (Ramaun "~" p1)) (p2:xs) _|(igual (quitar_parentesis p1) (quitar_parentesis p2))=((Rama "->" (Ramaun "~" (Ramaun "~" p1)) p2),("T4",[p1]))
buscar_aplicar_t4 x (p:xs) c = buscar_aplicar_t4 x xs c
buscar_aplicar_t4 x [] _ = ((Hoja ""),("",[]))
-- identificar las premisas de la demostracion
poner_como_premisa::Arbolprop->(Arbolprop,(String,[Arbolprop]))
poner_como_premisa x=(x,("P",[]))
ver_demostracion::[(Arbolprop,(String,[Arbolprop]))]->Int->String
ver_demostracion ((p,(op,sust)):xs) c= "("++show c++")"++(visualizar_arbol p)++" "++op++"/"++join ',' (map visualizar_arbol sust)++"\n"++(ver_demostracion xs (c+1))
ver_demostracion [] _= ""

--putStrLn(ver_demostracion (buscar_implicaciones [arbolprop1,arbolprop2,arbolprop3,arbolprop4] (map poner_como_premisa [arbolprop1,arbolprop2,arbolprop3,arbolprop4]) arbolprop5) 1)