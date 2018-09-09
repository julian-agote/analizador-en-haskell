import Parser_trans
import Utilidades
import Data.Char(ord,chr)
import Arbol
import System.IO
import System.Environment
obtener_programa::Arbolsintactico->String
obtener_programa x |((obtener_regla_aplicable x)=="irudia -> bolumena P_COMA transformatu")=(obtener_matriz_posiciones (head (devolver_ramas x)))++"\n"++(obtener_trans ((devolver_ramas x)!!2))++"\n"++
                                                                                            (obtener_lineas 2 (obtener_nuevo_id ((devolver_ramas x)!!2)) (head (devolver_ramas x)))
obtener_matriz_posiciones::Arbolsintactico->String
obtener_matriz_posiciones x |((obtener_regla_aplicable x)=="bolumena -> ID G_IRE posizio_bektore_multsoa G_ITXI lerroak")=(devolver_id (head (devolver_ramas x)))++" = ["++
                                                                           (quitar_ultimo_caracter (obtener_matriz ((devolver_ramas x)!!2)))++"];\n"++
                                                                           (recalcular_posiciones (devolver_id (head (devolver_ramas x))) ((devolver_ramas x)!!4))++
                                                                           (obtener_lineas 1 (devolver_id (head (devolver_ramas x))) ((devolver_ramas x)!!4))++
                                                                           (establecer_limites  (devolver_id ((devolver_ramas x)!!0)))   
obtener_matriz::Arbolsintactico->String
obtener_matriz x |((obtener_regla_aplicable x)=="posizio_bektore_multsoa -> posizio_bektorea posizio_bektore_multsoa")=(obtener_matriz (head (devolver_ramas x)))++";"++
                                                                           (obtener_matriz ((devolver_ramas x)!!1))
                 |((obtener_regla_aplicable x)=="posizio_bektorea -> ID P_IRE ZENB ZENB ZENB ZENB P_ITXI")=(devolver_id ((devolver_ramas x)!!2))++" "++(devolver_id ((devolver_ramas x)!!3))++" "++                                                                      
                                                                           (devolver_id ((devolver_ramas x)!!4))++" "++(devolver_id ((devolver_ramas x)!!5))
                 |otherwise=""   
quitar_ultimo_caracter::String->String        
quitar_ultimo_caracter x = (substr x 1 ((length x)-1))  
obtener_lineas::Int->String->Arbolsintactico->String
obtener_lineas prim m x |((obtener_regla_aplicable x)=="lerroak -> P_IRE ID ID kurba P_ITXI lerroak")="X1=["++m++"("++(show (devuelve_pos_id (devolver_id ((devolver_ramas x)!!1))))++",1) "++
                                                                                                        m++"("++(show (devuelve_pos_id (devolver_id ((devolver_ramas x)!!2))))++",1)];"++
                                                                                                "Y1=["++m++"("++(show (devuelve_pos_id (devolver_id ((devolver_ramas x)!!1))))++",2) "++
                                                                                                        m++"("++(show (devuelve_pos_id (devolver_id ((devolver_ramas x)!!2))))++",2)];"++
                                                                                                "Z1=["++m++"("++(show (devuelve_pos_id (devolver_id ((devolver_ramas x)!!1))))++",3) "++
                                                                                                        m++"("++(show (devuelve_pos_id (devolver_id ((devolver_ramas x)!!2))))++",3)];\n"++
                                                                                                (if(prim>0) then "subplot(1,2,"++(show prim)++");" else "")++
                                                                                                (dibujar_curva ((devolver_ramas x)!!3))++(if(prim>0) then "view(3);hold on;" else "")++"\n"++
                                                                                                (obtener_lineas 0 m ((devolver_ramas x)!!5))                                                                                                  
                       -- spline kubikoa bukaerako baldintza erlaxatuekin
                        |((obtener_regla_aplicable x)=="lerroak -> SPLINE lista_id lerroak")=(obtener_tk ((devolver_ramas x)!!1) m)++
                                                                                             (obtener_puntos_inter m ((devolver_ramas x)!!1) 1 (obtener_profundidad ((devolver_ramas x)!!1)))++
                                                                                             (if(prim>0) then "subplot(1,2,"++(show prim)++");" else "")++
                                                                                             (dibujar_curva ((devolver_ramas x)!!1))++(if(prim>0) then "view(3);hold on;" else "")++"\n"++
                                                                                             (obtener_lineas 0 m ((devolver_ramas x)!!2)) 
                        |((obtener_regla_aplicable x)=="bolumena -> ID G_IRE posizio_bektore_multsoa G_ITXI lerroak")=(obtener_lineas prim m ((devolver_ramas x)!!4))++
                                                                                                                      (establecer_limites  m)                                                                      
                        |otherwise=""
devuelve_pos_id::String->Int
devuelve_pos_id y = ord(head y)-ord('A')+1  
dibujar_curva::Arbolsintactico->String
dibujar_curva x |((obtener_regla_aplicable x)=="kurba -> ZATIHIP ZENB ZENB aldaketa_segida")="a="++(devolver_id ((devolver_ramas x)!!1))++";b="++(devolver_id ((devolver_ramas x)!!2))++
                                                          ";thetamax=acosh(X1(2)/a);thetamin=acosh(X1(1)/a);dtheta=(thetamax-thetamin)/7;sd=sinh(dtheta);cd=cosh(dtheta);\nX1=[a*cd 0];Y1=[b*sd 0];\n"++
                                                          "XAUX=[a*cd b*sd 0 1];\nfor t=2:1:7\n\tXAUX(t,1)=XAUX(t-1,1)*cd+(a/b)*XAUX(t-1,2)*sd;\n\tXAUX(t,2)=(b/a)*XAUX(t-1,1)*sd+XAUX(t-1,2)*cd;\n\tXAUX(t,3)=0;XAUX(t,4)=1;\nend;\n"++
                                                          (if ((obtener_regla_aplicable ((devolver_ramas x)!!3))/="") then "XAUX = XAUX * "++(quitar_ultimo_caracter(obtener_matriz_cambios ((devolver_ramas x)!!3)))++";\n" else "")++
                                                          "for t=2:1:7\n\tX1(2)=XAUX(t,1);\n\tY1(2)=XAUX(t,2);\n\tline(X1,Y1,Z1);\n\tX1(1)=X1(2);\n\tY1(1)=Y1(2);\nend;\n"
                |((obtener_regla_aplicable x)=="kurba -> ZATIPAR ZENB aldaketa_segida")="a="++(devolver_id ((devolver_ramas x)!!1))++
                                                          ";thetamax=sqrt(X1(2)/a);thetamin=sqrt(X1(1)/a);dtheta=(thetamax-thetamin)/7;\nY1=[2*a*thetamin 0];\n"++
                                                          "XAUX=[X1(1) 2*a*thetamin 0 1];\nfor t=2:1:7\n\tXAUX(t,1)=XAUX(t-1,1)+XAUX(t-1,2)*dtheta+dtheta^2;\n\tXAUX(t,2)=XAUX(t-1,2)+2*dtheta;\n\tXAUX(t,3)=0;XAUX(t,4)=1;\nend;\n"++
                                                          (if ((obtener_regla_aplicable ((devolver_ramas x)!!2))/="") then "XAUX = XAUX * "++(quitar_ultimo_caracter(obtener_matriz_cambios ((devolver_ramas x)!!2)))++";\n" else "")++
                                                          "for t=2:1:7\n\tX1(2)=XAUX(t,1);\n\tY1(2)=XAUX(t,2);\n\tline(X1,Y1,Z1);\n\tX1(1)=X1(2);\n\tY1(1)=Y1(2);\nend;\n"
                |((obtener_regla_aplicable x)=="lista_id -> ID rlista_id")="X1=[P(1,1) P(2,1)];Y1=[P(1,2) P(2,2)];Z1=[P(1,3) P(2,3)];line(X1,Y1,Z1);\n"++
                                               (dibujar_sgte_trazo 2 (obtener_profundidad ((devolver_ramas x)!!1)))
                |otherwise="line(X1,Y1,Z1);"
dibujar_sgte_trazo::Int->Int->String                
dibujar_sgte_trazo n tam = "X1=[P("++(show n)++",1) P("++(show (n+1))++",1)];Y1=[P("++(show n)++",2) P("++(show (n+1))++",2)];Z1=[P("++(show n)++",3) P("++(show (n+1))++",3)];line(X1,Y1,Z1);\n" ++
                           (if (n<=(2*tam)) then (dibujar_sgte_trazo (n+1) tam) else "")                 
obtener_tk::Arbolsintactico->String->String                
obtener_tk x m ="p1="++(show (devuelve_pos_id (devolver_id (head (devolver_ramas x)))))++";"++(calcular_sig_tk 2 (devuelve_pos_id (devolver_id (head (devolver_ramas x)))) ((devolver_ramas x)!!1) m)++"G=["++
                (obtener_fila_M 1 (obtener_profundidad ((devolver_ramas x)!!1)))++
                (obtener_fila_R 1 (obtener_profundidad ((devolver_ramas x)!!1)) m)
calcular_sig_tk::Int->Int->Arbolsintactico->String->String
calcular_sig_tk n pos x m |((obtener_regla_aplicable x)=="rlista_id -> ID rlista_id")="t"++(show n)++" = sqrt(("++m++"("++(show (devuelve_pos_id (devolver_id (head (devolver_ramas x)))))++",1) -"++m++"("++(show pos)++",1))^2 + ("++
                                                                                       m++"("++(show (devuelve_pos_id (devolver_id (head (devolver_ramas x)))))++",2) -"++m++"("++(show pos)++",2))^2 + ("++
                                                                                       m++"("++(show (devuelve_pos_id (devolver_id (head (devolver_ramas x)))))++",3) -"++m++"("++(show pos)++",3))^2);\np"++(show n)++"="++(show (devuelve_pos_id (devolver_id (head (devolver_ramas x)))))++";\n"++
                                                                                      (calcular_sig_tk (n+1) (devuelve_pos_id (devolver_id (head (devolver_ramas x)))) ((devolver_ramas x)!!1) m) 
                          |otherwise=""                                                            
obtener_fila_M::Int->Int->String
obtener_fila_M n tam |(n==1)="1 0.5 "++(resto_ceros (tam-2))++";"++(obtener_fila_M (n+1) tam)
                     |(n==tam)=(resto_ceros (tam-2))++" 2 4]\\"
                     |otherwise=(resto_ceros (n-2))++"t"++(show (n+1))++" 2*(t"++(show n)++" + t"++(show (n+1))++") "++"t"++(show n)++" "++(resto_ceros (tam-(n+1)))++";"++(obtener_fila_M (n+1) tam)
obtener_profundidad::Arbolsintactico->Int
obtener_profundidad x  |((obtener_regla_aplicable x)=="rlista_id -> ID rlista_id")=(1+(obtener_profundidad ((devolver_ramas x)!!1)))
                       |((obtener_regla_aplicable x)=="lista_id -> ID rlista_id")=(obtener_profundidad ((devolver_ramas x)!!1))
                       |otherwise = 1
resto_ceros::Int->String
resto_ceros n|(n>0)="0 "++(resto_ceros (n-1))
             |otherwise="" 
obtener_fila_R::Int->Int->String->String
obtener_fila_R n tam m |(n==1)="[(3/(2*t2))*("++m++"(p2,1)-"++m++"(p1,1))  (3/(2*t2))*("++m++"(p2,2)-"++m++"(p1,2)) (3/(2*t2))*("++m++"(p2,3)-"++m++"(p1,3));"++(obtener_fila_R (n+1) tam m)  
                       |(n==tam)="(6/t"++(show n)++")*("++m++"(p"++(show n)++",1)-"++m++"(p"++(show (n-1))++",1)) (6/t"++(show n)++")*("++m++"(p"++(show n)++",2)-"++m++"(p"++(show (n-1))++",2)) (6/t"++(show n)++")*("++m++"(p"++(show n)++",3)-"++m++"(p"++(show (n-1))++",3))];\n"
                       |otherwise="(3/(t"++(show n)++"*t"++(show (n+1))++"))*(t"++(show n)++"^2*("++m++"(p"++(show (n+1))++",1)-"++m++"(p"++(show n)++",1))+t"++(show (n+1))++"^2*("++m++"(p"++(show n)++",1)-"++m++"(p"++(show (n-1))++",1))) "++
                                  "(3/(t"++(show n)++"*t"++(show (n+1))++"))*(t"++(show n)++"^2*("++m++"(p"++(show (n+1))++",2)-"++m++"(p"++(show n)++",2))+t"++(show (n+1))++"^2*("++m++"(p"++(show n)++",2)-"++m++"(p"++(show (n-1))++",2))) "++
                                  "(3/(t"++(show n)++"*t"++(show (n+1))++"))*(t"++(show n)++"^2*("++m++"(p"++(show (n+1))++",3)-"++m++"(p"++(show n)++",3))+t"++(show (n+1))++"^2*("++m++"(p"++(show n)++",3)-"++m++"(p"++(show (n-1))++",3)));"++(obtener_fila_R (n+1) tam m) 
obtener_puntos_inter::String->Arbolsintactico->Int->Int->String                                  
obtener_puntos_inter m x n tam |((obtener_regla_aplicable x)=="lista_id -> ID rlista_id")="P = ["++m++"("++(show (devuelve_pos_id (devolver_id (head (devolver_ramas x)))))++",1) "++
                                                                                                   m++"("++(show (devuelve_pos_id (devolver_id (head (devolver_ramas x)))))++",2) "++
                                                                                                   m++"("++(show (devuelve_pos_id (devolver_id (head (devolver_ramas x)))))++",3);\n"++(obtener_puntos_inter m ((devolver_ramas x)!!1) (n+1) tam)
                               |((obtener_regla_aplicable x)=="rlista_id -> ID rlista_id") && (n<=tam)="[2*(1/3)^3-3*(1/3)^2+1 (-2*(1/3)^3+3*(1/3)^2) (1/3)*((1/3)^2-2*(1/3)+1)*t"++(show n)++
                                                                                                      " (1/3)*((1/3)^2-(1/3))*t"++(show n)++"]*["++m++"(p"++(show (n-1))++",1) "++m++"(p"++(show (n-1))++",2) "++m++"(p"++(show (n-1))++",3); "++m++
                                                                                                                                                      "(p"++(show n)++",1) "++m++"(p"++(show n)++",2) "++m++"(p"++(show n)++",3);G("++(show (n-1))++",1) G("++(show (n-1))++",2) G("++
                                                                                                                                                      (show (n-1))++",3); G("++(show n)++",1) G("++(show n)++",2) G("++(show n)++",3)];\n"++
                                                                                                      "[2*(2/3)^3-3*(2/3)^2+1 (-2*(2/3)^3+3*(2/3)^2) (2/3)*((2/3)^2-2*(2/3)+1)*t"++(show n)++
                                                                                                      " (2/3)*((2/3)^2-(2/3))*t"++(show n)++"]*["++m++"(p"++(show (n-1))++",1) "++m++"(p"++(show (n-1))++",2) "++m++"(p"++(show (n-1))++",3); "++m++
                                                                                                                                                      "(p"++(show n)++",1) "++m++"(p"++(show n)++",2) "++m++"(p"++(show n)++",3);G("++(show (n-1))++",1) G("++(show (n-1))++",2) G("++
                                                                                                                                                      (show (n-1))++",3); G("++(show n)++",1) G("++(show n)++",2) G("++(show n)++",3)];\n"++
                                                                                                      m++"("++(show (devuelve_pos_id (devolver_id (head (devolver_ramas x)))))++",1) "++
                                                                                                      m++"("++(show (devuelve_pos_id (devolver_id (head (devolver_ramas x)))))++",2) "++
                                                                                                      m++"("++(show (devuelve_pos_id (devolver_id (head (devolver_ramas x)))))++",3)"++(if (n<tam) then ";" else "")++ 
                                                                                                      (obtener_puntos_inter m ((devolver_ramas x)!!1) (n+1) tam)
                               |otherwise= "];\n"                                                                      

recalcular_posiciones::String->Arbolsintactico->String                                                                                                
recalcular_posiciones m x |((obtener_regla_aplicable x)=="lerroak -> P_IRE ID ID kurba P_ITXI lerroak")=(recalcular_curva m (show (devuelve_pos_id (devolver_id ((devolver_ramas x)!!1)))) (show (devuelve_pos_id (devolver_id ((devolver_ramas x)!!2)))) ((devolver_ramas x)!!3)) ++       
                                                                                                        (recalcular_posiciones m ((devolver_ramas x)!!5))  
                          |otherwise=""                                                                                       
recalcular_curva::String->String->String->Arbolsintactico->String                
recalcular_curva m p1 p2 x |((obtener_regla_aplicable x)=="kurba -> ZATIHIP ZENB ZENB aldaketa_segida")="a="++(devolver_id ((devolver_ramas x)!!1))++";b="++(devolver_id ((devolver_ramas x)!!2))++
                                                          "X1=["++m++"("++p1++",1) "++m++"("++p2++",1)];Y1=["++m++"("++p1++",2) "++m++"("++p2++",2)];"++
                                                          ";thetamax=acosh(X1(2)/a);thetamin=acosh(X1(1)/a);dtheta=(thetamax-thetamin)/7;sd=sinh(dtheta);cd=cosh(dtheta);\nXAUX=[a*cd b*sd 0 1];\n"++                                                         
                                                          "for t=2:1:7\n\tXAUX(t,1)=XAUX(t-1,1)*cd+(a/b)*XAUX(t-1,2)*sd;\n\tXAUX(t,2)=(b/a)*XAUX(t-1,1)*sd+XAUX(t-1,2)*cd;\n\tXAUX(t,3)=0;XAUX(t,4)=1;\nend;\n"++
                                                          (if ((obtener_regla_aplicable ((devolver_ramas x)!!3))/="") then ("XAUX = XAUX * "++(quitar_ultimo_caracter(obtener_matriz_cambios ((devolver_ramas x)!!3)))++";\n") else "")++
                                                           m++"("++p1++",2) = XAUX(1,2);"++m++"("++p2++",2) = XAUX(7,2);\n"
                           |((obtener_regla_aplicable x)=="kurba -> ZATIPAR ZENB aldaketa_segida")="a="++(devolver_id ((devolver_ramas x)!!1))++
                                                          ";X1=["++m++"("++p1++",1) "++m++"("++p2++",1)];Y1=["++m++"("++p1++",2) "++m++"("++p2++",2)];"++
                                                          ";thetamax=sqrt(X1(2)/a);thetamin=sqrt(X1(1)/a);dtheta=(thetamax-thetamin)/7;\nY1=[2*a*thetamin 0];\n"++
                                                          "XAUX=[X1(1) 2*a*thetamin 0 1];\nfor t=2:1:7\n\tXAUX(t,1)=XAUX(t-1,1)+XAUX(t-1,2)*dtheta+dtheta^2;\n\tXAUX(t,2)=XAUX(t-1,2)+2*dtheta;\n\tXAUX(t,3)=0;XAUX(t,4)=1;\nend;\n"++
                                                          (if ((obtener_regla_aplicable ((devolver_ramas x)!!2))/="") then "XAUX = XAUX * "++(quitar_ultimo_caracter(obtener_matriz_cambios ((devolver_ramas x)!!2)))++";\n" else "")++
                                                           m++"("++p1++",2) = XAUX(1,2);"++m++"("++p2++",2) = XAUX(7,2);\n"
                           |otherwise=""  
                                                                                                                                                                                                                                                                            
establecer_limites::String->String
establecer_limites m = "xlim([min("++m++"(:,1))-1 max("++m++"(:,1))+1]);ylim([min("++m++"(:,2))-1 max("++m++"(:,2))+1]);zlim([min("++m++"(:,3))-1 max("++m++"(:,3))+1]);xlabel('x');ylabel('y');zlabel('z');"
obtener_nuevo_id::Arbolsintactico->String
obtener_nuevo_id x |((obtener_regla_aplicable x)=="transformatu -> ID ESLEI ID aldaketa_segida")=(devolver_id (head(devolver_ramas x)))                                                                                                                   
obtener_trans::Arbolsintactico->String
obtener_trans x  |((obtener_regla_aplicable x)=="transformatu -> ID ESLEI ID aldaketa_segida")=(obtener_vector_cos_directores ((devolver_ramas x)!!3))++(devolver_id (head(devolver_ramas x)))++" = "++(devolver_id ((devolver_ramas x)!!2))++"*"++
                                                                          (quitar_ultimo_caracter(obtener_matriz_cambios ((devolver_ramas x)!!3)))    
obtener_matriz_cambios::Arbolsintactico->String
obtener_matriz_cambios x |((obtener_regla_aplicable x)=="aldaketa_segida -> aldaketa aldaketa_segida")=(obtener_matriz_cambios (head(devolver_ramas x)))++"*"++(obtener_matriz_cambios ((devolver_ramas x)!!1))
                         |((obtener_regla_aplicable x)=="aldaketa -> BIRAKETA ardatza ZENB")=if(obtener_eje ((devolver_ramas x)!!1))=="x" then (matriz_giro_x (devolver_id ((devolver_ramas x)!!2))) else
                                                                                             if(obtener_eje ((devolver_ramas x)!!1))=="y" then (matriz_giro_y (devolver_id ((devolver_ramas x)!!2))) else 
                                                                                                                                               (matriz_giro_z (devolver_id ((devolver_ramas x)!!2)))
                         |((obtener_regla_aplicable x)=="aldaketa -> LEKU_ALDATZEA posizio_bektorea")="[1 0 0 0;0 1 0 0;0 0 1 0;"++(obtener_coord ((devolver_ramas x)!!1) 2)++" "++(obtener_coord ((devolver_ramas x)!!1) 3)++" "++(obtener_coord ((devolver_ramas x)!!1) 4)++" 1]"                                                                                                                        
                         |((obtener_regla_aplicable x)=="aldaketa -> ARDATZAREN_INGURUKO_BIRAKETA posizio_bektorea posizio_bektorea ZENB")="[1 0 0 0;0 1 0 0;0 0 1 0;"++(obtener_coord ((devolver_ramas x)!!1) 2)++" "++(obtener_coord ((devolver_ramas x)!!1) 3)++" "++(obtener_coord ((devolver_ramas x)!!1) 4)++" 1]*"++
                                                                          (matriz_giro_x ("acos(C(2)/sqrt(C(1)^2+C(2)^2))"))++"*"++
                                                                          (matriz_giro_y ("-asin(C(1))"))++"*"++(matriz_giro_z (devolver_id ((devolver_ramas x)!!3)))++
                                                                          "*inv("++(matriz_giro_y ("-asin(C(1))"))++")*inv("++(matriz_giro_x ("acos(C(2)/sqrt(C(1)^2+C(2)^2))"))++")*inv("++
                                                                          "[1 0 0 0;0 1 0 0;0 0 1 0;"++(obtener_coord ((devolver_ramas x)!!1) 2)++" "++(obtener_coord ((devolver_ramas x)!!1) 3)++" "++(obtener_coord ((devolver_ramas x)!!1) 4)++" 1])"
                         |otherwise="" 
obtener_vector_cos_directores::Arbolsintactico->String
obtener_vector_cos_directores x |((obtener_regla_aplicable x)=="aldaketa_segida -> aldaketa aldaketa_segida")=(obtener_vector_cos_directores (head (devolver_ramas x)))
                                |((obtener_regla_aplicable x)=="aldaketa -> ARDATZAREN_INGURUKO_BIRAKETA posizio_bektorea posizio_bektorea ZENB")=(cosenos_directores ((devolver_ramas x)!!1) ((devolver_ramas x)!!2))
                                |otherwise=""
cosenos_directores::Arbolsintactico->Arbolsintactico->String                         
cosenos_directores p q="C=[("++(devolver_id ((devolver_ramas q)!!2))++" - "++(devolver_id ((devolver_ramas p)!!2))++") ("++(devolver_id ((devolver_ramas q)!!3))++" - "++(devolver_id ((devolver_ramas p)!!3))++") ("++(devolver_id ((devolver_ramas q)!!4))++" - "++(devolver_id ((devolver_ramas p)!!4))++")]./sqrt(("++
                       (devolver_id ((devolver_ramas q)!!2))++" - "++(devolver_id ((devolver_ramas p)!!2))++")^2+("++(devolver_id ((devolver_ramas q)!!3))++" - "++(devolver_id ((devolver_ramas p)!!3))++")^2+("++(devolver_id ((devolver_ramas q)!!4))++" - "++(devolver_id ((devolver_ramas p)!!4))++")^2);"  
obtener_eje::Arbolsintactico->String
obtener_eje x |((obtener_regla_aplicable x)=="ardatza -> P_X")="x"
              |((obtener_regla_aplicable x)=="ardatza -> P_Y")="y"
              |otherwise="z"  
obtener_coord::Arbolsintactico->Int->String 
obtener_coord x n= (devolver_id ((devolver_ramas x)!!n))            
matriz_giro_x::String->String                            
matriz_giro_x angulo = "[1 0 0 0;0 cos("++angulo++") sin("++angulo++") 0;0 -sin("++angulo++") cos("++angulo++") 0;0 0 0 1 ]" 
matriz_giro_y::String->String                            
matriz_giro_y angulo = "[cos("++angulo++") sin("++angulo++") 0 0;-sin("++angulo++") cos("++angulo++") 0 0;0 0 1 0;0 0 0 1 ]" 
matriz_giro_z::String->String                            
matriz_giro_z angulo = "[cos("++angulo++") 0 -sin("++angulo++") 0;0 1 0 0;sin("++angulo++") 0 cos("++angulo++") 0;0 0 0 1 ]" 
main = do
        h1 <- openFile "prog_trans3.txt" ReadMode
        y  <- hGetContents h1
        --putStrLn (parser_slr y [1])
        writeFile "aldaketak3.m" (obtener_programa(parser_slr_arbol y [1] []))
        hClose h1       
