import Parser_trans
import Utilidades
import Data.Char(ord,chr)
import Arbol
import System.IO
import System.Environment
obtener_programa::Arbolsintactico->String
obtener_programa x |((obtener_regla_aplicable x)=="irudia -> bolumena P_COMA transformatu")=(obtener_matriz_posiciones (head (devolver_ramas x)))++"\n"++(obtener_trans ((devolver_ramas x)!!2) (head(devolver_ramas x)))++"\n"++
																							(obtener_lineas (if (tipo_trans ((devolver_ramas x)!!2))=="superficie" then 0 else 2) (obtener_nuevo_id ((devolver_ramas x)!!2)) (head (devolver_ramas x)) 0)
obtener_matriz_posiciones::Arbolsintactico->String
obtener_matriz_posiciones x |((obtener_regla_aplicable x)=="bolumena -> ID G_IRE posizio_bektore_multsoa G_ITXI lerroak")=(devolver_id (head (devolver_ramas x)))++" = ["++
																		   (quitar_ultimo_caracter (obtener_matriz ((devolver_ramas x)!!2)))++"];\n"++
																		   (recalcular_posiciones (devolver_id (head (devolver_ramas x))) ((devolver_ramas x)!!4))++
																		   (devolver_id (head (devolver_ramas x)))++" = "++(devolver_id (head (devolver_ramas x)))++" * [1 0 0 0;0 cosd(90) sind(90) 0;0 -sind(90) cosd(90) 0;0 0 0 1 ];\n"++
																		   (obtener_lineas 1 (devolver_id (head (devolver_ramas x))) ((devolver_ramas x)!!4) 0)++
																		   (establecer_limites	(devolver_id ((devolver_ramas x)!!0)))	 
obtener_matriz::Arbolsintactico->String
obtener_matriz x |((obtener_regla_aplicable x)=="posizio_bektore_multsoa -> posizio_bektorea posizio_bektore_multsoa")=(obtener_matriz (head (devolver_ramas x)))++";"++
																		   (obtener_matriz ((devolver_ramas x)!!1))
				 |((obtener_regla_aplicable x)=="posizio_bektorea -> ID P_IRE ZENB ZENB ZENB ZENB P_ITXI")=(devolver_id ((devolver_ramas x)!!2))++" "++(devolver_id ((devolver_ramas x)!!3))++" "++																		 
																		   (devolver_id ((devolver_ramas x)!!4))++" "++(devolver_id ((devolver_ramas x)!!5))
				 |otherwise=""	 
quitar_ultimo_caracter::String->String		  
quitar_ultimo_caracter x = (substr x 1 ((length x)-1))	
obtener_lineas::Int->String->Arbolsintactico->Int->String
obtener_lineas prim m x ntab|((obtener_regla_aplicable x)=="lerroak -> P_IRE ID ID kurba P_ITXI lerroak")="X1=["++m++"("++(show (devuelve_pos_id (devolver_id ((devolver_ramas x)!!1))))++",1) "++
																										m++"("++(show (devuelve_pos_id (devolver_id ((devolver_ramas x)!!2))))++",1)];"++
																								"Y1=["++m++"("++(show (devuelve_pos_id (devolver_id ((devolver_ramas x)!!1))))++",2) "++
																										m++"("++(show (devuelve_pos_id (devolver_id ((devolver_ramas x)!!2))))++",2)];"++
																								"Z1=["++m++"("++(show (devuelve_pos_id (devolver_id ((devolver_ramas x)!!1))))++",3) "++
																										m++"("++(show (devuelve_pos_id (devolver_id ((devolver_ramas x)!!2))))++",3)];\n"++(indentar ntab)++
																								(obtener_ptos_inter_curva ((devolver_ramas x)!!3))++		
																								(if(prim>0) then "subplot(1,2,"++(show prim)++");" else "")++
																								(dibujar_curva ((devolver_ramas x)!!3) 0)++(if(prim>0) then "view(3);hold on;" else "")++"\n"++(indentar ntab)++
																								(obtener_lineas 0 m ((devolver_ramas x)!!5) ntab)																								   
					   -- spline kubikoa bukaerako baldintza erlaxatuekin
						|((obtener_regla_aplicable x)=="lerroak -> SPLINE lista_id lerroak")=(obtener_tk ((devolver_ramas x)!!1) m)++
																							 (obtener_puntos_inter m ((devolver_ramas x)!!1) 1 (obtener_profundidad ((devolver_ramas x)!!1)))++
																							 (if(prim>0) then "subplot(1,2,"++(show prim)++");" else "")++
																							 (dibujar_curva ((devolver_ramas x)!!1)((2*(obtener_profundidad ((devolver_ramas x)!!1)))+1))++(if(prim>0) then "view(3);hold on;" else "")++"\n"++(indentar ntab)++
																							 (obtener_lineas 0 m ((devolver_ramas x)!!2) ntab) 
						|((obtener_regla_aplicable x)=="lerroak -> BSPLINE ZENB lista_id lerroak")="k="++(show (devolver_valor ((devolver_ramas x)!!1)))++";n="++(show (obtener_profundidad ((devolver_ramas x)!!2)))++";\n"++(indentar ntab)++
																							 (obtener_matriz_B ((devolver_ramas x)!!2) m)++(indentar ntab)++
																							 (obtener_ptos_inter_bspline2 (devolver_valor ((devolver_ramas x)!!1)) ((obtener_profundidad ((devolver_ramas x)!!2))-1) 20 "0.05" ntab)++
																							 (if(prim>0) then "subplot(1,2,"++(show prim)++");" else "")++
																							 (dibujar_curva ((devolver_ramas x)!!2) (2*(obtener_profundidad ((devolver_ramas x)!!2))))++(if(prim>0) then "view(3);hold on;" else "")++"\n"++(indentar ntab)++
																							 (obtener_lineas 0 m ((devolver_ramas x)!!3) ntab) 
						|((obtener_regla_aplicable x)=="lerroak -> BSPLINE_IRE ZENB lista_id lerroak")="k="++(show (devolver_valor ((devolver_ramas x)!!1)))++";n="++(show (obtener_profundidad ((devolver_ramas x)!!2)))++";\n"++(indentar ntab)++
																							 (obtener_matriz_B ((devolver_ramas x)!!2) m)++(indentar ntab)++
																							 (obtener_ptos_inter_bspline_ire (devolver_valor ((devolver_ramas x)!!1)) (obtener_profundidad ((devolver_ramas x)!!2)) (2*(obtener_profundidad ((devolver_ramas x)!!2))) ntab)++
																							 (if(prim>0) then "subplot(1,2,"++(show prim)++");" else "")++
																							 (dibujar_curva ((devolver_ramas x)!!2) (2*(obtener_profundidad ((devolver_ramas x)!!2))))++(if(prim>0) then "view(3);hold on;" else "")++"\n"++(indentar ntab)++
																							 (obtener_lineas 0 m ((devolver_ramas x)!!3) ntab) 
						|((obtener_regla_aplicable x)=="lerroak -> NAHASTE_PAR lista_id lerroak")=(obtener_ctes ((devolver_ramas x)!!1) m)++(indentar ntab)++
																							 (obtener_matriz_B ((devolver_ramas x)!!1) m)++(indentar ntab)++
																							 (obtener_ptos_inter_naha_par (5*(obtener_profundidad ((devolver_ramas x)!!1))) 0 ntab)++
																							 (if(prim>0) then "subplot(1,2,"++(show prim)++");" else "")++
																							 (dibujar_curva ((devolver_ramas x)!!1) (5*(obtener_profundidad ((devolver_ramas x)!!1))))++(if(prim>0) then "view(3);hold on;" else "")++"\n"++(indentar ntab)++
																							 (obtener_lineas 0 m ((devolver_ramas x)!!2) ntab) 
						|((obtener_regla_aplicable x)=="bolumena -> ID G_IRE posizio_bektore_multsoa G_ITXI lerroak")=(obtener_lineas prim m ((devolver_ramas x)!!4) ntab)++
																													  (if(prim>0) then (establecer_limites	m) else "")																	   
						|otherwise=""
devuelve_pos_id::String->Int
devuelve_pos_id y = ord(head y)-ord('A')+1
obtener_ptos_inter_curva::Arbolsintactico->String
obtener_ptos_inter_curva x|((obtener_regla_aplicable x)=="kurba -> ZATIPAR ZENB aldaketa_segida")="P=zeros(10,3);a="++(devolver_id ((devolver_ramas x)!!1))++
														  ";thetamax=sqrt(X1(2)/a);thetamin=sqrt(X1(1)/a);dtheta=(thetamax-thetamin)/9;\nP(1,:)=[X1(1) 2*a*thetamin Z1(1)];\n"++
														  "P(2,:)=obtener_pto_parabola(a,dtheta,P(1,1),P(1,2),P(1,3));\n"++(obtener_sgte_pto_inter_curva x 3 10)++
														  (if ((obtener_regla_aplicable ((devolver_ramas x)!!2))=="aldaketa_segida -> aldaketa aldaketa_segida") then "P = P * "++(quitar_ultimo_caracter(obtener_matriz_cambios ((devolver_ramas x)!!2) "no aplica" ((devolver_ramas x)!!2)))++";\n" else "")
						  |((obtener_regla_aplicable x)=="kurba -> ZATIHIP ZENB ZENB aldaketa_segida")="P=zeros(8,3);a="++(devolver_id ((devolver_ramas x)!!1))++";b="++(devolver_id ((devolver_ramas x)!!2))++
														  ";thetamax=acosh(X1(2)/a);thetamin=acosh(X1(1)/a);dtheta=(thetamax-thetamin)/7;sd=sinh(dtheta);cd=cosh(dtheta);\nP(1,:)=[a*cd b*sd Z1(1)];\n"++
														  "P(2,:)=obtener_pto_hiperbola(a,b,cd,sd,P(1,1),P(1,2),P(1,3));\n"++(obtener_sgte_pto_inter_curva x 3 8)++
														  (if ((obtener_regla_aplicable ((devolver_ramas x)!!2))=="aldaketa_segida -> aldaketa aldaketa_segida") then "P = P * "++(quitar_ultimo_caracter(obtener_matriz_cambios ((devolver_ramas x)!!2) "no aplica" ((devolver_ramas x)!!2)))++";\n" else "")
						  |otherwise=""
obtener_sgte_pto_inter_curva::Arbolsintactico->Int->Int->String
obtener_sgte_pto_inter_curva x t inter|((obtener_regla_aplicable x)=="kurba -> ZATIPAR ZENB aldaketa_segida" && (t<=inter))="P("++(show t)++",:)=obtener_pto_parabola(a,dtheta,P("++(show (t-1))++",1),P("++(show (t-1))++",2),P("++(show (t-1))++",3));\n"++
																															(obtener_sgte_pto_inter_curva x (t+1) inter)
									  |((obtener_regla_aplicable x)=="kurba -> ZATIHIP ZENB ZENB aldaketa_segida" && (t<=inter))="P("++(show t)++",:)=obtener_pto_hiperbola(a,b,cd,sd,P("++(show (t-1))++",1),P("++(show (t-1))++",2),P("++(show (t-1))++",3));\n"++
																															(obtener_sgte_pto_inter_curva x (t+1) inter)
									  |otherwise=""																												
dibujar_curva::Arbolsintactico->Int->String
dibujar_curva x inter|((obtener_regla_aplicable x)=="lista_id -> ID rlista_id")="X1=[P(1,1) P(2,1)];Y1=[P(1,2) P(2,2)];Z1=[P(1,3) P(2,3)];line(X1,Y1,Z1);\n"++
											   (dibujar_sgte_trazo 2 inter)
				|((obtener_regla_aplicable x)=="kurba -> ZATIPAR ZENB aldaketa_segida")="X1=[P(1,1) P(2,1)];Y1=[P(1,2) P(2,2)];Z1=[P(1,3) P(2,3)];line(X1,Y1,Z1);\n"++
											   (dibujar_sgte_trazo 2 9)	
				|((obtener_regla_aplicable x)=="kurba -> ZATIHIP ZENB ZENB aldaketa_segida")="X1=[P(1,1) P(2,1)];Y1=[P(1,2) P(2,2)];Z1=[P(1,3) P(2,3)];line(X1,Y1,Z1);\n"++
											   (dibujar_sgte_trazo 2 7)												   
				|otherwise="line(X1,Y1,Z1);"
dibujar_sgte_trazo::Int->Int->String				
dibujar_sgte_trazo n tam = "X1=[P("++(show n)++",1) P("++(show (n+1))++",1)];Y1=[P("++(show n)++",2) P("++(show (n+1))++",2)];Z1=[P("++(show n)++",3) P("++(show (n+1))++",3)];line(X1,Y1,Z1);\n" ++
						   (if (n<tam) then (dibujar_sgte_trazo (n+1) tam) else "")				 
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
indentar::Int->String
indentar n|(n>0)="\t"++(indentar (n-1))
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
obtener_matriz_B::Arbolsintactico->String->String							   
obtener_matriz_B x m="B=["++m++"("++(show(devuelve_pos_id(devolver_id(head(devolver_ramas x)))))++",1) "++
							m++"("++(show(devuelve_pos_id(devolver_id(head(devolver_ramas x)))))++",2) "++ 
							m++"("++(show(devuelve_pos_id(devolver_id(head(devolver_ramas x)))))++",3);"++
						(quitar_ultimo_caracter (obtener_fila_B ((devolver_ramas x)!!1) m))++"];\n"
obtener_fila_B::Arbolsintactico->String->String
obtener_fila_B x m |((obtener_regla_aplicable x)=="rlista_id -> ID rlista_id")=m++"("++(show(devuelve_pos_id(devolver_id(head(devolver_ramas x)))))++",1) "++
																			   m++"("++(show(devuelve_pos_id(devolver_id(head(devolver_ramas x)))))++",2) "++ 
																			   m++"("++(show(devuelve_pos_id(devolver_id(head(devolver_ramas x)))))++",3);"++
																			   (obtener_fila_B ((devolver_ramas x)!!1) m)
				   |otherwise=""
obtener_ptos_inter_bspline2::Int->Int->Int->String->Int->String
obtener_ptos_inter_bspline2 k n inter h ntab="P=zeros("++(show inter)++",3);X=0:"++(show (n+k))++";j=1;\n"++(indentar ntab)++"for t=0:"++h++":"++(show (n+k))++";\n"++(indentar ntab)++
                                           "\tP(j:)=zeros(1,3);\n"++(indentar ntab)++"\tfor i=1:"++(show (n+1))++"\n"++(indentar ntab)++"\t\tP(j:)=P(j:)+B(i)*calcN(i,"++(show k)++",t,X);\n"++(indentar ntab)++
										   "\tend\n"++(indentar ntab)++"\tj=j+1;\n"++(indentar ntab)++"end\n"
obtener_ptos_inter_bspline::Int->Int->Int->Int->String
obtener_ptos_inter_bspline k n inter ntab="h=((n+1)-(k-1))/(2*n);P=zeros(length(k-1:h:n+1),3);\n"++(indentar ntab)++(obtener_fila_spline (n + 1) 0 (k - 1) inter ntab) 
obtener_fila_spline::Int->Int->Int->Int->Int->String
obtener_fila_spline n t k inter ntab|(t<=inter)="N=bspline_oinarri_funtzio_per(k-1,n,"++(show t)++","++(show inter)++");\n"++(indentar ntab)++"for i=1:n\n"++(indentar ntab)++"\tP("++(show (t + 1))++",1)=P("++(show (t + 1))++",1)+N(i,k)*B(i,1);\n"++(indentar ntab)++"\t"++
												"P("++(show (t + 1))++",2)=P("++(show (t + 1))++",2)+N(i,k)*B(i,2);\n"++(indentar ntab)++"\t"++
												"P("++(show (t + 1))++",3)=P("++(show (t + 1))++",3)+N(i,k)*B(i,3);\n"++(indentar ntab)++"end\n"++(indentar ntab)++
												(obtener_fila_spline n (t + 1) k inter ntab) 
									|otherwise=""									 
obtener_ptos_inter_bspline_ire::Int->Int->Int->Int->String
obtener_ptos_inter_bspline_ire k n inter ntab="h=(n-k+2)/(2*n);P=zeros(length(0:h:n-k+2),3);\n"++(indentar ntab)++(obtener_fila_spline_ire n 0 k inter ntab) 
obtener_fila_spline_ire::Int->Int->Int->Int->Int->String
obtener_fila_spline_ire n t k inter ntab|(t<=inter)="N=bspline_oinarri_funtzio_ire(k,n,"++(show t)++","++(show inter)++");\n"++(indentar ntab)++"for i=1:n\n"++(indentar ntab)++"\tP("++(show (t + 1))++",1)=P("++(show (t + 1))++",1)+N(i,k)*B(i,1);\n"++(indentar ntab)++"\t"++
													"P("++(show (t + 1))++",2)=P("++(show (t + 1))++",2)+N(i,k)*B(i,2);\n"++(indentar ntab)++"\t"++
													"P("++(show (t + 1))++",3)=P("++(show (t + 1))++",3)+N(i,k)*B(i,3);\n"++(indentar ntab)++"end\n"++(indentar ntab)++
													(obtener_fila_spline_ire n (t + 1) k inter ntab) 
										|otherwise=""									 
obtener_ctes::Arbolsintactico->String->String
obtener_ctes x m = "c21=sqrt(("++m++"("++(show (devuelve_pos_id (devolver_id (obten_pto 2 x))))++",1)-"++m++"("++(show (devuelve_pos_id (devolver_id (obten_pto 1 x))))++",1))^2+("++
								 m++"("++(show (devuelve_pos_id (devolver_id (obten_pto 2 x))))++",2)-"++m++"("++(show (devuelve_pos_id (devolver_id (obten_pto 1 x))))++",2))^2+("++
								 m++"("++(show (devuelve_pos_id (devolver_id (obten_pto 2 x))))++",3)-"++m++"("++(show (devuelve_pos_id (devolver_id (obten_pto 1 x))))++",3))^2);\n"++
				   "c32=sqrt(("++m++"("++(show (devuelve_pos_id (devolver_id (obten_pto 3 x))))++",1)-"++m++"("++(show (devuelve_pos_id (devolver_id (obten_pto 2 x))))++",1))^2+("++
								 m++"("++(show (devuelve_pos_id (devolver_id (obten_pto 3 x))))++",2)-"++m++"("++(show (devuelve_pos_id (devolver_id (obten_pto 2 x))))++",2))^2+("++
								 m++"("++(show (devuelve_pos_id (devolver_id (obten_pto 3 x))))++",3)-"++m++"("++(show (devuelve_pos_id (devolver_id (obten_pto 2 x))))++",3))^2);\n"++			 
				   "c43=sqrt(("++m++"("++(show (devuelve_pos_id (devolver_id (obten_pto 4 x))))++",1)-"++m++"("++(show (devuelve_pos_id (devolver_id (obten_pto 3 x))))++",1))^2+("++
								 m++"("++(show (devuelve_pos_id (devolver_id (obten_pto 4 x))))++",2)-"++m++"("++(show (devuelve_pos_id (devolver_id (obten_pto 3 x))))++",2))^2+("++
								 m++"("++(show (devuelve_pos_id (devolver_id (obten_pto 4 x))))++",3)-"++m++"("++(show (devuelve_pos_id (devolver_id (obten_pto 3 x))))++",3))^2);\n"++								 
					"alfa=c21/(c32+c21);beta=c32/(c43+c32);\n"
obten_pto::Int->Arbolsintactico->Arbolsintactico
obten_pto n x|(n==1)=(head (devolver_ramas x))
			 |(n>1)=(obten_pto (n-1) ((devolver_ramas x)!!1))
obtener_ptos_inter_naha_par::Int->Int->Int->String			 
obtener_ptos_inter_naha_par n t ntab|(t<=n)="P("++(show (t+1))++",:)=nahaste_par(alfa,beta,"++(show t)++","++(show n)++",B);\n"++(indentar ntab)++(obtener_ptos_inter_naha_par n (t+1) ntab)
									|otherwise=""			 
recalcular_posiciones::String->Arbolsintactico->String																								  
recalcular_posiciones m x |((obtener_regla_aplicable x)=="lerroak -> P_IRE ID ID kurba P_ITXI lerroak")=(recalcular_curva m (show (devuelve_pos_id (devolver_id ((devolver_ramas x)!!1)))) (show (devuelve_pos_id (devolver_id ((devolver_ramas x)!!2)))) ((devolver_ramas x)!!3)) ++		
																										(recalcular_posiciones m ((devolver_ramas x)!!5))  
						  |((obtener_regla_aplicable x)=="lerroak -> SPLINE lista_id lerroak")=(recalcular_posiciones m ((devolver_ramas x)!!2)) 
						  |((obtener_regla_aplicable x)=="lerroak -> BEZIER lista_id lerroak")=(recalcular_posiciones m ((devolver_ramas x)!!2)) 
						  |((obtener_regla_aplicable x)=="lerroak -> BSPLINE ZENB lista_id lerroak")=(recalcular_posiciones m ((devolver_ramas x)!!3))
						  |((obtener_regla_aplicable x)=="lerroak -> NAHASTE_PAR lista_id lerroak")=(recalcular_posiciones m ((devolver_ramas x)!!2))
						  |((obtener_regla_aplicable x)=="lerroak -> BSPLINE_IRE ZENB lista_id lerroak")=(recalcular_posiciones m ((devolver_ramas x)!!3))						  
						  |otherwise=""																						  
recalcular_curva::String->String->String->Arbolsintactico->String				 
recalcular_curva m p1 p2 x |((obtener_regla_aplicable x)=="kurba -> ZATIHIP ZENB ZENB aldaketa_segida")="a="++(devolver_id ((devolver_ramas x)!!1))++";b="++(devolver_id ((devolver_ramas x)!!2))++
														  "P=["++m++"("++p2++",1) "++m++"("++p2++",2) "++m++"("++p2++",3)];\n"++
														  "thetamax=acosh(P(2)/a);P(2)=b*sinh(thetamax);\n"++														  
														  (if ((obtener_regla_aplicable ((devolver_ramas x)!!3))=="aldaketa_segida -> aldaketa aldaketa_segida") then ("P = P * "++(quitar_ultimo_caracter(obtener_matriz_cambios ((devolver_ramas x)!!3) "no aplica" ((devolver_ramas x)!!3)))++";\n") else "")++
														   m++"("++p2++",2) = P(2);\n"
						   |((obtener_regla_aplicable x)=="kurba -> ZATIPAR ZENB aldaketa_segida")="a="++(devolver_id ((devolver_ramas x)!!1))++
														  ";P=["++m++"("++p2++",1) "++m++"("++p2++",2) "++m++"("++p2++",3)];\n"++
														  "thetamax=sqrt(P(1)/a);P(2)=2*a*thetamax;\n"++
														  (if ((obtener_regla_aplicable ((devolver_ramas x)!!2))=="aldaketa_segida -> aldaketa aldaketa_segida") then "P = P * "++(quitar_ultimo_caracter(obtener_matriz_cambios ((devolver_ramas x)!!2) "no aplica" ((devolver_ramas x)!!2)))++";\n" else "")++
														   m++"("++p2++",2) = P(2);\n"++
														  "P=["++m++"("++p1++",1) "++m++"("++p1++",2) "++m++"("++p1++",3)];\n"++
														  "thetamin=sqrt(P(1)/a);P(2)=2*a*thetamin;\n"++
														  (if ((obtener_regla_aplicable ((devolver_ramas x)!!2))=="aldaketa_segida -> aldaketa aldaketa_segida") then "P = P * "++(quitar_ultimo_caracter(obtener_matriz_cambios ((devolver_ramas x)!!2) "no aplica" ((devolver_ramas x)!!2)))++";\n" else "")++
														   m++"("++p1++",2) = P(2);\n"
						   |otherwise=""  
																																																																			
establecer_limites::String->String
establecer_limites m = "xlim([min("++m++"(:,1))-1 max("++m++"(:,1))+1]);ylim([min("++m++"(:,2))-1 max("++m++"(:,2))+1]);zlim([min("++m++"(:,3))-1 max("++m++"(:,3))+1]);xlabel('x');ylabel('y');zlabel('z');"
obtener_nuevo_id::Arbolsintactico->String
obtener_nuevo_id x |((obtener_regla_aplicable x)=="transformatu -> ID ESLEI ID aldaketa_segida")=(devolver_id (head(devolver_ramas x)))																													  
obtener_trans::Arbolsintactico->Arbolsintactico->String
obtener_trans x z|((obtener_regla_aplicable x)=="transformatu -> ID ESLEI ID aldaketa_segida")=(obtener_vector_cos_directores ((devolver_ramas x)!!3))++(devolver_id (head(devolver_ramas x)))++" = "++(devolver_id ((devolver_ramas x)!!2))++
																		  (if((obtener_regla_aplicable (head(devolver_ramas ((devolver_ramas x)!!3))))=="aldaketa -> BIRATU_OSOA ardatza") then ";\n" else if((obtener_regla_aplicable (head(devolver_ramas ((devolver_ramas x)!!3))))=="aldaketa -> EKORKETA aldaketa_segida P_COMA") then ";\n" else "*")++
																		  (quitar_ultimo_caracter(obtener_matriz_cambios ((devolver_ramas x)!!3) (devolver_id (head(devolver_ramas x))) z))	   
obtener_matriz_cambios::Arbolsintactico->String->Arbolsintactico->String
obtener_matriz_cambios x y z|((obtener_regla_aplicable x)=="aldaketa_segida -> aldaketa aldaketa_segida")=(obtener_matriz_cambios (head(devolver_ramas x)) y z)++"*"++(obtener_matriz_cambios ((devolver_ramas x)!!1) y z)
						 |((obtener_regla_aplicable x)=="aldaketa -> BIRAKETA ardatza ZENB")=if(obtener_eje ((devolver_ramas x)!!1))=="x" then (matriz_giro_x (devolver_id ((devolver_ramas x)!!2))) else
																							 if(obtener_eje ((devolver_ramas x)!!1))=="y" then (matriz_giro_y (devolver_id ((devolver_ramas x)!!2))) else 
																																			   (matriz_giro_z (devolver_id ((devolver_ramas x)!!2)))
						 |((obtener_regla_aplicable x)=="aldaketa -> LEKU_ALDATZEA posizio_bektorea")="[1 0 0 0;0 1 0 0;0 0 1 0;"++(obtener_coord ((devolver_ramas x)!!1) 2)++" "++(obtener_coord ((devolver_ramas x)!!1) 3)++" "++(obtener_coord ((devolver_ramas x)!!1) 4)++" 1]"																														   
						 |((obtener_regla_aplicable x)=="aldaketa -> ARDATZAREN_INGURUKO_BIRAKETA posizio_bektorea posizio_bektorea ZENB")="[1 0 0 0;0 1 0 0;0 0 1 0;"++(obtener_coord ((devolver_ramas x)!!1) 2)++" "++(obtener_coord ((devolver_ramas x)!!1) 3)++" "++(obtener_coord ((devolver_ramas x)!!1) 4)++" 1]*"++
																		  (matriz_giro_x ("acos(C(2)/sqrt(C(1)^2+C(2)^2))"))++"*"++
																		  (matriz_giro_y ("-asin(C(1))"))++"*"++(matriz_giro_z (devolver_id ((devolver_ramas x)!!3)))++
																		  "*inv("++(matriz_giro_y ("-asin(C(1))"))++")*inv("++(matriz_giro_x ("acos(C(2)/sqrt(C(1)^2+C(2)^2))"))++")*inv("++
																		  "[1 0 0 0;0 1 0 0;0 0 1 0;"++(obtener_coord ((devolver_ramas x)!!1) 2)++" "++(obtener_coord ((devolver_ramas x)!!1) 3)++" "++(obtener_coord ((devolver_ramas x)!!1) 4)++" 1])"
					   |((obtener_regla_aplicable x)=="aldaketa -> BIRATU_OSOA ardatza")="for i=0:35\n\t"++y++" = "++y++" * "++(if(obtener_eje ((devolver_ramas x)!!1))=="x" then (matriz_giro_x "10") else (matriz_giro_y "10"))++";\n\t"++
																						 (obtener_lineas 0 y z 1)++"\nend\n"++y++" = "++y++" * "++(if(obtener_eje ((devolver_ramas x)!!1))=="x" then (matriz_giro_x "10") else (matriz_giro_y "10"))
					   |((obtener_regla_aplicable x)=="aldaketa -> EKORKETA aldaketa_segida P_COMA")="for i=0:95\n\t"++y++" = "++y++" * "++(quitar_ultimo_caracter (obtener_matriz_cambios ((devolver_ramas x)!!1) y z))++";\n\t"++
																						 (obtener_lineas 0 y z 1)++"\nend\n"
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
matriz_giro_x angulo = "[1 0 0 0;0 cosd("++angulo++") sind("++angulo++") 0;0 -sind("++angulo++") cosd("++angulo++") 0;0 0 0 1 ]" 
matriz_giro_y::String->String							 
matriz_giro_y angulo = "[cosd("++angulo++") sind("++angulo++") 0 0;-sind("++angulo++") cosd("++angulo++") 0 0;0 0 1 0;0 0 0 1 ]" 
matriz_giro_z::String->String							 
matriz_giro_z angulo = "[cosd("++angulo++") 0 -sind("++angulo++") 0;0 1 0 0;sind("++angulo++") 0 cosd("++angulo++") 0;0 0 0 1 ]" 
tipo_trans::Arbolsintactico->String
tipo_trans x |((obtener_regla_aplicable x)=="transformatu -> ID ESLEI ID aldaketa_segida")=tipo_trans ((devolver_ramas x)!!3)
			 |((obtener_regla_aplicable x)=="aldaketa_segida -> aldaketa aldaketa_segida")=(if (obtener_regla_aplicable (head(devolver_ramas x)))=="aldaketa -> BIRATU_OSOA ardatza" || (obtener_regla_aplicable (head(devolver_ramas x)))=="aldaketa -> EKORKETA aldaketa_segida P_COMA" then "superficie" else tipo_trans ((devolver_ramas x)!!1))
			 |otherwise=""
main = do
		h1 <- openFile "prog_bspline_gainazala.txt" ReadMode
		y  <- hGetContents h1
		--putStrLn (parser_slr y [1])
		writeFile "ekorketa_bspline.m" (obtener_programa(parser_slr_arbol y [1] []))
		hClose h1		
