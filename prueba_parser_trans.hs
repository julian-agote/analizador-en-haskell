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
obtener_lineas prim m x |((obtener_regla_aplicable x)=="lerroak -> P_IRE ID ID P_ITXI lerroak")="X1=["++m++"("++(show (devuelve_pos_id (devolver_id ((devolver_ramas x)!!1))))++",1) "++
                                                                                                        m++"("++(show (devuelve_pos_id (devolver_id ((devolver_ramas x)!!2))))++",1)];"++
                                                                                                "Y1=["++m++"("++(show (devuelve_pos_id (devolver_id ((devolver_ramas x)!!1))))++",2) "++
                                                                                                        m++"("++(show (devuelve_pos_id (devolver_id ((devolver_ramas x)!!2))))++",2)];"++
                                                                                                "Z1=["++m++"("++(show (devuelve_pos_id (devolver_id ((devolver_ramas x)!!1))))++",3) "++
                                                                                                        m++"("++(show (devuelve_pos_id (devolver_id ((devolver_ramas x)!!2))))++",3)];\n"++
                                                                                                (if(prim>0) then "subplot(1,2,"++(show prim)++");" else "")++
                                                                                                "line(X1,Y1,Z1);"++(if(prim>0) then "view(3);hold on;" else "")++"\n"++
                                                                                                (obtener_lineas 0 m ((devolver_ramas x)!!4))                                                                                                  
                        |((obtener_regla_aplicable x)=="bolumena -> ID G_IRE posizio_bektore_multsoa G_ITXI lerroak")=(obtener_lineas prim m ((devolver_ramas x)!!4))++
                                                                                                                      (establecer_limites  m)                                                                      
                        |otherwise=""
devuelve_pos_id::String->Int
devuelve_pos_id y = ord(head y)-ord('A')+1  
establecer_limites::String->String
establecer_limites m = "xlim([min("++m++"(:,1)) max("++m++"(:,1))]);ylim([min("++m++"(:,2)) max("++m++"(:,2))]);zlim([min("++m++"(:,3)) max("++m++"(:,3))]);xlabel('x');ylabel('y');zlabel('z');"
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
        putStrLn (obtener_programa(parser_slr_arbol y [1] []))
        hClose h1       
