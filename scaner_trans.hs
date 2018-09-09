module Scaner_trans(sigToken,buscaSigToken) where 
import Data.Char(ord,chr)
import Utilidades
palabra_clave::String->String
palabra_clave x |(x=="mozketa")="MOZKETA"
                |(x=="biraketa")= "BIRAKETA"
                |(x=="islapena")="ISLAPENA"
                |(x=="leku_aldatzea")="LEKU_ALDATZEA"
                |(x=="hautazko_ardatz_baten_inguruko_biraketa")="ARDATZAREN_INGURUKO_BIRAKETA"
                |(x=="hiperbola")="ZATIHIP"
                |(x=="parabola")="ZATIPAR"
                |(x=="spline")="SPLINE"
buscaSigToken::String->String->(String,String)
buscaSigToken "" "" =("$","")
buscaSigToken "" y = if((ord(head y)>=ord('0') && ord(head y)<=ord('9'))||ord(head y)==ord('-')) then ("ZENB",y) else if (rtrim y)`elem` ["mozketa","biraketa","islapena","leku_aldatzea","hautazko_ardatz_baten_inguruko_biraketa","hiperbola","parabola","spline"] then ((palabra_clave (rtrim y)),(rtrim y)) else ("ID",y)
buscaSigToken x y |((head x)=='\n'||(head x)==' ') && (trim y)==""=(buscaSigToken  (tail x) "")
                  |(substr x 1 2)=="//" && (trim y)==""=buscaSigToken (substr x 3 (length x)) (y++(substr x 1 2))
                  |(head x)/='\n' && (trim y)/="" && (head y)=='/'=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
                  |(head x)=='\n' && (trim y)/="" && (head y)=='/'=(buscaSigToken  (tail x) "")
                  |(head x)=='(' && (trim y)==""=("P_IRE",(substr x 1 1))
                  |(head x)==')' && (trim y)==""=("P_ITXI",(substr x 1 1))
                  |(head x)=='=' && (trim y)==""=("ESLEI",(substr x 1 1))
                  |(head x)==';' && (trim y)==""=("P_COMA",(substr x 1 1))
                  |(head x)=='[' && (trim y)==""=("G_IRE",(substr x 1 1))
                  |(head x)==']' && (trim y)==""=("G_ITXI",(substr x 1 1))
                  |(substr x 1 2)=="x " && (trim y)==""=("P_X",(substr x 1 1))
                  |(substr x 1 2)=="y " && (trim y)==""=("P_Y",(substr x 1 1))
                  |(substr x 1 2)=="z " && (trim y)==""=("P_Z",(substr x 1 1))
                  |(substr x 1 2)=="b " && (trim y)==""=("P_B",(substr x 1 1))
                  |(substr x 1 2)=="c " && (trim y)==""=("P_C",(substr x 1 1))
                  |(substr x 1 2)=="d " && (trim y)==""=("P_D",(substr x 1 1))
                  |(substr x 1 2)=="f " && (trim y)==""=("P_F",(substr x 1 1))
                  |(substr x 1 2)=="g " && (trim y)==""=("P_G",(substr x 1 1))
                  |(substr x 1 2)=="h " && (trim y)==""=("P_H",(substr x 1 1))
                  |(null y ||(head y)`elem` (['a'..'z']++['A'..'Z']))&&((ord(head x)>=ord('a') && ord(head x)<=ord('z'))||(ord(head x)>=ord('A') && ord(head x)<=ord('Z'))||(ord(head x)>=ord('0') && ord(head x)<=ord('9'))||(head x)=='-'||(head x)=='_')=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
                  |(null y ||(head y)`elem` ['0'..'9']||ord(head y)==ord('-'))&&((ord(head x)>=ord('0') && ord(head x)<=ord('9'))||ord(head x)==ord('.')||ord(head x)==ord('-'))=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
                  |otherwise = if((ord(head y)>=ord('0') && ord(head y)<=ord('9'))||ord(head y)==ord('-')) then ("ZENB",y) else if (rtrim y)`elem` ["mozketa","biraketa","islapena","leku_aldatzea","hautazko_ardatz_baten_inguruko_biraketa","hiperbola","parabola","spline"] then ((palabra_clave (rtrim y)),(rtrim y)) else ("ID",y)
sigToken::String->(String,String)
sigToken "" = ("$","")
sigToken x = (buscaSigToken x "")
