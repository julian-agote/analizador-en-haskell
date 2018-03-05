module Scaner_jkl3(sigToken,buscaSigToken) where 
import Data.Char(ord,chr)
import Utilidades
palabra_clave::String->String
palabra_clave x |x=="program"="P_PROG"
                |x=="var"= "PVAR"
                |x=="begin"="PBEGIN"
                |x=="end"="PEND"
                |x=="if"="PIF"
                |x=="then"="PTHEN"
                |x=="else"="PELSE"
                |x=="while"="PWHILE"
                |x=="do"="PDO"
                |x=="read"="PREAD"
                |x=="writec"="PWRITC"
                |x=="write"="PWRITE"
                |x=="writeln"="PWRITL"
                |x=="for"="PFORP"
                |x=="to"="PTODO"
                |x=="downto"="PTODO"
buscaSigToken::String->String->(String,String)
buscaSigToken "" "" =("$","")
buscaSigToken "" y = if(ord(head y)>=ord('0') && ord(head y)<=ord('9')) then ("NUM",y) else if (rtrim y)=="and" then ("OP_AND","and") else if (rtrim y)=="or" then ("OP_OR","or") else if (rtrim y)=="not" then ("OP_NOT","not") else if (rtrim y)`elem` ["program","var","begin","end","if","then","else","while","do","read","writec","write","writeln","for","to","downto"] then ((palabra_clave (rtrim y)),(rtrim y)) else ("ID",y)
buscaSigToken x y |((head x)=='\n'||(head x)==' ') && (trim y)==""=(buscaSigToken  (tail x) "")
                  |(substr x 1 2)=="//" && (trim y)==""=buscaSigToken (substr x 3 (length x)) (y++(substr x 1 2))
                  |(head x)/='\n' && (trim y)/="" && (head y)=='/'=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
                  |(head x)=='\n' && (trim y)/="" && (head y)=='/'=(buscaSigToken  (tail x) "")
                  |(head x)=='(' && (trim y)==""=("PAR_ABR",(substr x 1 1))
                  |(head x)==')' && (trim y)==""=("PAR_CER",(substr x 1 1))
                  |(head x)==';' && (trim y)==""=("P_COMA",(substr x 1 1))
                  |(head x)=='.' && (trim y)==""=("PUNTO",(substr x 1 1))
                  |(head x)=='+' && (trim y)==""=("OP_ADIT",(substr x 1 1))
                  |(head x)=='-' && (trim y)==""=("OP_ADIT",(substr x 1 1))
                  |(head x)=='*' && (trim y)==""=("OP_MULT",(substr x 1 1))
                  |(head x)=='/' && (trim y)==""=("OP_MULT",(substr x 1 1))
                  |(head x)=='%' && (trim y)==""=("OP_MULT",(substr x 1 1))
                  |(substr x 1 2)=="<=" && (trim y)==""=("OP_REL",(substr x 1 2))
                  |(substr x 1 2)==">=" && (trim y)==""=("OP_REL",(substr x 1 2))
                  |(substr x 1 2)=="==" && (trim y)==""=("OP_REL",(substr x 1 2))
                  |(substr x 1 2)=="<>" && (trim y)==""=("OP_REL",(substr x 1 2))
                  |(head x)=='>' && (trim y)==""=("OP_REL",(substr x 1 1))
                  |(head x)=='<' && (trim y)==""=("OP_REL",(substr x 1 1))
                  |(substr x 1 2)==":=" && (trim y)==""=("ASIGN",(substr x 1 2))
                  |(head x)=='"' && (trim y)==""=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
                  |(head x)/='"' && (trim y)/="" && (head y)=='"'=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
                  |(head x)=='"' && (trim y)/=""=("CAD",(y++(substr x 1 1)))
                  |(null y ||(head y)`elem` (['a'..'z']++['A'..'Z']))&&((ord(head x)>=ord('a') && ord(head x)<=ord('z'))||(ord(head x)>=ord('A') && ord(head x)<=ord('Z'))||(ord(head x)>=ord('0') && ord(head x)<=ord('9'))||(head x)=='-'||(head x)=='_')=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
                  |(null y ||(head y)`elem` ['0'..'9'])&&(ord(head x)>=ord('0') && ord(head x)<=ord('9'))=buscaSigToken (substr x 2 (length x)) (y++(substr x 1 1))
                  |otherwise = if(ord(head y)>=ord('0') && ord(head y)<=ord('9')) then ("NUM",y) else if (rtrim y)=="and" then ("OP_AND","and") else if (rtrim y)=="or" then ("OP_OR","or") else if (rtrim y)=="not" then ("OP_NOT","not") else  if (rtrim y)`elem` ["program","var","begin","end","if","then","else","while","do","read","writec","write","writeln","for","to","downto"] then ((palabra_clave (rtrim y)),(rtrim y)) else ("ID",y)
sigToken::String->(String,String)
sigToken "" = ("$","")
sigToken x = (buscaSigToken x "")
