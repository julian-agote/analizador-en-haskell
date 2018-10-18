Hemen agertzen diren programak, haskell-en idatzitako interprete txiki batenak dira. Interprete honek, marrazkietan aldaketak adierazten dituen (asmatutako gramatikatik) fitxategi batetik (adibidez prog_bspline.txt), matlab-en idatzitako programak ateratzen ditu, eta marrazkiak egiteko erabiltzen da. Helburua aipatu dugun interpretearen laguntzarekin, infografiara-ko frogak egitea da. Kurba desberdinak probatu, aldaketak egin definitutako kurba eta lerroetan. Jarraipena badut gainazalak gehituz eta abar.
Hurrengo programak exekutatzeko jarraitu behar diren pausuak, nik behintzat hurrengo komandoak erabiltzen ditut:
(aurretik haskell compiladore bat instalatuta eduki behar da https://www.haskell.org/downloads)
Behin haskellen interpretea martxan jarrita, lehenengotik mugitu behar da gure programak dauden disko gogorreko direktoriora (:cd )
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
Prelude> :cd D:\trabajo\haskell
Prelude>
Gero mementoan dudan gramatikarako (grama_trans.txt) parser-a lortu behar da, horretarako geLRco.hs egikaritu behar da, parametro zuzenak pasatuz. Komandoak hurrengoak dira:
Prelude> :set args ["grama_trans.txt","parser_trans.hs"]
Prelude> :load geLRco
[2 of 2] Compiling Main             ( geLRco.hs, interpreted )
Ok, modules loaded: Utilidades (Utilidades.o), Main.
*Main> main
Orain gure gramatikarako parser-a badaukagu (analizatzaile sintaktikoa), falta zaiguna eskaner-a scaner_trans.hs gure aldetik idatzita, tokenak lortzeko eta erregela semantikoen programa, hemen deitu diodana prueba_parser_trans.hs. Azken hau irakurtzen du gure gramatikan idatzitako programak (adibidez prog_bspline.txt) eta matlab-era itzulitakoak ateratzen du (aldaketak_bspline.m). Kasu onetan ez ditut parametroen bidez izenak pasatzen, zuzenean prueba_parser_trans.hs daude kodetuta. 
Beraz azkenengo komandoa hurrengoa izango da:
*Main> :load prueba_parser_trans
[3 of 5] Compiling Scaner_trans     ( Scaner_trans.hs, interpreted )
[4 of 5] Compiling Parser_trans     ( Parser_trans.hs, interpreted )
[5 of 5] Compiling Main             ( prueba_parser_trans.hs, interpreted )
Ok, modules loaded: Utilidades (Utilidades.o), Main, Parser_trans, Arbol (Arbol.
o), Scaner_trans.
*Main> main
Orain joan daiteke bilatzera gure direktorioan matlab-eko fitxategia (aldaketak_bspline.m).
