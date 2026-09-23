000100 01  HAEN-WDN527.                                                         
000200*                                 KATALOG AVSNITTSREGISTER                
000300*                                 HÄNVISNING-SEGMENT KOPPLING             
000400*                                 FYSISK NYCKEL: WDN527KY                 
000500*                                 (IDCATNR + IDCATGRP + IDCATAVS          
000600*                                  + IDCATRAD + KDCATPUB-FOM)             
000700     03 HAEN-IDCATRKY.                                                    
000800*                                 NYCKEL TILL KATALOGRAD                  
000900        05 HAEN-IDCATNR      PIC 9(5).                                    
001000*                                 KATALOG-ID                              
001100*                                 CATALOG-ID                              
001200        05 HAEN-IDCATGRP     PIC 9(2).                                    
001300*                                 KATALOG-GRUPP                           
001400*                                 CATALOG-GROUP                           
001500        05 HAEN-IDCATAVS     PIC 9(4).                                    
001600*                                 KATALOG-AVSNITT                         
001700*                                 CATALOG TEXT BLOCK                      
001800        05 HAEN-IDCATRAD     PIC 9(4).                                    
001900*                                 RADNUMMER                               
002000*                                 ROW NUMBER IN TEXT BLOCK                
002100        05 HAEN-KDCATPUB-FOM PIC X(6).                                    
002200*                                 PUBLICERINGS TIDKOD, F.O.M.             
002300*                                 RELEASE TIME CODE, FROM                 
002400     03 HAEN-KDHAEN          PIC X.                                       
002500*                                 HÄNVISNINGSKODER VÄRDEN:                
002600*                                  A = HÄNVISNING I ANM. FÄLTET           
002700*                                  B = HÄNVISNING I BEN. FÄLTET           
002800*                                  * = HÄNVISNING, EJ KLAR DEST.          
002900*                                  SPACE = INGEN HÄNVISNING               
003000*                                 REFERENCE CODE   VALUES:                
003100*                                  A = REFERENCE IN NOTE-FIELD            
003200*                                  B = REFERENCE IN DESCR-FIELD           
003300*                                  * = REFERENCE DURING EDITING           
003400*                                  SPACE = NO REFERENCE                   
003500*** END OF VILMAII-COPY LENGTH= 22 BYTES                                  
