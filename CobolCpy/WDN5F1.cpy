000100 01  AVSF-WDN5F1.                                                         
000200*                                 KATALOG AVSNITTSREGISTER                
000300*                                 SECONDARY INDEX PÅ FOTNOT               
000400*                                 FYSISK NYCKEL:  WDN5F1KY                
000500*                                 (IDFOTNR + IDCATNR                      
000600*                                  + IDCATGRP + IDCATAVS                  
000700*                                  + IDCATRAD + IDCATPUB)                 
000800*                                 SECONDARY NYCKEL: WDN5FSEQ              
000900*                                 (IDFOTNR)                               
001000     03 AVSF-IDFOTNR         PIC S9(5)           COMP-3.                  
001100*                                 FOTNOTSNUMMER                           
001200*                                 FOOT NOTE ID NUMBER                     
001300     03 AVSF-IDCATRKY.                                                    
001400*                                 NYCKEL TILL KATALOGRAD                  
001500        05 AVSF-IDCATNR      PIC 9(5).                                    
001600*                                 KATALOG-ID                              
001700*                                 CATALOG-ID                              
001800        05 AVSF-IDCATGRP     PIC 9(2).                                    
001900*                                 KATALOG-GRUPP                           
002000*                                 CATALOG-GROUP                           
002100        05 AVSF-IDCATAVS     PIC 9(4).                                    
002200*                                 KATALOG-AVSNITT                         
002300*                                 CATALOG TEXT BLOCK                      
002400        05 AVSF-IDCATRAD     PIC 9(4).                                    
002500*                                 RADNUMMER                               
002600*                                 ROW NUMBER IN TEXT BLOCK                
002700        05 AVSF-KDCATPUB-FOM PIC X(6).                                    
002800*                                 PUBLICERINGS TIDKOD, F.O.M.             
002900*                                 RELEASE TIME CODE, FROM                 
003000*** END OF VILMAII-COPY LENGTH= 24 BYTES                                  
