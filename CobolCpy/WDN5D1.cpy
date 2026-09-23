000100 01  AVSD-WDN5D1.                                                         
000200*                                 KATALOG AVSNITTSREGISTER                
000300*                                 SECONDARY INDEX PÅ                      
000400*                                           TILLÄGGSTEXT                  
000500*                                 FYSISK NYCKEL:  WDN5D1KY                
000600*                                 (IDTTEXNR + IDCATNR                     
000700*                                  + IDCATGRP + IDCATAVS                  
000800*                                  + IDCATRAD + IDCATPUB)                 
000900*                                 SECONDARY NYCKEL: WDN5DSEQ              
001000*                                 (IDTTEXNR)                              
001100     03 AVSD-IDTTEXNR        PIC S9(5)           COMP-3.                  
001200*                                 TILLÄGGSTEXT-NR                         
001300*                                 ADDITIONAL TEXT, ID NUMBER              
001400     03 AVSD-IDCATRKY.                                                    
001500*                                 NYCKEL TILL KATALOGRAD                  
001600        05 AVSD-IDCATNR      PIC 9(5).                                    
001700*                                 KATALOG-ID                              
001800*                                 CATALOG-ID                              
001900        05 AVSD-IDCATGRP     PIC 9(2).                                    
002000*                                 KATALOG-GRUPP                           
002100*                                 CATALOG-GROUP                           
002200        05 AVSD-IDCATAVS     PIC 9(4).                                    
002300*                                 KATALOG-AVSNITT                         
002400*                                 CATALOG TEXT BLOCK                      
002500        05 AVSD-IDCATRAD     PIC 9(4).                                    
002600*                                 RADNUMMER                               
002700*                                 ROW NUMBER IN TEXT BLOCK                
002800        05 AVSD-KDCATPUB-FOM PIC X(6).                                    
002900*                                 PUBLICERINGS TIDKOD, F.O.M.             
003000*                                 RELEASE TIME CODE, FROM                 
003100*** END OF VILMAII-COPY LENGTH= 24 BYTES                                  
