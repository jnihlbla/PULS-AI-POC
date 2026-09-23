000100 01  AVSB-WDN5B1.                                                         
000200*                                 KATALOG AVSNITTSREGISTER                
000300*                                 SECONDARY INDEX PÅ ARTIKEL              
000400*                                 FYSISK NYCKEL:  WDN5B1KY                
000500*                                 (IDARTNR + IDCATNR                      
000600*                                  + IDCATGRP + IDCATAVS                  
000700*                                  + IDCATRAD + KDCATPUB-FOM)             
000800*                                 SECONDARY NYCKEL: WDN5BSEQ              
000900*                                 (IDARTNR)                               
001000     03 AVSB-IDARTNR         PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200*                                 PART NUMBER                             
001300     03 AVSB-IDCATRKY.                                                    
001400*                                 NYCKEL TILL KATALOGRAD                  
001500        05 AVSB-IDCATNR      PIC 9(5).                                    
001600*                                 KATALOG-ID                              
001700*                                 CATALOG-ID                              
001800        05 AVSB-IDCATGRP     PIC 9(2).                                    
001900*                                 KATALOG-GRUPP                           
002000*                                 CATALOG-GROUP                           
002100        05 AVSB-IDCATAVS     PIC 9(4).                                    
002200*                                 KATALOG-AVSNITT                         
002300*                                 CATALOG TEXT BLOCK                      
002400        05 AVSB-IDCATRAD     PIC 9(4).                                    
002500*                                 RADNUMMER                               
002600*                                 ROW NUMBER IN TEXT BLOCK                
002700        05 AVSB-KDCATPUB-FOM PIC X(6).                                    
002800*                                 PUBLICERINGS TIDKOD, F.O.M.             
002900*                                 RELEASE TIME CODE, FROM                 
003000*** END OF VILMAII-COPY LENGTH= 26 BYTES                                  
