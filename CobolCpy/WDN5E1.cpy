000100 01  AVSE-WDN5E1.                                                         
000200*                                 KATALOG AVSNITTSREGISTER                
000300*                                 SECONDARY INDEX PÅ RUBRIK               
000400*                                 FYSISK NYCKEL:  WDN5E1KY                
000500*                                 (IDRUBNR + IDCATNR                      
000600*                                  + IDCATGRP + IDCATAVS                  
000700*                                  + IDCATRAD + IDCATPUB)                 
000800*                                 SECONDARY NYCKEL: WDN5ESEQ              
000900*                                 (IDRUBNR)                               
001000     03 AVSE-IDRUBNR         PIC S9(5)           COMP-3.                  
001100*                                 RUBRIKNUMMER                            
001200*                                 HEADLINE ID NUMBER                      
001300     03 AVSE-IDCATRKY.                                                    
001400*                                 NYCKEL TILL KATALOGRAD                  
001500        05 AVSE-IDCATNR      PIC 9(5).                                    
001600*                                 KATALOG-ID                              
001700*                                 CATALOG-ID                              
001800        05 AVSE-IDCATGRP     PIC 9(2).                                    
001900*                                 KATALOG-GRUPP                           
002000*                                 CATALOG-GROUP                           
002100        05 AVSE-IDCATAVS     PIC 9(4).                                    
002200*                                 KATALOG-AVSNITT                         
002300*                                 CATALOG TEXT BLOCK                      
002400        05 AVSE-IDCATRAD     PIC 9(4).                                    
002500*                                 RADNUMMER                               
002600*                                 ROW NUMBER IN TEXT BLOCK                
002700        05 AVSE-KDCATPUB-FOM PIC X(6).                                    
002800*                                 PUBLICERINGS TIDKOD, F.O.M.             
002900*                                 RELEASE TIME CODE, FROM                 
003000*** END OF VILMAII-COPY LENGTH= 24 BYTES                                  
