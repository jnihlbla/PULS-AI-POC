000100 01  AVS-WDN501.                                                          
000200*                                 KATALOG AVSNITTSREGISTER                
000300*                                 ROTSEGMENT AVSNITT                      
000400*                                 FYSISK NYCKEL: WDN501KY                 
000500*                                 (IDCATNR + IDCATGRP                     
000600*                                  IDCATAVS)                              
000700     03 AVS-IDCATAKY.                                                     
000800*                                 NYCKEL TILL KATALOGAVSNITT              
000900*                                 KEY TO A CATALOGUE TEXT BLOCK           
001000        05 AVS-IDCATNR       PIC 9(5).                                    
001100*                                 KATALOG-ID                              
001200*                                 CATALOG-ID                              
001300        05 AVS-IDCATGRP      PIC 9(2).                                    
001400*                                 KATALOG-GRUPP                           
001500*                                 CATALOG-GROUP                           
001600        05 AVS-IDCATAVS      PIC 9(4).                                    
001700*                                 KATALOG-AVSNITT                         
001800*                                 CATALOG TEXT BLOCK                      
001900     03 AVS-IDVERS           PIC S9(3)           COMP-3.                  
002000*                                 UTGÅVA                                  
002100*                                 CATALOG VERSION ID                      
002200     03 AVS-FLAVSUST         PIC X.                                       
002300*                                 AVSNITTET ÄNDRAT?                       
002400*                                 SECTION CHANGED?                        
002500     03 AVS-FLAVSTVAD        PIC X.                                       
002600*                                 AVSNITTET SKICKAS TILL VADIS?           
002700*                                 SECTION SENT TO VADIS?                  
002800*** END OF VILMAII-COPY LENGTH= 15 BYTES                                  
