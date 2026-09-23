000100 01  AVSC-WDN5C1.                                                         
000200*                                 KATALOG AVSNITTSREGISTER                
000300*                                 SECONDARY INDEX PÅ                      
000400*                                           ILLUSTRATION                  
000500*                                 FYSISK NYCKEL:  WDN5C1KY                
000600*                                 (IDILLU + KDKATPUB                      
000700*                                  + IDCATNR + IDCATGRP                   
000800*                                  + IDCATAVS)                            
000900*                                 SECONDARY NYCKEL: WDN5CSEQ              
001000*                                 (IDILLU, KDCATPUB)                      
001100     03 AVSC-IDILLU          PIC S9(5)           COMP-3.                  
001200*                                 ILLUSTRATIONENS NR                      
001300*                                 ILLUSTRATION NUMBER                     
001400     03 AVSC-KDCATPUB        PIC X(6).                                    
001500*                                 PUBLICERINGS TIDKOD, KATALOGRAD         
001600*                                 RELEASE TIME CODE, CATALOG LINE         
001700     03 AVSC-IDCATAKY.                                                    
001800*                                 NYCKEL TILL KATALOGAVSNITT              
001900*                                 KEY TO A CATALOGUE TEXT BLOCK           
002000        05 AVSC-IDCATNR      PIC 9(5).                                    
002100*                                 KATALOG-ID                              
002200*                                 CATALOG-ID                              
002300        05 AVSC-IDCATGRP     PIC 9(2).                                    
002400*                                 KATALOG-GRUPP                           
002500*                                 CATALOG-GROUP                           
002600        05 AVSC-IDCATAVS     PIC 9(4).                                    
002700*                                 KATALOG-AVSNITT                         
002800*                                 CATALOG TEXT BLOCK                      
002900*** END OF VILMAII-COPY LENGTH= 20 BYTES                                  
