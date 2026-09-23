000100 01  NOT-WDN524.                                                          
000200*                                 KATALOG AVSNITTSREGISTER                
000300*                                 NOTERINGS-SEGMENT                       
000400*                                 GRUPP, AVSNITT, RAD LAGRAS HÄR          
000500*                                 DÅ DET MAN HÄNVISAR TILL ÄNNU           
000600*                                 EJ REGISTRERATS                         
000700*                                 SÖKBEGREPP: IDSEGMNR                    
000800     03 NOT-IDSEGMNR         PIC S9              COMP-3.                  
000900*                                 ORDNINGSFÖLJD PÅ SEGMENTET              
001000*                                 SEQUENCE ORDER ID, ON SEGMENT           
001100     03 NOT-TENOTE           PIC X(40).                                   
001200*                                 NOTERINGSFÄLT                           
001300*                                 NOTE FIELD                              
001400     03 NOT-HAEN-AREA REDEFINES NOT-TENOTE.                               
001500        05 NOT-IDCATGRP      PIC 9(2).                                    
001600*                                 KATALOG-GRUPP                           
001700*                                 CATALOG-GROUP                           
001800        05 NOT-IDCATAVS      PIC 9(4).                                    
001900*                                 KATALOG-AVSNITT                         
002000*                                 CATALOG TEXT BLOCK                      
002100        05 NOT-IDCATRAD      PIC 9(4).                                    
002200*                                 RADNUMMER                               
002300*                                 ROW NUMBER IN TEXT BLOCK                
002400        05 NOT-KDCATPUB-FOM  PIC X(6).                                    
002500*                                 PUBLICERINGS TIDKOD, KATALOGRAD         
002600*                                 RELEASE TIME CODE, CATALOG LINE         
002700        05 NOT-KDCATPUB-TOM  PIC X(6).                                    
002800*                                 PUBLICERINGS TIDKOD, KATALOGRAD         
002900*                                 RELEASE TIME CODE, CATALOG LINE         
003000        05 NOT-FILLER        PIC X(18).                                   
003100*** END OF VILMAII-COPY LENGTH= 41 BYTES                                  
