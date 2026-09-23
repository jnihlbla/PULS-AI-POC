000100 01  REQU-WL0136I1.                                                       
000200*                                 REQUEST TO PGM WL0136                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDFKNGRP-KEY    PIC 9(4).                                    
000600*                                 FUNKTIONSGRUPP                          
000700     03 REQU-KDLEVSP-KEY     PIC 9(2).                                    
000800*                                 SPÄRRKOD LEVERANS                       
000900     03 REQU-FL-KVSPARR-KVAL-KEY                                          
001000                             PIC X.                                       
001100*                                 ALLMÄN FLAGGA                           
001200     03 REQU-IDUSER-SPKVAL-KEY                                            
001300                             PIC X(8).                                    
001400*                                 ANVÄNDAR-ID KVALITETSPÄRR               
001500     03 REQU-KDSORT1         PIC 9.                                       
001600*                                 SORTERINGSKOD                           
001700     03 REQU-FLSKRIV         PIC X.                                       
001800*                                 JA = ÅTERSTART AV BEGÄRD LISTA          
001900*** END OF VILMAII-COPY LENGTH= 19 BYTES                                  
