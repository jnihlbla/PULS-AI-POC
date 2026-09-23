000100 01  RESP-WL0136O1.                                                       
000200*                                 RESPONS FROM PGM WL0136                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDFKNGRP-KEY    PIC Z(3)9.                                   
000600*                                 FUNKTIONSGRUPP                          
000700     03 RESP-KDLEVSP-KEY     PIC Z9.                                      
000800*                                 SPÄRRKOD LEVERANS                       
000900     03 RESP-FL-KVSPARR-KVAL-KEY                                          
001000                             PIC X.                                       
001100*                                 ALLMÄN FLAGGA                           
001200     03 RESP-IDUSER-SPKVAL-KEY                                            
001300                             PIC X(8).                                    
001400*                                 ANVÄNDAR-ID KVALITETSPÄRR               
001500     03 RESP-KDSORT1         PIC 9.                                       
001600*                                 SORTERINGSKOD                           
001700     03 RESP-FLSKRIV         PIC X.                                       
001800*                                 JA = ÅTERSTART AV BEGÄRD LISTA          
001900     03 RESP-KVRADER         PIC Z(4)9.                                   
002000*                                 ANTAL RADER                             
002100     03 RESP-RAD             OCCURS 500 TIMES.                            
002200        05 RESP-IDARTNR      PIC Z(7)9.                                   
002300*                                 ARTIKELNUMMER                           
002400        05 RESP-IDDC         PIC X(2).                                    
002500*                                 IDENTIFIERARE LAGER                     
002600        05 RESP-IDFKNGRP     PIC Z(3)9.                                   
002700*                                 FUNKTIONSGRUPP                          
002800        05 RESP-BEART        PIC X(25).                                   
002900*                                 ARTIKELBENÄMNING                        
003000        05 RESP-KDLEVSP      PIC Z9.                                      
003100*                                 SPÄRRKOD LEVERANS                       
003200        05 RESP-KVSPARR-KVAL PIC Z(6)9.                                   
003300*                                 SPÄRRAT ANTAL KVALITETSFEL              
003400        05 RESP-KVLS         PIC -(7)9.                                   
003500*                                 LAGERSALDO                              
003600        05 RESP-KVROS        PIC -(7)9.                                   
003700*                                 RESTORDERSALDO                          
003800        05 RESP-TISPARR-KVAL PIC 9(6).                                    
003900*                                 SPÄRRAD DATUM KVALITETSFEL              
004000        05 RESP-IDUSER-SPKVAL                                             
004100                             PIC X(8).                                    
004200*                                 ANVÄNDAR-ID KVALITETSPÄRR               
004300*** END OF VILMAII-COPY LENGTH= 39024 BYTES                               
