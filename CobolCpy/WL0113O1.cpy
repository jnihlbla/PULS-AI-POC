000100 01  RESP-WL0113O1.                                                       
000200*                                 RESPONS FROM PGM WL0113                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDARTNR-KEY     PIC Z(7)9.                                   
000600*                                 ARTIKELNUMMER                           
000700     03 RESP-IDDC2-KEY       PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 RESP-SHOW-KEY        PIC X.                                       
001000*                                 ALLMÄN FLAGGA                           
001100     03 RESP-BELANG-KEY      PIC X(20).                                   
001200*                                 SPRÅK I KLARTEXT                        
001300     03 RESP-BEART           PIC X(25).                                   
001400*                                 ARTIKELBENÄMNING                        
001500     03 RESP-KVRADER         PIC Z(4)9.                                   
001600*                                 ANTAL RADER                             
001700     03 RESP-RAD             OCCURS 500 TIMES.                            
001800        05 RESP-IDDC         PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000        05 RESP-KDLEVSP      PIC Z9.                                      
002100*                                 SPÄRRKOD LEVERANS                       
002200        05 RESP-KDLEVSP-UPD  PIC 9(2).                                    
002300*                                 SPÄRRKOD LEVERANS                       
002400        05 RESP-KVSPARR-KVAL PIC Z(6)9.                                   
002500*                                 SPÄRRAT ANTAL KVALITETSFEL              
002600        05 RESP-KVSPARR-KVAL-UPD                                          
002700                             PIC 9(7).                                    
002800*                                 SPÄRRAT ANTAL KVALITETSFEL              
002900        05 RESP-TEKVAL       PIC X(10).                                   
003000*                                 KVALITETSNOTERING SPÄRR                 
003100        05 RESP-TISPARR-KVAL PIC 9(6).                                    
003200*                                 SPÄRRAD DATUM KVALITETSFEL              
003300        05 RESP-IDUSER-SPKVAL                                             
003400                             PIC X(8).                                    
003500*                                 ANVÄNDAR-ID KVALITETSPÄRR               
003600        05 RESP-KVLS         PIC -(7)9.                                   
003700*                                 LAGERSALDO                              
003800*** END OF VILMAII-COPY LENGTH= 26063 BYTES                               
