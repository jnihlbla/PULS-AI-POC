000100 01  RESP-WL0116O1.                                                       
000200*                                 RESPONS FROM PGM WL0116                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDDISTR-KEY     PIC Z(3)9.                                   
000600*                                 DISTRIKTNUMMER                          
000700     03 RESP-IDKUNDNR-KEY    PIC Z(5)9.                                   
000800*                                 KUNDNUMMER                              
000900     03 RESP-IDORDNR-KEY     PIC Z(4)9.                                   
001000*                                 ORDERNUMMER UTGÅR PD90                  
001100     03 RESP-IDPRODNR-KEY    PIC Z(6)9.                                   
001200*                                 PRODUKTIONSNUMMER                       
001300     03 RESP-KVRADER         PIC Z(4)9.                                   
001400*                                 ANTAL RADER                             
001500     03 RESP-RAD             OCCURS 500 TIMES.                            
001600*                                                                         
001700        05 RESP-ADPACOMR     PIC X(2).                                    
001800*                                 PACKNINGSOMRÅDE                         
001900        05 RESP-IDRADNR-ORD-FROM                                          
002000                             PIC Z(3)9.                                   
002100*                                 RADNUMMER PÅ VOLVOORDER FROM            
002200        05 RESP-IDRADNR-ORD-TOM                                           
002300                             PIC Z(3)9.                                   
002400*                                 RADNUMMER PÅ VOLVOORDER TOM             
002500        05 RESP-KVORAPP      PIC Z(5)9.                                   
002600*                                 EJ-RAPPORTERAT-ANTAL                    
002700        05 RESP-IDANSTNR     PIC Z(4)9.                                   
002800*                                 ANSTÄLLNINGSNUMMER                      
002900*** END OF VILMAII-COPY LENGTH= 10529 BYTES                               
