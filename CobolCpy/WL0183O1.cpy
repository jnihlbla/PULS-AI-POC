000100 01  RESP-WL0183O1.                                                       
000200*                                 RESPONS FROM PGM WL0183                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 RESP-IDSHIPM-KEY     PIC Z(6)9.                                   
000700*                                 SKEPPNINGSNUMMER                        
000800*                                 SHIPMENT NO                             
000900     03 RESP-KVRADER         PIC Z(4)9.                                   
001000*                                 ANTAL RADER                             
001100*                                 NUMBER OF LINES                         
001200     03 RESP-RADER           OCCURS 500 TIMES.                            
001300*                                                                         
001400        05 RESP-IDDISTR      PIC Z(3)9.                                   
001500*                                 DISTRIKTNUMMER                          
001600*                                 DISTRICT NUMBER                         
001700        05 RESP-IDKUNDNR     PIC Z(5)9.                                   
001800*                                 KUNDNUMMER                              
001900*                                 CUSTOMER NO                             
002000        05 RESP-IDORDNR5     PIC Z(4)9.                                   
002100*                                 ORDERNUMMER                             
002200*                                 ORDER NUMBER                            
002300        05 RESP-IDKOLLI      PIC Z(4)9.                                   
002400*                                 KOLLINUMMER                             
002500*                                 CASE NUMBER                             
002600*** END OF VILMAII-COPY LENGTH= 10014 BYTES                               
