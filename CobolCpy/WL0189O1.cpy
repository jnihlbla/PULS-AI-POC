000100 01  RESP-WL0189O1.                                                       
000200*                                 RESPONS FROM PGM WL0189                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDDISTR-KEY     PIC Z(3)9.                                   
000600*                                 DISTRIKTNUMMER                          
000700     03 RESP-KVRADER         PIC Z(4)9.                                   
000800*                                 ANTAL RADER                             
000900     03 RESP-RADER           OCCURS 500 TIMES.                            
001000*                                 RADINFORMATION                          
001100        05 RESP-IDDISTR      PIC Z(3)9.                                   
001200*                                 DISTRIKTNUMMER                          
001300        05 RESP-IDKUNDNR     PIC Z(5)9.                                   
001400*                                 KUNDNUMMER                              
001500        05 RESP-IDORDNR      PIC Z(4)9.                                   
001600*                                 ORDERNUMMER UTG≈R PD90                  
001700        05 RESP-IDPRODNR     PIC Z(6)9.                                   
001800*                                 PRODUKTIONSNUMMER                       
001900        05 RESP-TIRFS        PIC 9(10).                                   
002000*                                 KLART F÷R TRANSPORT ≈≈MMDDTTMM          
002100        05 RESP-IDTRPTNR     PIC Z(2)9.                                   
002200*                                 TRANSPORTIDENTITET                      
002300*** END OF VILMAII-COPY LENGTH= 17511 BYTES                               
