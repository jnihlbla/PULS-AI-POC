000100 01  MOD-W1O50801.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 BILDNUMMER                              
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS FELMEDDELANDE                       
000600     03 MOD-BEFOTNOT-SOEK-IN PIC X(25).                                   
000700*                                 SÖKORD, DEL AV FOTNOTTEXT               
000800     03 MOD-BEFOTNOT-SOEK-UT PIC X(25).                                   
000900*                                 SÖKORD, DEL AV FOTNOTTEXT               
001000     03 MOD-IDSKYLT-IN       PIC X(3).                                    
001100*                                 NATIONALITETSTECKEN IN                  
001200     03 MOD-IDSKYLT-UT       PIC X(3).                                    
001300*                                 NATIONALITETSTECKEN UT                  
001400     03 MOD-IDFOTNR-IN       PIC X(5).                                    
001500*                                 FOTNOTSNUMMER                           
001600     03 MOD-IDFOTNR-UT       PIC X(5).                                    
001700*                                 FOTNOTSNUMMER                           
001800     03 MOD-IDFOTNR-SPAR     PIC 9(5).                                    
001900*                                 FOTNOTSNUMMER                           
002000     03 MOD-UTRAD            OCCURS 14 TIMES.                             
002100        05 MOD-IDFOTNR       PIC 9(5).                                    
002200*                                 FOTNOTSNUMMER                           
002300        05 MOD-BEFOTNOT      PIC X(55).                                   
002400*                                 FOTNOTSTEXT                             
002500     03 MOD-TEMFSINF         PIC X(55).                                   
002600*                                 INFORMATIONSMEDDELANDE                  
002700*** END OF VILMAII-COPY LENGTH= 1010 BYTES                                
