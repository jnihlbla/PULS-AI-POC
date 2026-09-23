000100 01  MOD-W1O50101.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 BILDNUMMER                              
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS FELMEDDELANDE                       
000600     03 MOD-BERUBTXT-SOEK-IN PIC X(25).                                   
000700*                                 S÷KORD, DEL AV RUBRIKTEXT               
000800     03 MOD-BERUBTXT-SOEK-UT PIC X(25).                                   
000900*                                 S÷KORD, DEL AV RUBRIKTEXT               
001000     03 MOD-IDSKYLT-IN       PIC X(3).                                    
001100*                                 NATIONALITETSTECKEN IN                  
001200     03 MOD-IDSKYLT-UT       PIC X(3).                                    
001300*                                 NATIONALITETSTECKEN UT                  
001400     03 MOD-IDRUBNR-IN       PIC X(5).                                    
001500*                                 RUBRIKNUMMER                            
001600     03 MOD-IDRUBNR-UT       PIC X(5).                                    
001700*                                 RUBRIKNUMMER                            
001800     03 MOD-IDRUBNR-SPAR     PIC 9(5).                                    
001900*                                 RUBRIKNUMMER                            
002000     03 MOD-UTRAD            OCCURS 7 TIMES.                              
002100        05 MOD-IDRUBNR       PIC 9(5).                                    
002200*                                 RUBRIKNUMMER                            
002300        05 MOD-FLKOMBINERAS  PIC X.                                       
002400*                                 FLAGGA, F≈R KOMBINERAS                  
002500        05 MOD-BERUBTXT-RAD  OCCURS 3 TIMES.                              
002600           07 MOD-BERUBTXT   PIC X(30).                                   
002700*                                 RUBRIKTEXT                              
002800        05 MOD-DATUM-RUBRIK  PIC X(5).                                    
002900        05 MOD-FILLER        PIC X.                                       
003000        05 MOD-TIUPPDAT      PIC 9(6).                                    
003100*                                 UPPDATERINGSDATUM  (≈≈MMDD)             
003200     03 MOD-TEMFSINF         PIC X(55).                                   
003300*                                 INFORMATIONSMEDDELANDE                  
003400*** END OF VILMAII-COPY LENGTH= 926 BYTES                                 
