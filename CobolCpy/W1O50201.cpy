000100 01  MOD-W1O50201.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 BILDNUMMER                              
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS FELMEDDELANDE                       
000600     03 MOD-BERUBTXT-IN      PIC X(25).                                   
000700*                                 S÷KORD, DEL AV RUBRIKTEXT               
000800     03 MOD-BERUBTXT-UT      PIC X(25).                                   
000900*                                 S÷KORD, DEL AV RUBRIKTEXT               
001000     03 MOD-IDSKYLT-IN       PIC X(3).                                    
001100*                                 NATIONALITETSTECKEN IN                  
001200     03 MOD-IDSKYLT-UT       PIC X(3).                                    
001300*                                 NATIONALITETSTECKEN UT                  
001400     03 MOD-IDRUBNR-IN       PIC X(5).                                    
001500*                                 RUBRIKNUMMER                            
001600     03 MOD-IDRUBNR-UT       PIC X(5).                                    
001700*                                 RUBRIKNUMMER                            
001800     03 MOD-IDRUBNR-MIN      PIC 9(5).                                    
001900*                                 RUBRIKNUMMER                            
002000     03 MOD-IDRUBNR-MAX      PIC 9(5).                                    
002100*                                 RUBRIKNUMMER                            
002200     03 MOD-UTRAD            OCCURS 7 TIMES.                              
002300        05 MOD-IDRUBNR       PIC 9(5).                                    
002400*                                 RUBRIKNUMMER                            
002500        05 MOD-FILLER        PIC X(2).                                    
002600        05 MOD-FLKOMBINERAS  PIC X.                                       
002700*                                 FLAGGA, F≈R KOMBINERAS                  
002800        05 MOD-BERUBTXT-RAD  OCCURS 3 TIMES.                              
002900           07 MOD-BERUBTXT   PIC X(30).                                   
003000*                                 RUBRIKTEXT                              
003100        05 MOD-DATUM-RUBRIK  PIC X(5).                                    
003200        05 MOD-FILLER        PIC X.                                       
003300        05 MOD-TIUPPDAT      PIC 9(6).                                    
003400*                                 UPPDATERINGSDATUM  (≈≈MMDD)             
003500     03 MOD-TEMFSINF         PIC X(55).                                   
003600*                                 INFORMATIONSMEDDELANDE                  
003700*** END OF VILMAII-COPY LENGTH= 945 BYTES                                 
