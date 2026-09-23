000100 01  MOD-W6O16101.                                                        
000200*                                 UPPDATERING HANTERINGSKOD               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-MESSAGE-RAD1     PIC X(40).                                   
000600*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000700     03 MOD-INPUT-RAD        OCCURS 10 TIMES.                             
000800        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
000900*                                 MFS ATTRIBUTFÄLT                        
001000        05 MOD-IDARTNR       PIC X(2).                                    
001100*                                 MFS BEHANDLING AV INPUTFÄLT             
001200        05 MOD-KDARTHNT-V-ATTR                                            
001300                             PIC X(2).                                    
001400*                                 MFS ATTRIBUTFÄLT                        
001500        05 MOD-KDARTHNT-V    PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700        05 MOD-KDARTHNT-H-ATTR                                            
001800                             PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000        05 MOD-KDARTHNT-H    PIC X(2).                                    
002100*                                 MFS BEHANDLING AV INPUTFÄLT             
002200        05 MOD-KDARTURS-ATTR PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400        05 MOD-KDARTURS      PIC X(2).                                    
002500*                                 MFS BEHANDLING AV INPUTFÄLT             
002600     03 MOD-MESSAGE-RAD23    PIC X(61).                                   
002700*                                 MEDDELANDEFÄLT PÅ RAD 23                
002800*** END OF VILMAII-COPY LENGTH= 265 BYTES                                 
