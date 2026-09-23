000100 01  MOD-W90431O1.                                                        
000200*                                 UPPDATERING HANTERINGSKOD               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-MESSAGE-RAD1     PIC X(40).                                   
000600*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000700     03 MOD-INPUT-RAD        OCCURS 10 TIMES.                             
000800        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
000900*                                 MFS ATTRIBUTFÄLT                        
001000        05 MOD-FILLER        PIC X(2).                                    
001100        05 MOD-FILLER        PIC X(2).                                    
001200        05 MOD-FILLER        PIC X(2).                                    
001300        05 MOD-KDARTHNT-H-ATTR                                            
001400                             PIC X(2).                                    
001500*                                 MFS ATTRIBUTFÄLT                        
001600        05 MOD-FILLER        PIC X(2).                                    
001700        05 MOD-FILLER        PIC X(2).                                    
001800        05 MOD-FILLER        PIC X(2).                                    
001900     03 MOD-MESSAGE-RAD23    PIC X(61).                                   
002000*                                 MEDDELANDEFÄLT PÅ RAD 23                
002100*** END OF VILMAII-COPY LENGTH= 265 BYTES                                 
