000100 01  MID-W2I12301.                                                        
000200*                                 COPYTEXT FÖR MID W2I12301               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-PBJUST-1         OCCURS 2 TIMES.                              
000800*                                                                         
000900        05 MID-KVPB-JUST-1   PIC X(8).                                    
001000*                                 PERIODBEHOVSJUSTERING                   
001100        05 MID-TIPBJUST-1    PIC X(4).                                    
001200*                                 DATUM FÖR PB-JUSTERING (ÅÅVV)           
001300     03 MID-PBJUST-2         OCCURS 2 TIMES.                              
001400*                                                                         
001500        05 MID-KVPB-JUST-2   PIC X(8).                                    
001600*                                 PERIODBEHOVSJUSTERING                   
001700        05 MID-TIPBJUST-2    PIC X(4).                                    
001800*                                 DATUM FÖR PB-JUSTERING (ÅÅVV)           
001900     03 MID-KVMAD-TOT        PIC X(8).                                    
002000*                                 TOTALT PROGNOSFEL                       
002100     03 MID-KDTECKEN-TREND   PIC X.                                       
002200*                                 PLUS ELLER MINUS (+ -)                  
002300     03 MID-KVPB-TREND       PIC X(8).                                    
002400*                                 PERIODTRENDVÄRDE                        
002500     03 MID-KVVECKOR-TREND   PIC X(2).                                    
002600*                                 ANTAL VECKOR TRENDVÄRDE                 
002700*** END OF VILMAII-COPY LENGTH= 85 BYTES                                  
