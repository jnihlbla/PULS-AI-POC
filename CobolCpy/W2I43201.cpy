000100 01  MID-W2I43201.                                                        
000200*                                 COPYTEXT FÖR MID W2I43201               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-KDTECKEN-TREND-IN                                             
001200                             PIC X.                                       
001300*                                 PLUS ELLER MINUS (+ -)                  
001400     03 MID-INPUT.                                                        
001500*                                                                         
001600        05 MID-KVPB-TREND-IN PIC X(8).                                    
001700*                                 PERIODTRENDVÄRDE                        
001800        05 MID-KVVECKOR-TREND-IN                                          
001900                             PIC 9(2).                                    
002000*                                 ANTAL VECKOR TRENDVÄRDE                 
002100        05 MID-KVPB-JUST1-IN PIC X(8).                                    
002200*                                 PERIODBEHOVSJUSTERING-1                 
002300        05 MID-TIPBJUST-1-IN PIC 9(4).                                    
002400*                                 DATUM FÖRSTA PB-JUSTERING ÅÅVV          
002500        05 MID-KVPB-JUST2-IN PIC X(8).                                    
002600*                                 PERIODBEHOVSJUSTERING-2                 
002700        05 MID-TIPBJUST-2-IN PIC 9(4).                                    
002800*                                 DATUM ANDRA PB-JUSTERING ÅÅVV           
002900*** END OF VILMAII-COPY LENGTH= 57 BYTES                                  
