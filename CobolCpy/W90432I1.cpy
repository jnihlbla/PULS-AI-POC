000100 01  MID-W90432I1.                                                        
000200*                                 MID-COPYTEXT FÖR W9043200               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-FILLER           PIC X(9).                                    
000800     03 MID-EMB-Q.                                                        
000900*                                 RADER SOM VISAR EMBALLAGE ARTIK         
001000*                                 LAR                                     
001100        05 MID-FILLER        PIC X(9).                                    
001200        05 MID-FILLER        PIC X(5).                                    
001300        05 MID-FILLER        PIC X(3).                                    
001400        05 MID-FILLER        PIC X(9).                                    
001500        05 MID-FILLER        PIC X(5).                                    
001600        05 MID-FILLER        PIC X(3).                                    
001700        05 MID-FILLER        PIC X(9).                                    
001800        05 MID-FILLER        PIC X(5).                                    
001900        05 MID-KDEMBKOD-EMBQ2-IN                                          
002000                             PIC X(3).                                    
002100*                                 EMBALLAGEKOD 2                          
002200        05 MID-FILLER        PIC X(5).                                    
002300     03 MID-EMB-X            OCCURS 10 TIMES.                             
002400*                                 RADER SOM VISAR EXTRA EMBALLAGE         
002500*                                  ARTIKLAR                               
002600        05 MID-FILLER        PIC X(9).                                    
002700        05 MID-FILLER        PIC X(5).                                    
002800*** END OF VILMAII-COPY LENGTH= 223 BYTES                                 
