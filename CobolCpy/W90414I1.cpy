000100 01  MID-W90414I1.                                                        
000200*                                 MID-COPYTEXT F÷R W90414                 
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-FILLER           PIC X(3).                                    
000800     03 MID-FILLER           PIC X(3).                                    
000900     03 MID-VECKA-FOM-IN     PIC X(4).                                    
001000*                                 ≈R - VECKA  (≈≈VV)                      
001100     03 MID-VECKA-FOM-UT     PIC X(4).                                    
001200*                                 ≈R - VECKA  (≈≈VV)                      
001300     03 MID-VECKA-TOM-IN     PIC X(4).                                    
001400*                                 ≈R - VECKA  (≈≈VV)                      
001500     03 MID-VECKA-TOM-UT     PIC X(4).                                    
001600*                                 ≈R - VECKA  (≈≈VV)                      
001700     03 MID-IDRADNR-DOLT     PIC X(4).                                    
001800*                                 RADNUMMER                               
001900     03 MID-IDRADNR-DOLT2    PIC X(4).                                    
002000*                                 RADNUMMER                               
002100     03 MID-FILLER           PIC X(9).                                    
002200     03 MID-INPUT            OCCURS 13 TIMES.                             
002300*                                                                         
002400        05 MID-FILLER        PIC X.                                       
002500        05 MID-FILLER        PIC X(5).                                    
002600        05 MID-FILLER        PIC X(30).                                   
002700        05 MID-FILLER        PIC X.                                       
002800     03 MID-FILLER           PIC X.                                       
002900*** END OF VILMAII-COPY LENGTH= 539 BYTES                                 
