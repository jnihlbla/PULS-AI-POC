000100 01  MID-W2I10501.                                                        
000200*                                 MID-COPYTEXT FÖR W2010500               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-INPUT.                                                        
000800        05 MID-DASPSEA       PIC X(6).                                    
000900        05 MID-RENSA-IX      PIC X.                                       
001000        05 MID-SIMULERING    OCCURS 12 TIMES.                             
001100           07 MID-SIMIX      PIC 9(3).                                    
001200           07 MID-SIMANT     PIC 9(7).                                    
001300*                                 ORDERINGÅNG I STYCK PER TIDSENH         
001400*** END OF VILMAII-COPY LENGTH= 145 BYTES                                 
