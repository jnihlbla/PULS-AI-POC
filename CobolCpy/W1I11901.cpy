000100 01  MID-W1O11901.                                                        
000200*                                 MID-COPYTEXT FÖR W1011900               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-ROW-IN.                                                       
000800        05 MID-RADER.                                                     
000900           07 MID-UPDAT      OCCURS 10 TIMES                              
001000                             PIC X.                                       
001100           07 MID-PRODB      OCCURS 10 TIMES                              
001200                             PIC X(2).                                    
001300*                                 PRODUKTSLAG                             
001400*** END OF VILMAII-COPY LENGTH= 48 BYTES                                  
