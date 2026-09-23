000100 01  MID-W2I12801.                                                        
000200*                                 MID-COPYTEXT F÷R W2012800               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-LINE-INFO        OCCURS 14 TIMES.                             
000800        05 MID-TIAAVV        PIC X(4).                                    
000900*                                 ≈R - VECKA  (≈≈VV)                      
001000     03 MID-CURSOR.                                                       
001100*                                 CURSOR PLACERING                        
001200        05 MID-CURSOR-RAD    PIC S9(4)           COMP.                    
001300*                                 CURSORPLACERING RAD                     
001400        05 MID-CURSOR-KOL    PIC S9(4)           COMP.                    
001500*                                 CURSORPLACERING KOLUMN                  
001600*** END OF VILMAII-COPY LENGTH= 78 BYTES                                  
