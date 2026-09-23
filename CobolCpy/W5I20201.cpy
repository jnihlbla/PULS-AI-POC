000100 01  MID-W5I20201.                                                        
000200*                                 MID-COPY TEXT FÖR W5020200              
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-INPUT.                                                        
000600        05 MID-RADER         OCCURS 12 TIMES.                             
000700           07 MID-KDBEH      PIC X.                                       
000800*                                 BEHANDLINGSKOD                          
000900           07 MID-KDPSLLOC-IN                                             
001000                             PIC 9(2).                                    
001100*                                 PRODUKTSLAG LOKALT                      
001200     03 MID-UPD.                                                          
001300        05 MID-IDARTNR-UPD   PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500        05 MID-KDPSLLOC-UPD  PIC 9(2).                                    
001600*                                 PRODUKTSLAG LOKALT                      
001700*** END OF VILMAII-COPY LENGTH= 56 BYTES                                  
