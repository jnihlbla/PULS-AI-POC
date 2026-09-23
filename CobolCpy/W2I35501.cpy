000100 01  MID-W2I35501.                                                        
000200*                                 MID-COPYTEXT FÖR W2035500               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-INPUT.                                                        
001200        05 MID-DASPSEA       PIC X(6).                                    
001300        05 MID-SIMULERING    OCCURS 12 TIMES.                             
001400           07 MID-SIMIX      PIC 9(3).                                    
001500           07 MID-SIMANT     PIC 9(7).                                    
001600*                                 ORDERINGÅNG I STYCK PER TIDSENH         
001700*** END OF VILMAII-COPY LENGTH= 148 BYTES                                 
