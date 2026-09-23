000100 01  MID-W2I40501.                                                        
000200*                                 MID-COPYTEXT FÖR W2040500               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDDC-IN          PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-INPUT.                                                        
000800        05 MID-TISPSEA       PIC X(6).                                    
000900*                                 SÄSONG SPÄRRAD TOM    ÅÅMMDD            
001000        05 MID-SIMULERING    OCCURS 12 TIMES.                             
001100           07 MID-RESEASON-SIMIX                                          
001200                             PIC 9(3).                                    
001300*                                 SÄSONGSINDEX                            
001400           07 MID-KVOI-SIMANT                                             
001500                             PIC 9(7).                                    
001600*                                 ORDERINGÅNG I STYCK PER TIDSENH         
001700*** END OF VILMAII-COPY LENGTH= 137 BYTES                                 
