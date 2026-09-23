000100 01  HTERM-MID-W6I15103-CTX.                                              
000200*                                 COPYTEXT FÖR MID                        
000300*                                 W6I10203                                
000400     03 HTERM-MID-IDLBFIKT-UT                                             
000500                             PIC X(3).                                    
000600*                                 FIKTIVT TRAILERNUMMER                   
000700     03 HTERM-MID-W6I15103-001-GRP                                        
000800                             OCCURS 14 TIMES.                             
000900*                                 LINES                                   
001000        05 HTERM-MID-IDLEVNR-KOLLI                                        
001100                             PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300        05 HTERM-MID-IDOKOLLI                                             
001400                             PIC X(9).                                    
001500*                                 ODETTE KOLLINUMMER                      
