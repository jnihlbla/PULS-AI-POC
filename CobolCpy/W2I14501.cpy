000100 01  MID-W2I14501.                                                        
000200*                                 COPYTEXT F÷R MID W2I14501               
000300     03 MID-IDLANDX2-IN      PIC X(2).                                    
000400*                                 2-STƒLLIG LANDSBETECKNINGSKOD           
000500     03 MID-IDLANDX2-UT      PIC X(2).                                    
000600*                                 2-STƒLLIG LANDSBETECKNINGSKOD           
000700     03 MID-INPUT.                                                        
000800        05 MID-RAD           OCCURS 20 TIMES.                             
000900           07 MID-CMD        PIC X.                                       
001000           07 MID-DADATUM-HELG                                            
001100                             PIC 9(8).                                    
001200*                                 HELGDAGAR (≈≈≈≈MMDD)                    
001300        05 MID-DADATUM-HELG-NY                                            
001400                             PIC 9(8).                                    
001500*                                 HELGDAGAR (≈≈≈≈MMDD)                    
001600*** END OF VILMAII-COPY LENGTH= 192 BYTES                                 
