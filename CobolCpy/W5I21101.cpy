000100 01  MID-W5I21101.                                                        
000200*                                 MID-COPY TEXT F÷R W5021100              
000300     03 MID-FROM-DAREGDAT-IN PIC 9(8).                                    
000400*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
000500     03 MID-TOM-DAREGDAT-IN  PIC 9(8).                                    
000600*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
000700     03 MID-IDPGM-IN         PIC X(8).                                    
000800*                                 PROGRAM IDENTITET                       
000900     03 MID-TABELLRAD        OCCURS 13 TIMES.                             
001000*                                 GRUPP MED TABELL RADER                  
001100        05 MID-CMD           PIC X.                                       
001200*** END OF VILMAII-COPY LENGTH= 37 BYTES                                  
