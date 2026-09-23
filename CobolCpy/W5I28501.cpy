000100 01  MID-W5I28501.                                                        
000200*                                 MID-COPY TEXT FÖR W5028500              
000300     03 MID-TIREGDAT-IN      PIC X(6).                                    
000400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000500     03 MID-TIREGTID-IN      PIC X(6).                                    
000600*                                 REGISTRERINGSTID                        
000700     03 MID-IDUSER-IN        PIC X(8).                                    
000800*                                 ANVÄNDARENS SÄKERHETS ID                
000900     03 MID-TIREGDAT-UT      PIC X(6).                                    
001000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001100     03 MID-TIREGTID-UT      PIC X(6).                                    
001200*                                 REGISTRERINGSTID                        
001300     03 MID-IDUSER-UT        PIC X(8).                                    
001400*                                 ANVÄNDARENS SÄKERHETS ID                
001500     03 MID-INPUT            OCCURS 13 TIMES.                             
001600        05 MID-IDARTNR       PIC X(8).                                    
001700*                                 ARTIKELNUMMER                           
001800        05 MID-KVLS-IN       PIC X(7).                                    
001900*                                 LAGERSALDO                              
002000*** END OF VILMAII-COPY LENGTH= 235 BYTES                                 
