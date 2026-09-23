000100 01  MID-W2I13701.                                                        
000200*                                                                         
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-INPUT.                                                        
000800*                                 SÄSONGSINDEX FÖRÄNDRING                 
000900        05 MID-KVPB-PER-SIM-GRP.                                          
001000*                                                                         
001100           07 MID-KVPB-PER-SIM                                            
001200                             OCCURS 12 TIMES                              
001300                             PIC 9(6).                                    
001400        05 MID-RESEASON-SIM-GRP.                                          
001500*                                                                         
001600           07 MID-RESEASON-SIM                                            
001700                             OCCURS 12 TIMES                              
001800                             PIC X(4).                                    
001900        05 MID-TISEASON-SIM  PIC 9(6).                                    
002000*** END OF VILMAII-COPY LENGTH= 144 BYTES                                 
