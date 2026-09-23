000100 01  MOD-W90424O1.                                                        
000200*                                 MOD-COPYTEXT FÖR W9042400               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-FILLER           PIC X(9).                                    
000800     03 MOD-FILLER           PIC X(9).                                    
000900     03 MOD-FILLER           PIC X.                                       
001000     03 MOD-FILLER           PIC X(7).                                    
001100     03 MOD-FILLER           PIC X(25).                                   
001200     03 MOD-KVOI-PER         OCCURS 6 TIMES.                              
001300        05 MOD-AARTAL        PIC X(4).                                    
001400        05 MOD-FILLER        OCCURS 12 TIMES                              
001500                             PIC Z(6)9.                                   
001600        05 MOD-KVOI-TOT      PIC Z(6)9.                                   
001700*                                 ORDERINGÅNG I STYCK PER TIDSENH         
001800     03 MOD-TEMFSINF         PIC X(55).                                   
001900*                                 INFORMATIONSMEDDELANDE                  
002000*** END OF VILMAII-COPY LENGTH= 720 BYTES                                 
