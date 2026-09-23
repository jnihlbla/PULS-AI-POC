000100 01  MOD-W90407O1.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 BILDNUMMER                              
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS FELMEDDELANDE                       
000600     03 MOD-FILLER           PIC X(9).                                    
000700     03 MOD-FILLER           PIC X(9).                                    
000800     03 MOD-FILLER           PIC X(25).                                   
000900     03 MOD-KDHOMONYM        PIC X.                                       
001000*                                 HOMONYMKOD                              
001100     03 MOD-BEART-NY-ATTR    PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300     03 MOD-FILLER           PIC X(25).                                   
001400     03 MOD-KDHOMONYM-ATTR   PIC X(2).                                    
001500*                                 MFS ATTRIBUTFÄLT                        
001600     03 MOD-FILLER           PIC X.                                       
001700     03 MOD-FILLER           PIC X(2).                                    
001800     03 MOD-FILLER           PIC X.                                       
001900     03 MOD-IDSKYLT-RAD      OCCURS 16 TIMES.                             
002000        05 MOD-FILLER        PIC X(2).                                    
002100        05 MOD-FILLER        PIC X(25).                                   
002200     03 MOD-HOMONYM-RAD      OCCURS 2 TIMES.                              
002300        05 MOD-TEHOMONYM     PIC X(60).                                   
002400*                                 HOMONYMTEXT                             
002500     03 MOD-TEMFSINF         PIC X(55).                                   
002600*                                 INFORMATIONSMEDDELANDE                  
002700*** END OF VILMAII-COPY LENGTH= 728 BYTES                                 
